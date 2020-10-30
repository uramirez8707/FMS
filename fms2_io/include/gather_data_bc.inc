!* This file is part of the GFDL Flexible Modeling System (FMS).
!*
!* FMS is free software: you can redistribute it and/or modify it under
!* the terms of the GNU Lesser General Public License as published by
!* the Free Software Foundation, either version 3 of the License, or (at
!* your option) any later version.
!*
!* FMS is distributed in the hope that it will be useful, but WITHOUT
!* ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
!* FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
!* for more details.
!*
!* You should have received a copy of the GNU Lesser General Public
!* License along with FMS.  If not, see <http://www.gnu.org/licenses/>.
!***********************************************************************

subroutine gather_data_bc_2d(fileobj, vdata, bc_info)
  class(FmsNetcdfFile_t), intent(inout) :: fileobj
  class(*), dimension(:,:), intent(in) :: vdata
  type(bc_information), intent(inout) :: bc_info

  integer :: i_glob, j_glob
  integer :: isc, iec, jsc, jec, i1, i2, j1, j2, i_add, j_add

  real(kind=real32), dimension(:,:), allocatable, target :: global_buf_real32, local_buf_real32
  real(kind=real64), dimension(:,:), allocatable, target :: global_buf_real64, local_buf_real64
  integer(kind=int64) :: chksum_val
  character(len=32) :: chksum

  !> Set the indices
  isc = bc_info%indices(1)
  iec = bc_info%indices(2)
  jsc = bc_info%indices(3)
  jec = bc_info%indices(4)

  !> This is the section of the PE that will actually be added to the global_buffer
  i1 = 1 + bc_info%x_halo
  i2 = i1 + (iec-isc)
  j1 = 1 + bc_info%y_halo
  j2 = j1 + (jec-jsc)

  !> Set up index shifts for global array
  i_add = bc_info%ishift
  j_add = bc_info%jshift

  !> Allocate a global_buffer that will be written
  if (fileobj%is_root) then
        i_glob = bc_info%global_size(1)
        j_glob = bc_info%global_size(2)
  endif

  !> Gather the data and calculate the checksum for the resulting array.
  select type(vdata)
    type is (real(kind=real32))
       !> If the fileobj's root is not the same as the variable's root
       if ( .not. bc_info%data_on_file_root) then
           !> Allocate global_buf_real32 to be one size bigger, global_buf_real32(i_glob+1,,:) is just dummy data
          allocate(global_buf_real32(i_glob+1, j_glob))
          !> Allocate a temp local buffer to the fileobj's root. This is needed because the data needs to be send
          !! to the fileobj's root, but because fileobj's root doesn't have any data, we just create dummy data and
          !! not write it later
          allocate(local_buf_real32(1,1))
          local_buf_real32 = 0.
          isc = 1+i_glob; i_add=0; iec=1+i_glob; jsc=j_glob; j_add=0; jec=j_glob
          i1=1; i2=1; j1=1; j2=1
       else
          !! In this case there is data in fileobj's root, so there is no need for the dummy data
          if(fileobj%is_root) allocate(global_buf_real32(i_glob, j_glob))
          allocate(local_buf_real32(size(vdata,1), size(vdata,2)))
          local_buf_real32 = vdata
       endif

       call mpp_gather(isc+i_add, iec+i_add, jsc+j_add, jec+j_add, bc_info%pelist, &
                          local_buf_real32(i1:i2,j1:j2), &
                          global_buf_real32, fileobj%is_root)

        deallocate(local_buf_real32)
       !> If you are on fileobj's root, calculate the checksum and save the gathered data in a buffer
       if (fileobj%is_root) then
#ifdef OVERLOAD_R4
           chksum_val = mpp_chksum(global_buf_real32(1:i_glob,1:j_glob), (/mpp_pe()/))
#endif
           allocate(bc_info%globaldata2d_32(i_glob, j_glob))
           bc_info%globaldata2d_32=global_buf_real32(1:i_glob,1:j_glob)
           deallocate(global_buf_real32)
       endif
    type is (real(kind=real64))
       !> If the fileobj's root is not the same as the variable's root
       if ( .not. bc_info%data_on_file_root) then
          !> Allocate global_buf_real64 to be one size bigger, global_buf_real64(i_glob+1,,:) is just dummy data
          allocate(global_buf_real64(i_glob+1, j_glob))
          !> Allocate a temp local buffer to the fileobj's root. This is needed because the data needs to be send
          !! to the fileobj's root, but because fileobj's root doesn't have any data, we just create dummy data and
          !! not write it later
          allocate(local_buf_real64(1,1))
          local_buf_real64 = 0.
          isc = 1+i_glob; i_add=0; iec=1+i_glob; jsc=j_glob; j_add=0; jec=j_glob
          i1=1; i2=1; j1=1; j2=1
       else
          !! In this case there is data in fileobj's root, so there is no need for the dummy data
          if(fileobj%is_root) allocate(global_buf_real64(i_glob, j_glob))
          allocate(local_buf_real64(size(vdata,1), size(vdata,2)))
          local_buf_real64 = vdata
       endif

       call mpp_gather(isc+i_add, iec+i_add, jsc+j_add, jec+j_add, bc_info%pelist, &
                          local_buf_real64(i1:i2,j1:j2), &
                          global_buf_real64, fileobj%is_root)
       deallocate(local_buf_real64)
       !> If you are on fileobj's root, calculate the checksum and save the gathered data in a buffer
       if (fileobj%is_root) then
           chksum_val = mpp_chksum(global_buf_real64(1:i_glob,1:j_glob), (/mpp_pe()/))
           allocate(bc_info%globaldata2d_64(i_glob, j_glob))
           bc_info%globaldata2d_64=global_buf_real64(1:i_glob,1:j_glob)
           deallocate(global_buf_real64)
       endif
     class default
        call error("unsupported type.")
   end select

   !> Save the checksum, so you can write it later
   if (fileobj%is_root) then
      chksum = ""
      write(chksum, "(Z16)") chksum_val
      bc_info%chksum = chksum
   endif

end subroutine gather_data_bc_2d

subroutine gather_data_bc_3d(fileobj, vdata, bc_info)
  class(FmsNetcdfFile_t), intent(inout) :: fileobj
  class(*), dimension(:,:,:), intent(in) :: vdata
  type(bc_information), intent(inout) :: bc_info

  integer :: i_glob, j_glob, k_glob
  integer :: isc, iec, jsc, jec, i1, i2, j1, j2, i_add, j_add

  real(kind=real32), dimension(:,:,:), allocatable, target :: global_buf_real32, local_buf_real32
  real(kind=real64), dimension(:,:,:), allocatable, target :: global_buf_real64, local_buf_real64
  integer(kind=int64) :: chksum_val
  character(len=32) :: chksum

  !> Set the indices
  isc = bc_info%indices(1)
  iec = bc_info%indices(2)
  jsc = bc_info%indices(3)
  jec = bc_info%indices(4)

  !> This is the section of the PE that will actually be added to the global_buffer
  i1 = 1 + bc_info%x_halo
  i2 = i1 + (iec-isc)
  j1 = 1 + bc_info%y_halo
  j2 = j1 + (jec-jsc)

  !> Set up index shifts for global array
  i_add = bc_info%ishift
  j_add = bc_info%jshift

  !> Allocate a global_buffer that will be written
  if (fileobj%is_root) then
        i_glob = bc_info%global_size(1)
        j_glob = bc_info%global_size(2)
  endif

  k_glob=bc_info%global_size(3)
  !> Gather the data and calculate the checksum for the resulting array.
  select type(vdata)
    type is (real(kind=real32))
       !> If the fileobj's root is not the same as the variable's root
       if ( .not. bc_info%data_on_file_root) then
          !> Allocate global_buf_real64 to be one size bigger, global_buf_real64(i_glob+1,,:) is just dummy data
          allocate(global_buf_real32(i_glob+1, j_glob, bc_info%global_size(3)))
          !> Allocate a temp local buffer to the fileobj's root. This is needed because the data needs to be send
          !! to the fileobj's root, but because fileobj's root doesn't have any data, we just create dummy data and
          !! not write it later
          allocate(local_buf_real32(1,1,1))
          local_buf_real32 = 0.
          isc = 1+i_glob; i_add=0; iec=1+i_glob; jsc=j_glob; j_add=0; jec=j_glob
          i1=1; i2=1; j1=1; j2=1
       else
          !! In this case there is data in fileobj's root, so there is no need for the dummy data
          if(fileobj%is_root) allocate(global_buf_real32(i_glob, j_glob, k_glob))
          allocate(local_buf_real32(size(vdata,1), size(vdata,2), size(vdata,3)))
          local_buf_real32 = vdata
       endif

       call mpp_gather(isc+i_add, iec+i_add, jsc+j_add, jec+j_add, k_glob, bc_info%pelist, &
                          local_buf_real32(i1:i2,j1:j2,:), &
                          global_buf_real32, fileobj%is_root)
       deallocate(local_buf_real32)
       !> If you are on fileobj's root, calculate the checksum and save the gathered data in a buffer
       if (fileobj%is_root) then
#ifdef OVERLOAD_R4
           chksum_val = mpp_chksum(global_buf_real32(1:i_glob,1:j_glob, :), (/mpp_pe()/))
#endif
           allocate(bc_info%globaldata3d_32(i_glob, j_glob, bc_info%global_size(3)))
           bc_info%globaldata3d_32=global_buf_real32(1:i_glob,1:j_glob,:)
           deallocate(global_buf_real32)
       endif
    type is (real(kind=real64))
       !> If the fileobj's root is not the same as the variable's root
       if ( .not. bc_info%data_on_file_root) then
          !> Allocate global_buf_real64 to be one size bigger, global_buf_real64(i_glob+1,,:) is just dummy data
          allocate(global_buf_real64(i_glob+1, j_glob, bc_info%global_size(3)))
          !> Allocate a temp local buffer to the fileobj's root. This is needed because the data needs to be send
          !! to the fileobj's root, but because fileobj's root doesn't have any data, we just create dummy data and
          !! not write it later
          allocate(local_buf_real64(1,1,1))
          local_buf_real64 = 0.
          isc = 1+i_glob; i_add=0; iec=1+i_glob; jsc=j_glob; j_add=0; jec=j_glob
          i1=1; i2=1; j1=1; j2=1
       else
          !! In this case there is data in fileobj's root, so there is no need for the dummy data
          if(fileobj%is_root) allocate(global_buf_real64(i_glob, j_glob, k_glob))
          allocate(local_buf_real64(size(vdata,1), size(vdata,2), size(vdata,3)))
          local_buf_real64 = vdata
       endif

       call mpp_gather(isc+i_add, iec+i_add, jsc+j_add, jec+j_add, k_glob, bc_info%pelist, &
                          local_buf_real64(i1:i2,j1:j2,:), &
                          global_buf_real64, fileobj%is_root)
       deallocate(local_buf_real64)
       !> If you are on fileobj's root, calculate the checksum and save the gathered data in a buffer
       if (fileobj%is_root) then
           chksum_val = mpp_chksum(global_buf_real64(1:i_glob,1:j_glob, :), (/mpp_pe()/))
           allocate(bc_info%globaldata3d_64(i_glob, j_glob, bc_info%global_size(3)))
           bc_info%globaldata3d_64=global_buf_real64(1:i_glob,1:j_glob,:)
           deallocate(global_buf_real64)
       endif
     class default
        call error("unsupported type.")
   end select

   !> Save the checksum, so you can write it later
   if (fileobj%is_root) then
      chksum = ""
      write(chksum, "(Z16)") chksum_val
      bc_info%chksum = chksum
   endif

end subroutine gather_data_bc_3d
