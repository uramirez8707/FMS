!***********************************************************************
!*                   GNU Lesser General Public License
!*
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

subroutine scatter_data_bc_2d(fileobj, varname, vdata, bc_info, unlim_dim_level)
  class(FmsNetcdfFile_t), intent(inout) :: fileobj
  character(len=*), intent(in) :: varname !< Variable name.
  class(*), dimension(:,:), intent(inout) :: vdata
  type(bc_information), intent(inout) :: bc_info
  integer, intent(in), optional :: unlim_dim_level !< Unlimited dimension
                                                     !! level.

  real(kind=real64), dimension(:,:), allocatable, target :: global_buf_real64, local_buf_real64
  real(kind=real32), dimension(:,:), allocatable, target :: global_buf_real32, local_buf_real32

  integer(kind=int64) :: chksum_val
  character(len=32) :: chksum, chksum_read

  integer :: isc, iec, jsc, jec, i1, i2, j1, j2, i_add, j_add
  integer :: i_glob, j_glob

  !> Set the indices
  isc = bc_info%indices(1)
  iec = bc_info%indices(2)
  jsc = bc_info%indices(3)
  jec = bc_info%indices(4)

  !> This is the section of the PE that will actually be read to the global_buffer
  i1 = 1 + bc_info%x_halo
  i2 = i1 + (iec-isc)
  j1 = 1 + bc_info%y_halo
  j2 = j1 + (jec-jsc)

  !> Set up index shifts for global array
  i_add = bc_info%ishift
  j_add = bc_info%jshift

  if (fileobj%is_root) then
        i_glob = bc_info%global_size(1)
        j_glob = bc_info%global_size(2)
  endif

  select type(vdata)
    type is (real(kind=real32))
      if (fileobj%is_root) then
         !> If you are the file root, read in the data
         allocate(global_buf_real32(i_glob, j_glob))

         call netcdf_read_data(fileobj, varname, &
                              global_buf_real32, &
                              unlim_dim_level=unlim_dim_level, &
                              broadcast=.false.)
         !> If the checksum exists read it and compare it with the calculated from the data that was read
         if (variable_att_exists(fileobj, varname, "checksum", broadcast=.false.)) then
            call get_variable_attribute(fileobj, varname, "checksum", chksum_read, broadcast=.false.)
#ifdef OVERLOAD_R4
            chksum_val = mpp_chksum(global_buf_real32, (/mpp_pe()/))
#endif
            chksum = ""
            write(chksum, "(Z16)") chksum_val
            if (.not. string_compare(adjustl(chksum), adjustl(chksum_read))) then
                call error("scatter_data_bc_2d: "//trim(varname)//" chksum_in:"//(chksum)//" chksum_out:"//(chksum_read))
            endif
         endif
      endif

      !> If the fileobj's root is not the same as the variable's root, then no data will be read for the
      !! file root
      if (fileobj%is_root .and. .not. bc_info%is_root_pe) then
          allocate(local_buf_real32(1,1))
          local_buf_real32 = 0.
          isc = 1; i_add=0; iec=1; jsc=1; j_add=0; jec=1
          i1=1; i2=1; j1=1; j2=1
      else
          allocate(local_buf_real32(i1:i2,j1:j2))
      endif

      !> Scatter the data to the other PE
      call mpp_scatter(isc+i_add, iec+i_add, jsc+j_add, jec+j_add, bc_info%pelist, &
                       local_buf_real32(i1:i2,j1:j2), global_buf_real32, fileobj%is_root)

      !> Return if fileobj's root is not the same as the variable's root
      if (fileobj%is_root .and. .not. bc_info%is_root_pe) return

      !> Set vdata to equal the scattered data
      vdata(i1:i2,j1:j2) = local_buf_real32(i1:i2,j1:j2)

      deallocate(local_buf_real32)
      if (fileobj%is_root) deallocate(global_buf_real32)

    type is (real(kind=real64))
      if (fileobj%is_root) then
         !> If you are the file root, read in the data
         allocate(global_buf_real64(i_glob, j_glob))

         call netcdf_read_data(fileobj, varname, &
                              global_buf_real64, &
                              unlim_dim_level=unlim_dim_level, &
                              broadcast=.false.)
         !> If the checksum exists read it and compare it with the calculated from the data that was read
         if (variable_att_exists(fileobj, varname, "checksum", broadcast=.false.)) then
            call get_variable_attribute(fileobj, varname, "checksum", chksum_read, broadcast=.false.)
            chksum_val = mpp_chksum(global_buf_real64, (/mpp_pe()/))
            chksum = ""
            write(chksum, "(Z16)") chksum_val
            if (.not. string_compare(adjustl(chksum), adjustl(chksum_read))) then
                call error("scatter_data_bc_2d: "//trim(varname)//" chksum_in:"//chksum//" chksum_out:"//chksum_read)
            endif
         endif
      endif

      !> If the fileobj's root is not the same as the variable's root, then no data will be read for the
      !! file root
      if (fileobj%is_root .and. .not. bc_info%is_root_pe) then
          allocate(local_buf_real64(1,1))
          local_buf_real64 = 0.
          isc = 1; i_add=0; iec=1; jsc=1; j_add=0; jec=1
          i1=1; i2=1; j1=1; j2=1
      else
          allocate(local_buf_real64(i1:i2,j1:j2))
      endif

      !> Scatter the data to the other PE
      print *, mpp_pe(), " -", trim(varname), ": ", bc_info%pelist, fileobj%is_root

      call mpp_scatter(isc+i_add, iec+i_add, jsc+j_add, jec+j_add, bc_info%pelist, &
                       local_buf_real64(i1:i2,j1:j2), global_buf_real64, fileobj%is_root)

      !> Return if fileobj's root is not the same as the variable's root
      if (fileobj%is_root .and. .not. bc_info%is_root_pe) return

      !> Set vdata to equal the scattered data
      vdata(i1:i2,j1:j2) = local_buf_real64(i1:i2,j1:j2)

      deallocate(local_buf_real64)
      if (fileobj%is_root) deallocate(global_buf_real64)

      class default
         call error("unsupported type.")
    end select
end subroutine scatter_data_bc_2d

subroutine scatter_data_bc_3d(fileobj, varname, vdata, bc_info, unlim_dim_level)
  class(FmsNetcdfFile_t), intent(inout) :: fileobj
  character(len=*), intent(in) :: varname !< Variable name.
  class(*), dimension(:,:,:), intent(inout) :: vdata
  type(bc_information), intent(inout) :: bc_info
  integer, intent(in), optional :: unlim_dim_level !< Unlimited dimension
                                                     !! level.

  real(kind=real64), dimension(:,:,:), allocatable, target :: global_buf_real64, local_buf_real64
  real(kind=real32), dimension(:,:,:), allocatable, target :: global_buf_real32, local_buf_real32

  integer(kind=int64) :: chksum_val
  character(len=32) :: chksum, chksum_read

  integer :: isc, iec, jsc, jec, i1, i2, j1, j2, i_add, j_add
  integer :: i_glob, j_glob, k_glob

  !> Set the indices
  isc = bc_info%indices(1)
  iec = bc_info%indices(2)
  jsc = bc_info%indices(3)
  jec = bc_info%indices(4)

  !> This is the section of the PE that will actually be read to the global_buffer
  i1 = 1 + bc_info%x_halo
  i2 = i1 + (iec-isc)
  j1 = 1 + bc_info%y_halo
  j2 = j1 + (jec-jsc)

  !> Set up index shifts for global array
  i_add = bc_info%ishift
  j_add = bc_info%jshift

  if (fileobj%is_root) then
        i_glob = bc_info%global_size(1)
        j_glob = bc_info%global_size(2)
  endif

  k_glob = bc_info%global_size(3)

  select type(vdata)
    type is (real(kind=real32))
      if (fileobj%is_root) then
         !> If you are the file root, read in the data
         allocate(global_buf_real32(i_glob, j_glob, k_glob))

         call netcdf_read_data(fileobj, varname, &
                              global_buf_real32, &
                              unlim_dim_level=unlim_dim_level, &
                              broadcast=.false.)
         !> If the checksum exists read it and compare it with the calculated from the data that was read
         if (variable_att_exists(fileobj, varname, "checksum", broadcast=.false.)) then
            call get_variable_attribute(fileobj, varname, "checksum", chksum_read, broadcast=.false.)
#ifdef OVERLOAD_R4
            chksum_val = mpp_chksum(global_buf_real32, (/mpp_pe()/))
#endif
            chksum = ""
            write(chksum, "(Z16)") chksum_val
            if (.not. string_compare(adjustl(chksum), adjustl(chksum_read))) then
                call error("scatter_data_bc_2d: "//trim(varname)//" chksum_in:"//(chksum)//" chksum_out:"//(chksum_read))
            endif
         endif
      endif

      !> If the fileobj's root is not the same as the variable's root, then no data will be read for the
      !! file root
      if (fileobj%is_root .and. .not. bc_info%is_root_pe) then
          allocate(local_buf_real32(1,1,1))
          local_buf_real32 = 0.
          isc = 1; i_add=0; iec=1; jsc=1; j_add=0; jec=1
          i1=1; i2=1; j1=1; j2=1
      else
          allocate(local_buf_real32(i1:i2,j1:j2,k_glob))
      endif

      !> Scatter the data to the other PE
      call mpp_scatter(isc+i_add, iec+i_add, jsc+j_add, jec+j_add, k_glob, bc_info%pelist, &
                       local_buf_real32(i1:i2,j1:j2,:), global_buf_real32, fileobj%is_root)

      !> Return if fileobj's root is not the same as the variable's root
      if (fileobj%is_root .and. .not. bc_info%is_root_pe) return

      !> Set vdata to equal the scattered data
      vdata(i1:i2,j1:j2,:) = local_buf_real32(i1:i2,j1:j2,:)

      deallocate(local_buf_real32)
      if (fileobj%is_root) deallocate(global_buf_real32)

    type is (real(kind=real64))
      if (fileobj%is_root) then
         !> If you are the file root, read in the data
         allocate(global_buf_real64(i_glob, j_glob, k_glob))

         call netcdf_read_data(fileobj, varname, &
                              global_buf_real64, &
                              unlim_dim_level=unlim_dim_level, &
                              broadcast=.false.)
         !> If the checksum exists read it and compare it with the calculated from the data that was read
         if (variable_att_exists(fileobj, varname, "checksum", broadcast=.false.)) then
            call get_variable_attribute(fileobj, varname, "checksum", chksum_read, broadcast=.false.)
            chksum_val = mpp_chksum(global_buf_real64, (/mpp_pe()/))
            chksum = ""
            write(chksum, "(Z16)") chksum_val
            if (.not. string_compare(adjustl(chksum), adjustl(chksum_read))) then
                call error("scatter_data_bc_2d: "//trim(varname)//" chksum_in:"//chksum//" chksum_out:"//chksum_read)
            endif
         endif
      endif

      !> If the fileobj's root is not the same as the variable's root, then no data will be read for the
      !! file root
      if (fileobj%is_root .and. .not. bc_info%is_root_pe) then
          allocate(local_buf_real64(1,1,1))
          local_buf_real64 = 0.
          isc = 1; i_add=0; iec=1; jsc=1; j_add=0; jec=1
          i1=1; i2=1; j1=1; j2=1
      else
          allocate(local_buf_real64(i1:i2,j1:j2,k_glob))
      endif

      !> Scatter the data to the other PE
      call mpp_scatter(isc+i_add, iec+i_add, jsc+j_add, jec+j_add, k_glob, bc_info%pelist, &
                       local_buf_real64(i1:i2,j1:j2,:), global_buf_real64, fileobj%is_root)

      !> Return if fileobj's root is not the same as the variable's root
      if (fileobj%is_root .and. .not. bc_info%is_root_pe) return

      !> Set vdata to equal the scattered data
      vdata(i1:i2,j1:j2,:) = local_buf_real64(i1:i2,j1:j2,:)

      deallocate(local_buf_real64)
      if (fileobj%is_root) deallocate(global_buf_real64)

      class default
         call error("unsupported type.")
    end select
end subroutine scatter_data_bc_3d
