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

!> @brief  Checks the output file after running test_reduction_methods using the "none" reduction method
program check_time_none
  use fms_mod,           only: fms_init, fms_end
  use fms2_io_mod,       only: FmsNetcdfFile_t, read_data, close_file, open_file
  use mpp_mod,           only: mpp_npes, mpp_error, FATAL, mpp_pe
  use platform_mod,      only: r8_kind
  use testing_utils,     only: allocate_buffer

  type(FmsNetcdfFile_t)              :: fileobj            !< FMS2 fileobj
  real(kind=r8_kind), allocatable    :: cdata_out(:,:,:,:) !< Data in the compute domain
  integer                            :: nx                 !< Number of points in the x direction
  integer                            :: ny                 !< Number of points in the y direction
  integer                            :: nz                 !< Number of points in the z direction
  integer                            :: nw                 !< Number of points in the 4th dimension
  integer                            :: i                  !< For looping
  
  call fms_init()

  nx = 96
  ny = 96
  nz = 5
  nw = 2

  if (.not. open_file(fileobj, "test_none.nc", "read")) &
    call mpp_error(FATAL, "unable to open file")

  cdata_out = allocate_buffer(1, nx, 1, ny, nz, nw)

  do i = 1, 8
    cdata_out = -999_r8_kind
    call read_data(fileobj, "var1_none", cdata_out(:,1,1,1), unlim_dim_level=i)
    call check_data_1d(cdata_out(:,1,1,1), i)

    cdata_out = -999_r8_kind
    call read_data(fileobj, "var2_none", cdata_out(:,:,1,1), unlim_dim_level=i)
    call check_data_2d(cdata_out(:,:,1,1), i)

    cdata_out = -999_r8_kind
    call read_data(fileobj, "var3_none", cdata_out(:,:,:,1), unlim_dim_level=i)
    call check_data_3d(cdata_out(:,:,:,1), i)
  enddo

  call fms_end()

contains

subroutine check_data_1d(buffer, time_level)
    real(kind=r8_kind), intent(inout) :: buffer(:)
    real(kind=r8_kind) :: buffer_exp
    integer, intent(in) :: time_level
    
    integer ii, j, k, l

    do ii = 1, size(buffer, 1)
      buffer_exp = real(ii, kind=r8_kind)* 1000_r8_kind+10_r8_kind+1_r8_kind + &
                   real(time_level*6, kind=r8_kind)/100_r8_kind
      if (abs(buffer(ii) - buffer_exp) > 0.01) then
        print *, mpp_pe(), ii, buffer(ii), buffer_exp
        call mpp_error(FATAL, "Data is not correct")
      endif
    enddo
  end subroutine check_data_1d

  subroutine check_data_2d(buffer, time_level)
    real(kind=r8_kind), intent(inout) :: buffer(:,:)   !< Buffer read from the table
    integer,            intent(in)    :: time_level    !< Time level read in
    real(kind=r8_kind)                :: buffer_exp    !< Expected result

    integer ii, j, k, l !< For looping

    do ii = 1, size(buffer, 1)
      do j = 1, size(buffer, 2)
        buffer_exp = real(ii, kind=r8_kind)* 1000_r8_kind+ &
                     10_r8_kind*real(j, kind=r8_kind)+1_r8_kind + &
                     real(time_level*6, kind=r8_kind)/100_r8_kind
        if (abs(buffer(ii, j) - buffer_exp) > 0.01) then
          print *, mpp_pe(), ii, j, buffer(ii, j), buffer_exp
          call mpp_error(FATAL, "Check_time_none::check_data_2d:: Data is not correct")
        endif
      enddo
    enddo
  end subroutine check_data_2d

  subroutine check_data_3d(buffer, time_level)
    real(kind=r8_kind), intent(inout) :: buffer(:,:,:) !< Buffer read from the table
    integer,            intent(in)    :: time_level    !< Time level read in
    real(kind=r8_kind)                :: buffer_exp    !< Expected result
    
    integer ii, j, k, l !< For looping

    do ii = 1, size(buffer, 1)
      do j = 1, size(buffer, 2)
        do k = 1, size(buffer, 3)
          buffer_exp = real(ii, kind=r8_kind)* 1000_r8_kind + &
                       10_r8_kind*real(j, kind=r8_kind) + &
                       1_r8_kind*real(k, kind=r8_kind) + &
                       real(time_level*6, kind=r8_kind)/100_r8_kind
          if (abs(buffer(ii, j, k) - buffer_exp) > 0.01) then
            print *, mpp_pe(), ii, buffer(ii, j, k), buffer_exp
            call mpp_error(FATAL, "Data is not correct")
          endif
        enddo
      enddo
    enddo
  end subroutine check_data_3d
end program