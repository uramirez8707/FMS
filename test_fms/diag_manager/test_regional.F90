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

!> @brief  This programs tests diat_manager with the following diag_table
!!test_diag_manager
!!2 1 1 0 0 0
!!"test_regional",         1, "hours",   1, "hours", "time"
!!"ocean_mod", "sst", "sst", "test_regional",  "all", "mean", "183 185 -28.5 31.5 2 3", 2

program test_regional

use   mpp_domains_mod
use   diag_manager_mod
use   fms_mod,          only: fms_init, fms_end
use   time_manager_mod, only: time_type, set_calendar_type, set_date, NOLEAP, JULIAN, operator(+), set_time, print_time, &
                             increment_date
use   fms2_io_mod
use   mpp_mod

implicit none

type(time_type)                   :: Time
integer, dimension(2)             :: layout = (/2,3/)
integer :: nlon, nlat, nz
type(domain2d)                    :: Domain
real, dimension(:), allocatable :: x, y, z
integer :: i, j
integer :: is, ie, js, je
real, allocatable, dimension(:,:,:) :: sst
integer :: id_x, id_y, id_z, id_sst, id_ice
integer :: used

call fms_init
call set_calendar_type(JULIAN)
call diag_manager_init

nlon = 360
nlat = 180
nz = 5

call mpp_domains_set_stack_size(17280000)
call mpp_define_domains( (/1,nlon,1,nlat/), layout, Domain)
call mpp_define_io_domain(Domain, (/1,1/))
call mpp_get_compute_domain(Domain, is, ie, js, je)

! Set up the data
allocate(x(nlon), y(nlat), z(nz))
allocate(sst(is:ie,js:je,1:nz))

!< This is the GLOBAL axis data
do i=1, nlon
  x(i) = real(i)
enddo
do j=1, nlat
  y(j) = -90.+real(j)
enddo
do i=1,nz
   z(i) = real(i)
enddo

sst = 999.9

! Set up the intial time
Time = set_date(2,1,1,0,0,0)

! Register the diags
id_x  = diag_axis_init('x',  x,  'point_E', 'x', long_name='point_E', Domain2=Domain)
id_y  = diag_axis_init('y',  y,  'point_N', 'y', long_name='point_N', Domain2=Domain)
id_z  = diag_axis_init('z',  z,  'point_Z', 'z', long_name='point_Z')
id_sst = register_diag_field  ('ocean_mod', 'sst', (/id_x,id_y,id_z/), Time, 'SST', 'K')

! Send the axis data
used = send_data(id_x, x, Time)
used = send_data(id_y, y, Time)
used = send_data(id_z, z, Time)

! Increase the time and send data
do i=1,6
   !< Increase the time by 30 minutes
   Time = increment_date(Time, 0, 0, 0, 0, 30, 0)
   sst = real(i)
   if(id_sst > 0) used = send_data(id_sst, sst, Time)
enddo

deallocate(x, y, z, sst)
call diag_manager_end(Time)

call mpp_sync()
call check_for_errors()

call fms_end

contains

subroutine check_for_errors()

  real    :: axis_data_boundary(2, 6) !< The expected begining and ending axis data for the
                                      !! x, y, z axis for the 2 file
  real, allocatable :: axis_data(:)
  integer :: npoints (2, 3)           !< Number of expected grid points in x, y, z for the
                                      !! 2 file
  character(len=25) :: filenames(2)   !< The expected filenames for the output diagnostics
  character(len=25) :: axis_names(3)
  logical :: test_passed              !< Flag indicating if a test passed
  type(FmsNetcdfFile_t) :: fileobj
  integer :: axis_size(1), var_size(4)

  integer :: i, j                     !< For do loops

  !< Local axis data:
  !! Rank 0: x=1:180, y=-89:-30
  !! Rank 1: x=181:360, y=-89:-30
  !! Rank 2: x=1:180, y=-29:30
  !! Rank 3: x=181:360, y=-29:30
  !! Rank 4: x=1:180, y=31:90
  !! Rank 5: x=181:360, y=31:90

  !< The data table:
  !!  "test_regional",         1, "hours",   1, "hours", "time"
  !!  "ocean_mod", "sst", "sst", "test_regional",  "all", "mean", "183 185 -28.5 31.5 2 3", 2
  !! So only data is only available for ranks: 1 and 3

  filenames(1) = "test_regional.nc.0003"
  filenames(2) = "test_regional.nc.0005"

  axis_names(1) = "x_sub01"
  axis_names(2) = "y_sub01"
  axis_names(3) = "z_sub01"

  !< The nearest value to -28.5 is -29 ... and to 31.5 is 31 ...
  axis_data_boundary(1,:) = (/183., 185., -29., 30., 2., 3./)
  axis_data_boundary(2,:) = (/183., 185.,  31., 31., 2., 3./)

  npoints(1,:) = (/3, 60, 2/)
  npoints(2,:) = (/3, 1,  2/)

  if(mpp_pe() .eq. mpp_root_pe()) then
    do i = 1, 2 !< Loop through number of files
       !< Check for the expected files
       inquire(file=filenames(i), exist=test_passed)
       if (.not. test_passed) call mpp_error(FATAL, "The expected file:"//trim(filenames(i))//&
                                   " does not exit!")
       if (.not. open_file(fileobj, filenames(i), "read")) &
          & call mpp_error(FATAL, "Error opening the file:"//trim(filenames(i)))

       !< Check the axis data
       do j = 1, 3
          if (.not. variable_exists(fileobj, axis_names(j))) call mpp_error(FATAL, &
              "The axis data :"//trim(axis_names(j))//" does not exist!")

          call get_variable_size(fileobj, axis_names(j), axis_size)
          if (axis_size(1) .ne. npoints(i,j)) call mpp_error(FATAL, "The size of the axis_data:"//trim(axis_names(j))//&
                                              "is not correct!")

          allocate(axis_data(axis_size(1)))
          call read_data(fileobj, axis_names(j), axis_data)

          !< Check the begining of the axis_data
          if (real(axis_data(1)) .ne. real(axis_data_boundary(i, 1+2*(j-1)))) then
              print *, axis_data(1)
              print *, axis_data_boundary(i, 1+2*(j-1))
              call mpp_error(FATAL, "The begining of the axis_data does not match:"//trim(axis_names(j)))
          endif

          !< Check the end of the axis_data
          if (real(axis_data(axis_size(1))) .ne. real(axis_data_boundary(i, 2+2*(j-1)))) then
              print *, axis_data(axis_size(1))
              print *, axis_data_boundary(i, 2+2*(j-1))
              call mpp_error(FATAL, "The ending of the axis_data does not match:"//trim(axis_names(j)))
          endif

          deallocate(axis_data)
       enddo

       !< Check the sst
       if (.not. variable_exists(fileobj, "sst")) call mpp_error(FATAL, "variable does not exist")
       call get_variable_size(fileobj, "sst", var_size)

       do j = 1, 4
          if (j==4) then
             if (var_size(j) .ne. 3) call mpp_error(FATAL, "variable has the wrong dimensions")
          else
             if (var_size(j) .ne. npoints(i,j)) call mpp_error(FATAL, "variable has the wrong dimensions")
          endif
       enddo

       call close_file(fileobj)

    enddo !< do i = 1, 2
  endif

end subroutine

end program test_regional
