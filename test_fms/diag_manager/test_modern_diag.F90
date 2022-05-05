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

!> @brief  This programs tests diag_manager with the following diag_table
!! test_diag_manager
!! 2 1 1 0 0 0
!! "ocn%4yr%2mo%2dy%2hr",      1,  "days", 1, "days", "time", 1, "days", "2 1 1 0 0 0"
!! "test_diag_manager_mod", "sst", "sst", "ocn%4yr%2mo%2dy%2hr",  "all", .true., "none", 2

program test_modern_diag

use   mpp_domains_mod,  only: domain2d, mpp_domains_set_stack_size, mpp_define_domains, mpp_define_io_domain
use   diag_manager_mod, only: diag_manager_init, diag_manager_end, diag_axis_init
use   fms_mod,          only: fms_init, fms_end
use   mpp_mod,          only: FATAL, mpp_error
use   time_manager_mod, only: time_type, set_calendar_type, set_date, JULIAN, set_time

implicit none

type(time_type)                   :: Time             !< Time of the simulation
integer, dimension(2)             :: layout = (/1,1/) !< Layout to use when setting up the domain
integer                           :: nx               !< Number of x points
integer                           :: ny               !< Number of y points
integer                           :: nz               !< Number of z points
type(domain2d)                    :: Domain           !< 2D domain
real, dimension(:), allocatable   :: x                !< X axis data
real, dimension(:), allocatable   :: y                !< Y axis_data
real, dimension(:), allocatable   :: z                !< Z axis_data
integer                           :: i                !< For do loops
integer                           :: id_x             !< axis id for the x dimension
integer                           :: id_y             !< axis id for the y dimension
integer                           :: id_z             !< axis id for the z dimention

call fms_init
call set_calendar_type(JULIAN)
call diag_manager_init

nx = 20
ny = 30
nz = 5

call mpp_domains_set_stack_size(17280000)
call mpp_define_domains( (/1,nx,1,ny/), layout, Domain, name='test_diag_manager')
call mpp_define_io_domain(Domain, (/1,1/))

! Set up the data
allocate(x(nx), y(ny), z(nz))
do i=1,nx
  x(i) = i
enddo
do i=1,ny
  y(i) = i
enddo
do i=1,nz
   z(i) = i
enddo

! Set up the intial time
Time = set_date(2,1,1,0,0,0)

! Register the diags axis
id_x  = diag_axis_init('x',  x,  'point_E', 'x', long_name='point_E', Domain2=Domain)
id_y  = diag_axis_init('y',  y,  'point_N', 'y', long_name='point_N', Domain2=Domain)
id_z  = diag_axis_init('z',  z,  'point_Z', 'z', long_name='point_Z')

if (id_x .ne. 1) call mpp_error(FATAL, "The x axis does not have the expected id")
if (id_y .ne. 2) call mpp_error(FATAL, "The y axis does not have the expected id")
if (id_z .ne. 3) call mpp_error(FATAL, "The z axis does not have the expected id")

call diag_manager_end(Time)
call fms_end

end program test_modern_diag
