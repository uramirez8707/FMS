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

program test_diag_manager_time

use   mpp_domains_mod
use   diag_manager_mod
use   fms_mod
use  time_manager_mod, only: time_type, set_calendar_type, set_date, NOLEAP, JULIAN, operator(+), set_time, print_time
use  diag_data_mod, only: diag_null

implicit none

type(time_type)                   :: Time
integer, dimension(2)             :: layout = (/1,1/)
integer :: nlon, nlat, nz
type(domain2d)                    :: Domain
real, dimension(:), allocatable :: x, y, z
integer :: i, j
integer :: is, ie, js, je
real, allocatable :: sst(:,:,:), ice(:,:)
integer :: id_x, id_y, id_z, id_sst1, id_sst2, id_sst3, id_sst4
logical :: used

call fms_init
call set_calendar_type(JULIAN)
call diag_manager_init

nlon = 20
nlat = 30
nz = 5

call mpp_domains_set_stack_size(17280000)
call mpp_define_domains( (/1,nlon,1,nlat/), layout, Domain, name='test_diag_manager')
call mpp_define_io_domain(Domain, (/1,1/))
call mpp_get_compute_domain(Domain, is, ie, js, je)

! Set up the data
allocate(x(nlon), y(nlat), z(nz))
allocate(sst(is:ie,js:je,1:nz), ice(is:ie,js:je))

do i=1,nlon
  x(i) = i
enddo
do j=1,nlat
  y(j) = j
enddo
do i=1,nz
   z(i) = i
enddo

sst = 666.66
ice = 619.0

! Set up the intial time
Time = set_date(2,1,1,0,0,0)

! Register the diags
id_x  = diag_axis_init('x',  x,  'point_E', 'x', long_name='point_E', Domain2=Domain)
id_y  = diag_axis_init('y',  y,  'point_N', 'y', long_name='point_N', Domain2=Domain)
id_z  = diag_axis_init('z',  z,  'point_Z', 'z', long_name='point_Z')
id_sst1 = register_diag_field  ('test_diag_manager_mod', 'sst1', (/id_x,id_y,id_z/), Time, 'SST', 'K')
if (id_sst1 .ne. 1) call mpp_error(FATAL, "The id of sst1 is not 1")

id_sst2 = register_diag_field  ('test_diag_manager_mod', 'sst2', (/id_x,id_y,id_z/), Time, 'SST', 'K')
if (id_sst2 .ne. 2) call mpp_error(FATAL, "The id of sst2 is not 2")

id_sst3 = register_diag_field  ('test_diag_manager_mod', 'sst3', (/id_x,id_y,id_z/), Time, 'SST', 'K')
if (id_sst3 .ne. diag_null) call mpp_error(FATAL, "The id of sst3 is not diag_null")

id_sst4 = register_diag_field  ('test_diag_manager_mod', 'sst4', (/id_x,id_y,id_z/), Time, 'SST', 'K')
if (id_sst4 .ne. 3) call mpp_error(FATAL, "The id of sst4 is not 3")

call diag_manager_end(Time)
call fms_end

end program test_diag_manager_time
