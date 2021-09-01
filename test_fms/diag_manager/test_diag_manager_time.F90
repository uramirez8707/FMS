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

implicit none

type(time_type)                   :: Time
integer :: nlon, nlat, nz
type(domain2d)                    :: Domain
real, dimension(:), allocatable :: x, y, z
integer :: i, j
integer :: is, ie, js, je
real, allocatable :: sst(:,:,:), ice(:,:)
integer :: id_x, id_y, id_z, id_sst, id_ice
logical :: used

integer :: domain_type = 1 !< Type of domain: 1: "lat_lon", 2: "cubed_sphere"
integer :: io_status !< Error code after reading the namelist
integer :: npes !< Number of PE ranks the test is running with
integer, dimension(2) :: io_layout=(/1, 1/) !< Io_layout for the cubed_sphere/lat-lon grids

!< For the cubesphere domain
integer, parameter :: ntiles=6 !< Number of tiles for the test
integer, dimension(4,ntiles) :: global_indices !< The global indices for each tile
integer, dimension(2,ntiles) :: layout !< The layout for each tile
integer, dimension(ntiles) :: pe_start !< The starting pe in the pelist for each tile
integer, dimension(ntiles) :: pe_end   !< The ending pe in the pelist for each tile

namelist / test_diag_manager_time_nml / domain_type

call fms_init
call set_calendar_type(JULIAN)
call diag_manager_init

read (input_nml_file, test_diag_manager_time_nml, iostat=io_status)
if (io_status > 0) call mpp_error(FATAL,'=>test_diag_manager_nml: Error reading input.nml')

nlon = 20
nlat = 30
nz = 5
npes = mpp_npes()

call mpp_domains_set_stack_size(17280000)

if (domain_type .eq. 1) then
   call mpp_define_domains( (/1,nlon,1,nlat/), (/1,1/), Domain, name='test_diag_manager')
   call mpp_define_io_domain(Domain, (/1,1/))
else if (domain_type .eq. 2 ) then
   do i = 1,ntiles
      global_indices(:, i) = (/1, nlon, 1, nlat/)
      layout(:, i) = (/1, npes/ntiles/)
      pe_start(i) = (i-1)*(npes/ntiles)
      pe_end(i) = i*(npes/ntiles) - 1
   enddo

   call create_atmosphere_domain((/nlon, nlon, nlon, nlon, nlon, nlon/), &
                                (/nlat, nlat, nlat, nlat, nlat, nlat/), &
                                global_indices, layout, pe_start, pe_end, &
                                io_layout, domain)
endif
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
id_sst = register_diag_field  ('test_diag_manager_mod', 'sst', (/id_x,id_y,id_z/), Time, 'SST', 'K')
id_ice = register_diag_field  ('test_diag_manager_mod', 'ice', (/id_x,id_y/), Time, 'ICE', 'm')

! Send the first time's data
used = send_data(id_sst, sst, Time)
used = send_data(id_sst, ice, Time)

! Increase the time and send data
do i=1,23
   Time = set_date(2,1,1,i,0,0)
   sst = real(i)
   ice = real(i)

   if(id_sst > 0) used = send_data(id_sst, sst, Time)
   if(id_ice > 0) used = send_data(id_ice, ice, Time)
enddo

call diag_manager_end(Time)
call fms_end

contains
include "../fms2_io/create_atmosphere_domain.inc"

end program test_diag_manager_time
