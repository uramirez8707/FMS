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

program test_diag_manager_UG

use   mpp_domains_mod
use   diag_manager_mod
use   fms_mod
use   time_manager_mod, only: time_type, set_calendar_type, set_date, NOLEAP, JULIAN, operator(+), set_time, print_time
use   mpp_mod

implicit none

type(time_type)      :: Time !< Simulation time
integer              :: nlon, nlat !< Number of lon/lat in each tile
type(domain2d)       :: Domain !< Cubesphere domain
type(domainug)       :: land_domain !< Unstructured domain
integer              :: i !< For do loops
real, allocatable    :: sst(:) !< Data in the unstructured grid
logical              :: used !< Flag indicating if data was sucessfully sent
integer              :: id_x, id_y, id_ug !< ids for the x,y, and unstructured grid axis
integer              :: id_sst !< id for the data in the unstructured grid
integer              :: ug_dim_size    ! Size of the unstructured axis
integer,allocatable  :: ug_dim_data(:) ! Unstructured axis data.
integer              :: npes !< Number of PE ranks the test is running with
integer,dimension(2) :: io_layout=(/1, 1/) !< Io_layout for the cubed_sphere/lat-lon grids
integer, parameter   :: ntiles=6 !< Number of tiles for the test
integer, dimension(4,ntiles) :: global_indices !< The global indices for each tile
integer, dimension(2,ntiles) :: layout !< The layout for each tile
integer, dimension(ntiles)   :: pe_start !< The starting pe in the pelist for each tile
integer, dimension(ntiles)   :: pe_end   !< The ending pe in the pelist for each tile
integer                      :: npes_group=1 !< For the land layout

call fms_init
call set_calendar_type(JULIAN)
call diag_manager_init

Time = set_date(2,1,1,0,0,0)
nlon = 20
nlat = 30
npes = mpp_npes()

call mpp_domains_set_stack_size(17280000)

!< Set up the domain
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
call create_land_domain(domain, nlon, nlat, ntiles, land_domain, npes_group)

call mpp_get_UG_compute_domain(land_domain, size=ug_dim_size)
allocate(ug_dim_data(ug_dim_size))
call mpp_get_UG_domain_grid_index(land_domain, ug_dim_data)
ug_dim_data = ug_dim_data - 1

!< Register the axis and the data
id_ug = diag_axis_init("grid_index",  real(ug_dim_data), "none", "U", long_name="grid indices", &
                         set_name="land", DomainU=land_domain, aux="geolon_t geolat_t")
id_x = diag_axis_init ( 'grid_xt', (/(real(i),i=1,nlon)/), 'degrees_E', 'X', &
          'T-cell longitude', set_name='land' )
id_y = diag_axis_init ( 'grid_yt', (/(real(i),i=1,nlat)/), 'degrees_N', 'Y', &
          'T-cell latitude', set_name='land' )
id_sst = register_diag_field  ('land_mod', 'sst', (/id_ug/), Time, 'SST', 'K')

call diag_axis_add_attribute(id_ug,'compress','grid_xt grid_yt')

!< Set up the data
allocate(sst(ug_dim_size))
sst = 999.

! Increase the time and send data
do i=1,23
   Time = set_date(2,1,1,i,0,0)
   sst = real(i)

   if(id_sst > 0) used = send_data(id_sst, sst, Time)
enddo

call diag_manager_end(Time)
call fms_end

contains
include "../fms2_io/create_atmosphere_domain.inc"
include "../fms2_io/create_land_domain.inc"

end program test_diag_manager_UG
