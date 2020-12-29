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

!> @brief  This programs tests functionality of register_axis (register_domain_decomposed_dimension)
!! interface using a 6 tile cubesphere grid and a lat-lon grid
program test_register_axis_domain
use   fms2_io_mod,     only: open_file, close_file, FmsNetcdfDomainFile_t, &
                             register_axis, get_dimension_size
use   fms_mod,         only: fms_init, fms_end
use   mpp_mod,         only: mpp_npes, mpp_pe, mpp_error, FATAL, input_nml_file
use   mpp_domains_mod, only: mpp_define_domains, mpp_define_io_domain, &
                             mpp_get_global_domain,domain2d, NORTH, EAST, &
                             mpp_get_io_domain
use   setup,           only: Params, init, create_cubed_sphere_domain

implicit none

type(Params)                          :: test_params      !< Test parameters
type(FmsNetcdfDomainFile_t)           :: fileobj          !< fms2io fileobj for domain decomposed
type(domain2d)                        :: Domain           !< Domain
type(domain2d), pointer               :: io_domain        !< Io_Domain
integer                               :: nlon             !< Number of points in x axis
integer                               :: nlat             !< Number of points in y axis
integer, dimension(2)                 :: layout=(/1,1/)   !< Domain layout
integer, dimension(2)                 :: io_layout=(/1,1/)!< Domain io_layout
integer                               :: dim_size         !< Dimension read in
integer                               :: io_status        !< Status of reading namelist
integer                               :: nlon_io          !< Number of points in x axis
integer                               :: nlat_io          !< Number of points in y axis
logical                               :: csphere=.false.  !< Flag indiciating whethere the domain is a
                                                          !! 6 tile cubesphere, default is a lat, lon grid

namelist / test_register_axis_domain_nml / layout, io_layout, csphere

nlat = 96
nlon = 96

call fms_init

read (input_nml_file, test_register_axis_domain_nml, iostat=io_status)
if (io_status > 0) call mpp_error(FATAL,'=>test_register_axis_domain_nml: Error reading input.nml')

if (csphere) then
    !< NOTE: In this test case the layout is setup as (1, npes/ntiles)
    call init(test_params, 6, nx=nlat, ny=nlon)
    call create_cubed_sphere_domain(test_params, Domain, io_layout)
else
    call mpp_define_domains( (/1,nlon,1,nlat/), layout, Domain, symmetry=.true., name='test_register_axis_domain')
    call mpp_define_io_domain(Domain, io_layout)
endif

io_domain => mpp_get_io_domain(Domain)
call mpp_get_global_domain(io_domain, xsize=nlon_io, ysize=nlat_io)

if (open_file(fileobj, "OUTPUT/test_register_axis_domain.nc", "overwrite", domain)) then
    call register_axis(fileobj, "lon", "x")
    call register_axis(fileobj, "lat", "y")

    call register_axis(fileobj, "lon_east", "x", domain_position=EAST)
    call register_axis(fileobj, "lat_north", "y", domain_position=NORTH)

    call close_file(fileobj)
else
    call mpp_error(FATAL, "test_register_axis_domain: error opening the file for writting")
endif

!< Error checking
if (open_file(fileobj, "OUTPUT/test_register_axis_domain.nc", "read", domain)) then
   call get_dimension_size(fileobj, "lat", dim_size)
   if (dim_size .ne. nlat_io) call mpp_error(FATAL, "lat is not correct")

   call get_dimension_size(fileobj, "lat_north", dim_size)
   if (dim_size .ne. nlat_io+1) call mpp_error(FATAL, "lat is not correct")

   call get_dimension_size(fileobj, "lon", dim_size)
   if (dim_size .ne. nlon_io) call mpp_error(FATAL, "lat is not correct")

   call get_dimension_size(fileobj, "lon_east", dim_size)
   if (dim_size .ne. nlon_io +1 ) call mpp_error(FATAL, "lat is not correct")

   call close_file(fileobj)
else
    call mpp_error(FATAL, "test_register_axis_domain: error opening the file for reading")
endif

call fms_end

end program
