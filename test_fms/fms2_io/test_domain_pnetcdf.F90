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

program test_domain_pnetcdf
!> @brief  This programs tests
use   mpp_domains_mod, only: mpp_domains_set_stack_size, mpp_define_domains, mpp_define_io_domain, &
                             mpp_get_compute_domain,domain2d,mpp_get_domain_tile_commid
use   mpp_mod,         only: mpp_pe, mpp_root_pe, mpp_error, FATAL, mpp_clock_id, mpp_clock_end, &
                             mpp_clock_begin, mpp_sync
use   fms2_io_mod,     only: open_file, register_axis, register_variable_attribute, close_file, &
                             FmsNetcdfDomainFile_t, write_data, register_field, read_data, &
                             parse_mask_table
use   fms_mod,         only: fms_init, fms_end
use   netcdf,          only: nf90_open, nf90_get_var, nf90_nowrite, NF90_NOERR, nf90_get_var, &
                             nf90_close
use   mpi,             only: mpi_barrier, mpi_comm_world
use   platform_mod

implicit none

integer, dimension(2)                 :: layout = (/2,3/) !< Domain layout
integer                               :: nlon             !< Number of points in x axis
integer                               :: nlat             !< Number of points in y axis
type(domain2d)                        :: Domain           !< Domain with mask table
real, dimension(:), allocatable       :: x                !< x axis data
real, dimension(:), allocatable       :: y                !< y axis data
real(kind=r8_kind), allocatable, dimension(:,:) :: sst     !< Data to be written
real(kind=r8_kind), allocatable, dimension(:,:) :: sst_in  !< Buffer where data will be read with netcdf
real(kind=r8_kind), allocatable, dimension(:,:) :: sst_in2 !< Buffer where data will be read with fms2io
logical, allocatable, dimension(:,:)  :: parsed_mask      !< Parsed masked
character(len=6), dimension(2)        :: names            !< Dimensions names
type(FmsNetcdfDomainFile_t)           :: fileobj          !< fms2io fileobj for domain decomposed
integer                               :: err              !< Return code.
integer                               :: ncid             !< File ID for checking file.
integer                               :: i, j             !< Helper integers
integer                               :: is               !< Starting x index
integer                               :: ie               !< Ending x index
integer                               :: js               !< Starting y index
integer                               :: je               !< Ending y index
integer :: domain_read, mpi_netcdf

call fms_init

nlon = 384
nlat = 384

!< Create a domain nlonXnlat with mask
call mpp_domains_set_stack_size(17280000)
call mpp_define_domains( (/1,nlon,1,nlat/), layout, Domain, name='test_domain_read')
call mpp_define_io_domain(Domain, layout)
call mpp_get_compute_domain(Domain, is, ie, js, je)

!< Set up the data
allocate(x(nlon), y(nlat))
allocate(sst(is:ie,js:je))
allocate(sst_in2(is:ie,js:je))

do i=1,nlon
  x(i) = i
enddo
do j=1,nlat
  y(j) = j
enddo

sst = real(7., kind=r8_kind)

!< Open a netCDF file and initialize the file object.
if (open_file(fileobj, "test_domain_read.nc", "overwrite", domain, nc_format="netcdf4")) then
    !< Register the axis
    names(1) = "lon"
    names(2) = "lat"
    call register_axis(fileobj, "lon", "x")
    call register_axis(fileobj, "lat", "y")

    !< Register the variable and Write out the data
    call register_field(fileobj, "sst", "double", names(1:2))
    call register_variable_attribute(fileobj, "sst", "_FillValue", real(999., kind=r8_kind))
    call write_data(fileobj, "sst", sst)

    !< Close the file
    call close_file(fileobj)
else
   call mpp_error(FATAL, "test_domain_read: error opening the file for writting")
endif

call mpp_sync()

domain_read = mpp_clock_id( 'Domain Read' )
mpi_netcdf = mpp_clock_id("MPI Netcdf Read")

!< Read the file back using fms2io

call mpp_clock_begin(domain_read)
sst_in2 = 0.
if (open_file(fileobj, "test_domain_read.nc", "read", domain)) then
   names(1) = "lon"
   names(2) = "lat"
   call register_axis(fileobj, "lon", "x")
   call register_axis(fileobj, "lat", "y")

   !< Register the variable and read out the data
   call register_field(fileobj, "sst", "double", names(1:2))

   call read_data(fileobj, "sst", sst_in2)
   call close_file(fileobj)
endif

call mpp_sync()
call mpp_clock_end(domain_read)


call mpp_clock_begin(mpi_netcdf)
fileobj%use_collective = .true.
fileobj%tile_comm = mpp_get_domain_tile_commid(domain)
if (open_file(fileobj, "test_domain_read.nc", "read", domain)) then
  names(1) = "lon"
  names(2) = "lat"
  call register_axis(fileobj, "lon", "x")
  call register_axis(fileobj, "lat", "y")

  !< Register the variable and read out the data
  call register_field(fileobj, "sst", "double", names(1:2))

  call read_data(fileobj, "sst", sst_in2)
  call close_file(fileobj)
endif
call mpp_sync()
call mpp_clock_end(mpi_netcdf)

call fms_end
end program test_domain_pnetcdf
