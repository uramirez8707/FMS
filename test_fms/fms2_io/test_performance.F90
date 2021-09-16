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

program test_performance
!> @brief  This programs tests fms2io/include/domain_write ability to write
!! data when the domain contains a mask table. For the points that are
!! masked out, no data should be writen.
!! It also tests fms2io/include/domain_read ability to read the data when
!! the domain contains a mask table. For this case the masked data should
!! not be read.

use   mpp_domains_mod, only: mpp_domains_set_stack_size, mpp_define_domains, mpp_define_io_domain, &
                             mpp_get_compute_domain,domain2d
use   mpp_mod
use   fms2_io_mod
use   fms_mod,         only: fms_init, fms_end
use   platform_mod

implicit none

integer, dimension(2)                 :: layout=(/1,1/)           !< Domain layout
integer, dimension(2)                 :: io_layout=(/1,1/)           !< Domain layout
integer                               :: nlon=20             !< Number of points in x axis
integer                               :: nlat=20             !< Number of points in y axis
type(domain2d)                        :: Domain           !< Domain with mask table
real(kind=r8_kind), allocatable, dimension(:,:,:) :: sst     !< Data to be written
real(kind=r8_kind), allocatable, dimension(:,:,:) :: sst_in2 !< Buffer where data will be read with fms2io
character(len=10), dimension(3)        :: names            !< Dimensions names
type(FmsNetcdfDomainFile_t)           :: fileobj          !< fms2io fileobj for domain decomposed
integer                               :: i, j             !< Helper integers
integer                               :: is               !< Starting x index
integer                               :: ie               !< Ending x index
integer                               :: js               !< Starting y index
integer                               :: je               !< Ending y index
integer                               :: nz=20               !< number of vertical dimensions
integer                               :: newclock1, newclock
integer :: io_status

namelist / test_performance_nml / nlon, nlat, nz, layout, io_layout

call fms_init

read (input_nml_file, test_performance_nml, iostat=io_status)
if (io_status > 0) call mpp_error(FATAL,'=>test_peformance: Error reading input.nml')

if (mpp_pe() .eq. mpp_root_pe()) then
   print *, "nlon = ", nlon, " nlat = ", nlat, " layout=", layout, " io_layout=", io_layout
endif

newClock = mpp_clock_id( 'Writing' )
newClock1 = mpp_clock_id( 'Reading' )

!< Create a domain nlonXnlat with mask
call mpp_domains_set_stack_size(17280000)
call mpp_define_domains( (/1,nlon,1,nlat/), layout, Domain, name='test_performance')
call mpp_define_io_domain(Domain, io_layout)
call mpp_get_compute_domain(Domain, is, ie, js, je)

!< Set up the data
allocate(sst(is:ie,js:je, nz))
allocate(sst_in2(is:ie,js:je, nz))

sst = real(7., kind=r8_kind)

!< Open a netCDF file and initialize the file object.
if (mpp_pe() .eq. mpp_root_pe()) print *, "Writing the restarts"
call mpp_clock_begin(newClock)
if (open_file(fileobj, "test_performance.nc", "overwrite", domain, is_restart=.true.)) then
    !< Register the axis
    names(1) = "xaxis_1"
    names(2) = "yaxis_1"
    names(3) = "zaxis_1"

    call register_axis(fileobj, names(1), "x")
    call register_axis(fileobj, names(2), "y")
    call register_Axis(fileobj, names(3), nz)

    !< Register the variable and Write out the data
    call register_restart_field(fileobj, "sst", sst, names(1:3))

    call write_restart(fileobj)

    !< Close the file
    call close_file(fileobj)
else
   call mpp_error(FATAL, "test_performance: error opening the file for writting")
endif
call mpp_clock_end(newClock)

call mpp_sync()

call mpp_clock_begin(newClock1)
if (open_file(fileobj, "test_performance.nc", "read", domain, is_restart=.true.)) then

    !< Register the axis
    names(1) = "xaxis_1"
    names(2) = "yaxis_1"
    names(3) = "zaxis_1"

    call register_axis(fileobj, names(1), "x")
    call register_axis(fileobj, names(2), "y")
    call register_Axis(fileobj, names(3), 64)

    !< Register the variable and Write out the data
    call register_restart_field(fileobj, "sst", sst, names(1:3))

    call read_restart(fileobj)
    call close_file(fileobj)
endif
call mpp_clock_end(newClock1)

call fms_end
end program test_performance
