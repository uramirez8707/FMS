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
use   fms_io_mod
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
type(restart_file_type)           :: fileobj          !< fms2io fileobj for domain decomposed
integer                               :: i, j             !< Helper integers
integer                               :: is               !< Starting x index
integer                               :: ie               !< Ending x index
integer                               :: js               !< Starting y index
integer                               :: je               !< Ending y index
integer                               :: nz=20               !< number of vertical dimensions
integer                               :: newclock1, newclock
integer :: io_status, id_restart

namelist / test_performance_nml / nlon, nlat, nz, layout, io_layout

call fms_init

read (input_nml_file, test_performance_nml, iostat=io_status)
if (io_status > 0) call mpp_error(FATAL,'=>test_peformance: Error reading input.nml')

newClock = mpp_clock_id( 'Writing' )
newClock1 = mpp_clock_id( 'Reading' )

!< Create a domain nlonXnlat with mask
call mpp_domains_set_stack_size(17280000)
call mpp_define_domains( (/1,nlon,1,nlat/), layout, Domain, name='test_performance')
call mpp_define_io_domain(Domain, (/1,1/))
call mpp_get_compute_domain(Domain, is, ie, js, je)

!< Set up the data
allocate(sst(is:ie,js:je, nz))
allocate(sst_in2(is:ie,js:je, nz))

sst = real(7., kind=r8_kind)

!< Open a netCDF file and initialize the file object.
call mpp_clock_begin(newClock)
id_restart = register_restart_field(fileobj, "test_performance_fmsio.nc", "sst", sst, domain)
call save_restart(fileobj)
call mpp_clock_end(newClock)

call mpp_sync()

call mpp_clock_begin(newClock1)
call restore_state(fileobj, directory="RESTART/")
call mpp_clock_end(newClock1)

call fms_end
end program test_performance
