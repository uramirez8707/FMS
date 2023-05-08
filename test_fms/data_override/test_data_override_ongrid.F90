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

program test_data_override_ongrid

!> @brief  This programs tests data_override ability to override data for an
!! on grid case

use   mpp_domains_mod,   only: mpp_define_domains, mpp_define_io_domain, mpp_get_data_domain, &
                               mpp_domains_set_stack_size, mpp_get_compute_domain, domain2d
use   mpp_mod,           only: mpp_init, mpp_exit, mpp_pe, mpp_root_pe, mpp_error, FATAL, &
                               input_nml_file, mpp_sync
use   data_override_mod, only: data_override_init, data_override
use   fms2_io_mod,       only: fms2_io_init
use   time_manager_mod,  only: set_calendar_type, time_type, set_date, NOLEAP
use   netcdf,            only: nf90_create, nf90_def_dim, nf90_def_var, nf90_enddef, nf90_put_var, &
                               nf90_close, nf90_put_att, nf90_clobber, nf90_64bit_offset, nf90_char, &
                               nf90_double, nf90_unlimited

implicit none

integer, dimension(2)                 :: layout = (/1,1/) !< Domain layout
integer                               :: nlon             !< Number of points in x axis
integer                               :: nlat             !< Number of points in y axis
type(domain2d)                        :: Domain           !< Domain with mask table
real, allocatable, dimension(:,:)     :: runoff           !< Data to be written
integer                               :: is               !< Starting x index
integer                               :: ie               !< Ending x index
integer                               :: js               !< Starting y index
integer                               :: je               !< Ending y index
type(time_type)                       :: Time             !< Time
integer                               :: i                !< Helper indices
integer                               :: ncid             !< Netcdf file id
integer                               :: err              !< Error Code
integer                               :: dim1d, dim2d, dim3d, dim4d    !< Dimension ids
integer                               :: varid, varid2, varid3, varid4 !< Variable ids
real, allocatable, dimension(:,:,:)   :: runoff_in        !< Data to be written to file
real                                  :: expected_result  !< Expected result from data_override
integer                               :: nhalox=2, nhaloy=2
integer                               :: io_status
logical                               :: it_worked

call mpp_init
call fms2_io_init

call set_calendar_type(NOLEAP)

nlon = 23
nlat = 14

!< Create a domain nlonXnlat with mask
call mpp_domains_set_stack_size(17280000)
call mpp_define_domains( (/1,nlon,1,nlat/), layout, Domain, xhalo=nhalox, yhalo=nhaloy)
call mpp_define_io_domain(Domain, (/1,1/))
call mpp_get_data_domain(Domain, is, ie, js, je)

!< Set up the data
allocate(runoff(is:ie,js:je))

runoff = 999.

!< Initiliaze data_override
call data_override_init(ice_domain_in=Domain)

!0002-03-01
!Corresponds to 62 days since 0002-01-01 00:00:00
it_worked = .false.
Time = set_date(2,3,1,0,0,0)
call data_override('ICE', 'sic_obs', runoff, Time, override=it_worked)
if (.not. it_worked) call mpp_error(FATAL, "Data_override was not sucessful")
write(mpp_pe() + 100, *) "0002-03-01::", runoff

!0002-06-01
!Corresponds to 151 days since 0002-01-01 00:00:00
it_worked = .false.
Time = set_date(2,6,1,0,0,0)
call data_override('ICE', 'sic_obs', runoff, Time, override=it_worked)
if (.not. it_worked) call mpp_error(FATAL, "Data_override was not sucessful")
write(mpp_pe() + 100, *) "0002-06-01::", runoff

!0002-12-01
!Corresponds to 335 days since 0002-01-01 00:00:00
it_worked = .false.
Time = set_date(2,12,1,0,0,0)
call data_override('ICE', 'sic_obs', runoff, Time, override=it_worked)
if (.not. it_worked) call mpp_error(FATAL, "Data_override was not sucessful")
write(mpp_pe() + 100, *) "0002-12-01::", runoff

deallocate(runoff)

call mpp_exit

end program test_data_override_ongrid
