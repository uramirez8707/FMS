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
integer, dimension(2)             :: layout = (/1,1/)
integer :: nlon, nlat, nz
type(domain2d)                    :: Domain
real, dimension(:), allocatable :: x, y, z
integer :: i, j
integer :: is, ie, js, je
real, allocatable :: sst(:,:,:), ice(:,:)
real, allocatable :: wut(:,:,:)
logical, allocatable :: wutt(:,:,:)
integer :: id_x, id_y, id_z, id_sst, id_ice
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
allocate(sst(is:ie,js:je,1:nz), ice(is:ie,js:je), wut(is:ie,js:je, 1:nz), wutt(is:ie,js:je, 1:nz))
wut = 1
wutt = .false.

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

! Increase the time and send data
do i=1,23
Time = set_date(2,1,1,i,0,0)
sst = real(i)
ice = real(i)
if(id_sst > 0) used = send_data_infra_3d(id_sst, sst, Time=Time, rmask=wut) !, mask=wutt)
if(id_ice > 0) used = send_data(id_ice, ice, Time)
enddo

call diag_manager_end(Time)
call fms_end

contains

logical function send_data_infra_3d(diag_field_id, field, is_in, ie_in, js_in, je_in, ks_in, ke_in, &
                                    time, mask, rmask, weight, err_msg)
  integer,                             intent(in) :: diag_field_id !< The diagnostic manager identifier for this field
  real, dimension(:,:,:),              intent(in) :: field !< A rank 1 array of floating point values being recorded
  integer,                   optional, intent(in) :: is_in !< The starting i-index for the data being recorded
  integer,                   optional, intent(in) :: ie_in !< The end i-index for the data being recorded
  integer,                   optional, intent(in) :: js_in !< The starting j-index for the data being recorded
  integer,                   optional, intent(in) :: je_in !< The end j-index for the data being recorded
  integer,                   optional, intent(in) :: ks_in !< The starting k-index for the data being recorded
  integer,                   optional, intent(in) :: ke_in !< The end k-index for the data being recorded
  type(time_type),           optional, intent(in) :: time  !< The time for the current record
  logical, dimension(:,:,:), optional, intent(in) :: mask  !< An optional 3-d logical mask
  real, dimension(:,:,:),    optional, intent(in) :: rmask !< An optional 3-d mask array
  real,                      optional, intent(in) :: weight !< A scalar weight factor to apply to the current
                                                           !! record if there is averaging in time
  character(len=*),          optional, intent(out) :: err_msg !< A log indicating the status of the post upon
                                                           !! returning to the calling routine

  if (present(mask) .and. present(rmask)) then
    send_data_infra_3d = send_data(diag_field_id, field, time=time, is_in=is_in, js_in=js_in, ks_in=ks_in, mask=mask, &
                               rmask=rmask, ie_in=ie_in, je_in=je_in, ke_in=ke_in, weight=weight, err_msg=err_msg)
  elseif (present(rmask)) then
    send_data_infra_3d = send_data(diag_field_id, field, time=time, is_in=is_in, js_in=js_in, ks_in=ks_in, &
                               rmask=rmask, ie_in=ie_in, je_in=je_in, ke_in=ke_in, weight=weight, err_msg=err_msg)
  else
    send_data_infra_3d = send_data(diag_field_id, field, time=time, is_in=is_in, js_in=js_in, ks_in=ks_in, &
                               ie_in=ie_in, je_in=je_in, ke_in=ke_in, weight=weight, err_msg=err_msg)
  endif

end function send_data_infra_3d
end program test_diag_manager_time
