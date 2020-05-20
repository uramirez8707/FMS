program test_diag_manager

use   mpp_domains_mod
use   diag_manager_mod
use   fms_mod
use  time_manager_mod, only: time_type, set_calendar_type, set_date, NOLEAP, JULIAN, operator(+), set_time, print_time

implicit none

type(time_type)                   :: Time
integer, dimension(2)             :: layout = (/1,6/)
integer :: nlon, nlat, nz
type(domain2d)                    :: Domain
real, dimension(:), allocatable :: x, y, z
integer :: i, j
integer :: is, ie, js, je
real, allocatable, dimension(:,:,:) :: sst, ice, wut, huh, pop
integer :: id_x, id_y, id_z, id_sst, id_ice, id_wut, id_huh, id_pop
integer :: used

call fms_init
call diag_manager_init
call set_calendar_type(NOLEAP)

nlon = 360
nlat = 320
nz = 75

call mpp_domains_set_stack_size(17280000)
call mpp_define_domains( (/1,nlon,1,nlat/), layout, Domain, name='test_diag_manager')
call mpp_define_io_domain(Domain, (/1,1/))
call mpp_get_compute_domain(Domain, is, ie, js, je)

! Set up the data
allocate(x(nlon), y(nlat), z(nz))
allocate(sst(is:ie,js:je,1:nz), ice(is:ie,js:je,1:nz), wut(is:ie,js:je,1:nz), huh(is:ie,js:je,1:nz), pop(is:ie,js:je,1:nz))

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
wut = 555.55
huh = 444.44
pop = 333.33

! Set up the intial time
Time = set_date(2,1,1,0,0,0)

! Register the diags
id_x  = diag_axis_init('x',  x,  'point_E', 'x', long_name='point_E', Domain2=Domain)
id_y  = diag_axis_init('y',  y,  'point_N', 'y', long_name='point_N', Domain2=Domain)
id_z  = diag_axis_init('z',  z,  'point_Z', 'z', long_name='point_Z')
id_sst = register_diag_field  ('test_diag_manager_mod', 'sst', (/id_x,id_y,id_z/), Time, 'SST', 'K')
id_ice = register_diag_field  ('test_diag_manager_mod', 'ice', (/id_x,id_y,id_z/), Time, 'ICE', 'K')
id_pop = register_diag_field  ('test_diag_manager_mod', 'pop', (/id_x,id_y,id_z/), Time, 'POP', 'K') 
id_huh = register_diag_field  ('test_diag_manager_mod', 'huh', (/id_x,id_y,id_z/), Time, 'HUH', 'K')
id_wut = register_diag_field  ('test_diag_manager_mod', 'wut', (/id_x,id_y,id_z/), Time, 'WUT', 'K') 

! Send the axis data
used = send_data(id_x, x, Time)
used = send_data(id_y, y, Time)
used = send_data(id_z, z, Time)

! Increase the time and send data
do i=1,10
Time = set_date(2,1,i,0,0,0)
if(id_sst > 0) used = send_data(id_sst, sst, Time)
if(id_ice > 0) used = send_data(id_ice, ice, Time)
if(id_pop > 0) used = send_data(id_pop, pop, Time)
if(id_huh > 0) used = send_data(id_huh, huh, Time)
if(id_wut > 0) used = send_data(id_wut, wut, Time)
enddo

call diag_manager_end(Time)
call fms_end

end program test_diag_manager
