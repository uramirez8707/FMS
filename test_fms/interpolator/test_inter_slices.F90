program test_inter_slices

use interpolator_mod
use time_manager_mod
use mpp_memutils_mod
use fms_mod

type(interpolate_type) :: o3
integer :: nxd, nyd, dx, dy, level
integer :: i

real, dimension(:,:,:), allocatable :: model_data, p_half
real, dimension(:), allocatable :: latb_mod(:,:),lonb_mod(:,:)
type(time_type) :: Time
character(len=12) :: mssg
real :: lambda

call fms_init()
call set_calendar_type(noleap)

nxd = 90
nyd = 90
dx = 350./nxd
dy = 180./nyd
level = 18

allocate(lonb_mod(nxd+1,nyd+1))
allocate(latb_mod(nxd+1,nyd+1))
allocate(p_half(nxd,nyd,level+1))
allocate(model_data(nxd,nyd,level))

do i = 1,nxd+1
  lonb_mod(i,:) = (i-1)*dx
enddo
do i = 1,nyd+1
  latb_mod(:,i) = -90. + (i-1)*dy
enddo

lonb_mod = lonb_mod*3.14/180 !> close enough for me
latb_mod = latb_mod*3.14/180 !> close enough for me

lambda = -1.0*log(1.0/101325.0)/(level+1)

p_half = 101325.0
do i=level,1,-1
  p_half(:,:,i)=p_half(:,:,i+1)*exp(-1.0*lambda)
enddo

!> Initiliaze the interpolator type:
call mpp_memuse_begin()
call interpolator_init( o3, "o3.climatology.nc", lonb_mod, latb_mod, &
                        data_out_of_bounds=(/CONSTANT/))
call mpp_print_memuse_stats("After first call")

Time = set_date(1850,12,1,0,0,0)

do i = 1, 60
    print *, "interation:", i
    !> Increment the time by 1 month: original time, year, month, day, min, sec 
    Time = increment_date(Time, 0, 1, 0, 0, 0, 0)
    !> Read the interpolator slices
    call obtain_interpolator_time_slices (o3, Time)
    call mpp_print_memuse_stats("after obtain_interpolator_time_slices")

    !> Interpolate
    call interpolator( o3, Time, p_half, model_data, "ozone")

    !> Set the clim_type%separate_time_vary_calc to .false. so you get read the
    !times for the next loop
    call unset_interpolator_time_flag(o3)
enddo

!> Clean up?
call interpolator_end(o3)
deallocate(lonb_mod)
deallocate(latb_mod)
deallocate(p_half)
deallocate(model_data)

call fms_end()

end program test_inter_slices
