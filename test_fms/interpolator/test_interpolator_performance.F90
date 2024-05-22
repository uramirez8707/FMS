program test
  use fms_mod, only: fms_init, fms_end
  use interpolator_mod, only: interpolator_init
  use time_manager_mod, only: JULIAN, set_calendar_type, time_type
  use mpp_mod, only: mpp_pe, mpp_root_pe, mpp_error, FATAL
  use constants_mod, only: deg_to_rad
  use mpp_domains_mod

  implicit none

  type(domain2d) :: domain
  integer :: nxd, nyd
  integer :: isd, ied, jsd, jed
  integer :: layout(2)
  real, dimension(:), allocatable :: latb_mod(:,:),lonb_mod(:,:),lon_mod(:),lat_mod(:)
  real, dimension(:,:,:), allocatable :: p_half, p_full
  integer :: level=33
  real :: p_bot,p_top,lambda
  real :: dx, dy
  integer :: i
  type(interpolate_type) :: climo_interp_type

  call fms_init()
  call set_calendar_type(JULIAN)

  nxd=360
  nyd=180
  layout = (/1,1/)
  call mpp_define_domains((/1,nxd,1,nyd/), layout, domain)
  call mpp_get_data_domain(domain,isd,ied,jsd,jed)

  allocate(lonb_mod(nxd+1,nyd+1),lon_mod(nxd))
  allocate(latb_mod(nxd+1,nyd+1),lat_mod(nyd))
  allocate(p_half(isd:ied,jsd:jed,level+1),p_full(isd:ied,jsd:jed,level))

  dx = 360./nxd
  dy = 180./nyd
  do i = 1,nxd+1
    lonb_mod(i,:) = (i-1)*dx
  enddo
  do i = 1,nyd+1
    latb_mod(:,i) = -90. + (i-1)*dy
  enddo
  do i=1,nxd
    lon_mod(i)=(lonb_mod(i+1,1)+lonb_mod(i,1))/2.0
  enddo
  do i=1,nyd
    lat_mod(i)=(latb_mod(1,i+1)+latb_mod(1,i))/2.0
  enddo
  lonb_mod = lonb_mod * deg_to_rad
  latb_mod = latb_mod * deg_to_rad

  p_top = 1.0
  p_bot = 101325.0 !Model level in Pa
  lambda = -1.0*log(p_top/p_bot)/(level+1)

  p_half(:,:,level+1) = p_bot
  do i=level,1,-1
    p_half(:,:,i)=p_half(:,:,i+1)*exp(-1.0*lambda)
  enddo
  do i=1,level
    p_full(:,:,i)=(p_half(:,:,i+1)+p_half(:,:,i))/2.0
  enddo

  call interpolator_init(climo_interp_type, "emissions.aircraft.aero.0.5x0.5.1849-2030.nc", lonb, latb)
  call fms_end()
end program