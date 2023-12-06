program test
use fms_mod, only: fms_init, fms_end, check_nml_error, string
use fms2_io_mod
use axis_utils2_mod
use constants_mod, only: DEG_TO_RAD, RAD_TO_DEG
use horiz_interp_mod
use mpp_mod

implicit none

type(FmsNetcdfFile_t) :: fileobj
character(len=90) :: filename = "INPUT/hadisst_sst.data.nc"
integer, parameter :: nlon = 360
integer, parameter :: nlat = 90
real :: lon(361)
real :: lat(181)
real :: sst_data(360,180)
type(horiz_interp_type) :: interp
real :: lon_out(nlon)
real :: lat_out(nlat)
real :: sst_data_out(nlon-1,nlat-1)
integer :: fort = 666
integer :: io, ierr
integer :: i, j

namelist /test_nml/ filename, fort

call fms_init()

read (input_nml_file, test_nml, iostat=io)
ierr = check_nml_error(io, 'test_nml')

print *, "Interpolating from the file:", trim(filename)
if (open_file(fileobj, trim(filename), "read")) then
  call axis_edges(fileobj, "lat", lat)
  call axis_edges(fileobj, "lon", lon)
  write(mpp_pe()+200, *) "LAT:", lat
  write(mpp_pe()+200, *) "LON:", lon
  call read_data(fileobj, "sst", sst_data, unlim_dim_level=1)
  call close_file(fileobj)
else
  call mpp_error(FATAL, "unable to open file")
endif

lon_out(1) = 0.0
do i = 2, nlon
  lon_out(i) = lon_out(i-1) + 1
enddo

lat_out(1) = -90.0
do i = 2, nlat
  lat_out(i) = lat_out(i-1) + 2
enddo

lat = lat * DEG_TO_RAD
lon = lon * DEG_TO_RAD
lon_out = lon_out*DEG_TO_RAD
lat_out = lat_out*DEG_TO_RAD

call horiz_interp_new(interp, lon, lat, lon_out, lat_out, interp_method="bilinear")
call horiz_interp(interp, sst_data, sst_data_out)

do i = 2, nlon
  do j = 2, nlat
    write(fort, *) "i=", string(i), " j=", string(j), " lat=", string(nint(lat_out(j-1) *RAD_TO_DEG)), ",", string(nint(lat_out(j)*RAD_TO_DEG)), &
      " lon=", string(nint(lon_out(i-1)*RAD_TO_DEG)), ",", string(nint(lon_out(i)*RAD_TO_DEG)), &
      " ::", string(sst_data_out(i-1, j-1))
  enddo
enddo

call fms_end()

end program test
