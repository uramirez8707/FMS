program test_performance
use fms_mod, only: fms_init, fms_end, check_nml_error
use mpp_domains_mod
use mpp_mod
use fms2_io_mod
use platform_mod

implicit none

integer, dimension(2)                 :: layout = (/2,3/) !< Domain layout
integer, dimension(2)                 :: io_layout = (/1,1/) !< Domain io layout
integer                               :: nlon=96          !< Number of points in x axis
integer                               :: nlat=96          !< Number of points in y axis
integer                               :: nz=33            !< Number of points in z axis
type(domain2d)                        :: Domain           !< Domain with mask table
real(kind=r8_kind), allocatable, dimension(:,:,:) :: sst  !< Data to be written
real(kind=r8_kind), allocatable, dimension(:,:,:) :: sst2 !< Data to be written
type(FmsNetcdfDomainFile_t)           :: fileobj          !< fms2io fileobj for domain decomposed
integer                               :: is               !< Starting x index
integer                               :: ie               !< Ending x index
integer                               :: js               !< Starting y index
integer                               :: je               !< Ending y index
character(len=6), dimension(2)        :: names            !< Dimensions names
integer :: newClock1
integer :: io
integer :: ierr

namelist /test_performance_nml/ layout, io_layout, nlon, nlat, nz

call fms_init

read(input_nml_file, nml=test_performance_nml, iostat=io)
ierr = check_nml_error(io, 'test_performance')

nlon = 96
nlat = 96
nz = 33

newClock1 = mpp_clock_id( 'Reading Time' )

call mpp_domains_set_stack_size(17280000)
call mpp_define_domains( (/1,nlon,1,nlat/), layout, Domain, name='test_performance')
call mpp_define_io_domain(Domain, io_layout)
call mpp_get_compute_domain(Domain, is, ie, js, je)

allocate(sst(is:ie,js:je,nz))
allocate(sst2(is:ie,js:je,nz))
sst = real(7., kind=r8_kind)
sst2 = real(-999.99, kind=r8_kind)

if (open_file(fileobj, "test_performance.nc", "overwrite", domain)) then
    !< Register the axis
    names(1) = "lon"
    names(2) = "lat"
    call register_axis(fileobj, "lon", "x")
    call register_axis(fileobj, "lat", "y")
    call register_axis(fileobj, "z", nz)

    !< Register the variable and Write out the data
    call register_field(fileobj, "sst", "double", names(1:2))
    call register_variable_attribute(fileobj, "sst", "_FillValue", real(999., kind=r8_kind))
    call write_data(fileobj, "sst", sst)

    !< Close the file
    call close_file(fileobj)
else
   call mpp_error(FATAL, "test_performance: error opening the file for writting")
endif

call mpp_sync()

call mpp_clock_begin(newClock1)
if (open_file(fileobj, "test_performance.nc", "read", domain)) then
    !< Register the axis
    names(1) = "lon"
    names(2) = "lat"
    call register_axis(fileobj, "lon", "x")
    call register_axis(fileobj, "lat", "y")

    !< Register the variable and Write out the data
    call register_field(fileobj, "sst", "double", names(1:2))
    call read_data(fileobj, "sst", sst)

    !< Close the file
    call close_file(fileobj)
else
   call mpp_error(FATAL, "test_performance: error opening the file for writting")
endif
call mpp_clock_end(newClock1)

call fms_end
end program