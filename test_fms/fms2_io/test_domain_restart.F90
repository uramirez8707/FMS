program test_domain_restart

use mpp_mod
use mpp_domains_mod
use fms2_io_mod
use platform_mod

implicit none

integer, dimension(2)                 :: layout = (/2,3/) !< Domain layout
integer                               :: nlon             !< Number of points in x axis
integer                               :: nlat             !< Number of points in y axis
type(domain2d)                        :: Domain           !< Domain with mask table

real(kind=r8_kind), allocatable, dimension(:,:,:) :: sst, sst_in     !< Data to be written
integer                               :: is               !< Starting x index
integer                               :: ie               !< Ending x index
integer                               :: js               !< Starting y index
integer                               :: je               !< Ending y index

character(len=6), dimension(3)        :: names            !< Dimensions names
type(FmsNetcdfDomainFile_t)           :: fileobj          !< fms2io fileobj for domain decomposed

integer :: ReadClock, WriteClock

call mpp_init
call mpp_domains_init
call fms2_io_init

ReadClock = mpp_clock_id( 'Reading' )
WriteClock= mpp_clock_id( 'Writing' )

nlon = 720
nlat = 720

!< Create a domain nlonXnlat with mask
call mpp_domains_set_stack_size(17280000)
call mpp_define_domains( (/1,nlon,1,nlat/), layout, Domain, name='test_domain_restart')
call mpp_define_io_domain(Domain, (/1,1/))
call mpp_get_compute_domain(Domain, is, ie, js, je)

allocate(sst(is:ie,js:je,50), sst_in(is:ie,js:je,50))
sst = real(7., kind=r8_kind)
sst_in = real(999., kind=r8_kind)

call mpp_clock_begin(WriteClock)
if (open_file(fileobj, "domain_restart.nc", "overwrite", domain, is_restart=.true.)) then
    !< Register the axis
    names(1) = "lon"
    names(2) = "lat"
    names(3) = "ht"

    call register_axis(fileobj, "lon", "x")
    call register_axis(fileobj, "lat", "y")
    call register_axis(fileobj, "ht", 50)

    !< Register the variable and Write out the data
    call register_restart_field(fileobj, "sst", sst, names(1:3))
    call write_restart(fileobj)

    !< Close the file
    call close_file(fileobj)
endif
call mpp_sync()
call mpp_clock_end(WriteClock)

call mpp_clock_begin(ReadClock)
if (open_file(fileobj, "domain_restart.nc", "read", domain, is_restart=.true.)) then
    !< Register the axis
    names(1) = "lon"
    names(2) = "lat"
    call register_axis(fileobj, "lon", "x")
    call register_axis(fileobj, "lat", "y")

    !< Register the variable and Write out the data
    call register_restart_field(fileobj, "sst", sst_in, names(1:3))
    call read_restart(fileobj)

    !< Close the file
    call close_file(fileobj)
endif
call mpp_sync()
call mpp_clock_end(ReadClock)

if (sum(sst) .ne. sum(sst_in)) call mpp_error(FATAL, "OH NO?")
deallocate(sst, sst_in)

call mpp_domains_exit()
call mpp_exit()

end program
