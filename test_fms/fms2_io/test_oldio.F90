program test_domain_restart

use mpp_mod
use mpp_domains_mod
use fms_io_mod
use platform_mod

implicit none

integer, dimension(2)                 :: layout = (/2,3/) !< Domain layout
integer                               :: nlon             !< Number of points in x axis
integer                               :: nlat             !< Number of points in y axis
type(domain2d)                        :: Domain           !< Domain with mask table

real(kind=r8_kind) :: sum_in, sum_out
real(kind=r8_kind), allocatable, dimension(:,:,:) :: sst     !< Data to be written
integer                               :: is               !< Starting x index
integer                               :: ie               !< Ending x index
integer                               :: js               !< Starting y index
integer                               :: je               !< Ending y index
integer :: id_restart
type(restart_file_type)           :: fileobj          !< fms2io fileobj for domain decomposed

integer :: ReadClock, WriteClock

call mpp_init
call mpp_domains_init
call fms_io_init

ReadClock = mpp_clock_id( 'Reading' )
WriteClock= mpp_clock_id( 'Writing' )

nlon = 720
nlat = 720

!< Create a domain nlonXnlat with mask
call mpp_domains_set_stack_size(17280000)
call mpp_define_domains( (/1,nlon,1,nlat/), layout, Domain, name='test_domain_restart')
call mpp_define_io_domain(Domain, (/1,1/))
call mpp_get_compute_domain(Domain, is, ie, js, je)

allocate(sst(is:ie,js:je,50))
sst = real(7., kind=r8_kind)

call mpp_clock_begin(WriteClock)
id_restart=register_restart_field(fileobj, "oldio.nc", "sst", sst, domain=domain)
call save_restart(fileobj)
call mpp_sync()
call mpp_clock_end(WriteClock)

sum_in = sum(sst)

sst = real(999., kind=r8_kind)
call mpp_clock_begin(ReadClock)
call restore_state(fileobj, directory="RESTART/")
call mpp_sync()
call mpp_clock_end(ReadClock)

sum_out = sum(sst)

if (sum_out .ne. sum_in) call mpp_error(FATAL, "OH NO?")
deallocate(sst)

call mpp_domains_exit()
call mpp_exit()

end program
