program test

use fms_mod, only: fms_init, fms_end
use fms2_io_mod
use mpp_mod

implicit none

type(FmsNetcdfFile_t) :: fileobj  !< fms2io netcd file obj
integer, dimension(:), allocatable :: global_pes !> Current pelist
integer, dimension(:), allocatable :: pes !> Current pelist
real :: vdata(720,360,25)
integer :: ngroup = 3

namelist /interpolator_read_nml/ ngroup

call fms_init

allocate(global_pes(mpp_npes()))
call mpp_get_current_pelist(global_pes)

pes = mpp_set_pes_group(global_pes, ngroup)

if (open_file(fileobj, "emissions_test_file.nc", "read", pelist=pes)) then
  call read_data(fileobj, "fuel", vdata, unlim_dim_level=2016)
  call close_file(fileobj)
else 
  call mpp_error(FATAL, "Error opening the file to read")
endif

call fms_end

contains
  function mpp_set_pes_group(gpelist, n) &
  result(pelist)
    integer, intent(in) :: gpelist(:)
    integer, intent(in) :: n

    integer, allocatable :: pelist(:)
    integer :: i, pe_begin, pe_end

    if (mod(size(gpelist), n) .ne. 0) call mpp_error(FATAL, "The global pelist is not divisible by ngroup")
    allocate(pelist(int(size(gpelist)/n)))

    do i = 1, n
        pe_begin = int(size(gpelist)/n) * (i-1)
        pe_end = pe_begin + int(size(gpelist)/n) -1
        if (mpp_pe() .ge. pe_begin .and. mpp_pe() .le. pe_end) then
            pelist=gpelist(pe_begin+1:pe_end+1)
            return
        endif
    enddo

  end function
end program test