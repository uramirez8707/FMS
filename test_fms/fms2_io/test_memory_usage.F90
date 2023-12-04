program test

  use mpp_mod, only: FATAL, mpp_error, input_nml_file
  use memutils_mod
  use fms_mod, only: fms_init, fms_end, check_nml_error, string
  use netcdf
  use platform_mod

  implicit none

  integer, parameter :: ntimes = 2
  integer :: ncid
  integer :: ierr, io
  character(len=255) :: filename = "transitions.nc"
  real(kind=r4_kind) :: var(1440,720)
  character(len=120) :: var_name
  integer :: ndims
  integer :: i, j, nvars

  namelist /test_memory_nml/ filename

  call fms_init()

  read(input_nml_file, nml=test_memory_nml, iostat=io)
  ierr = check_nml_error(io, 'test_memory_nml')

  call print_memuse_stats('Opening File:'//trim(filename))
  call check(nf90_open(filename, nf90_nowrite, ncid))

  call check(nf90_inquire(ncid, nVariables=nvars))
  do j = 1, ntimes
    do i = 1, nvars
      call check(nf90_inquire_variable(ncid, i, name=var_name, ndims=ndims))
      if (ndims < 3) cycle
      call check(nf90_get_var(ncid,i,var, start=(/1, 1, 100 + ntimes/), count=(/1440,720,1/)))
      call print_memuse_stats('After Reading:'//trim(var_name)//" at t="//string(j))
    enddo
  enddo

  call check(nf90_close(ncid))
  call print_memuse_stats('Closing File')
  call fms_end()

contains
subroutine check(err)
  integer, intent(in) :: err
  character(len=80) :: buf

  if (err .ne. nf90_noerr) then
    buf = nf90_strerror(err)
    call mpp_error(FATAL, buf)
    stop
  endif
end subroutine

end program