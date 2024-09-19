program test

  use fms_mod, only: fms_init, fms_end
  use mpp_mod
  use fms2_io_mod
  
  implicit none
  
  integer :: baseline
  integer :: one_read_broadcast
  integer :: pnetcdf
  type(FmsNetcdfFile_t) :: baseline_obj, one_read_broadcast_obj, pnetcdf_obj
  integer :: i
  integer, parameter :: nlat=360, nlon=720, np=25, ntimes=13
  real :: var1(nlon, nlat, np)
  integer, allocatable :: pes(:)
  integer :: mpp_communicator
  character(len=50) :: filename
  
  call fms_init()
  baseline = mpp_clock_id( 'Base line way of reading forcing files' )
  one_read_broadcast = mpp_clock_id( 'One PE reads and broadcasts way of reading forcing files' )
  pnetcdf = mpp_clock_id( 'Parallel netcdf way of reading files' )
  filename = "emissions_test_file.nc"

  allocate(pes(mpp_npes()))
  call mpp_get_current_pelist(pes)
  call mpp_declare_pelist(pes, name="all pes?", commID=mpp_communicator)

  ! METHOD 1
  call mpp_clock_begin(baseline)
  if (.not. open_file(baseline_obj, filename, "read")) &
    call mpp_error(FATAL, "Unable to open the file!")

  do i = 1, ntimes
    call read_data(baseline_obj, "fuel", var1, unlim_dim_level=i)
  enddo

  call close_file(baseline_obj)
  call mpp_clock_end(baseline)

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! METHOD 2
  call mpp_clock_begin(one_read_broadcast)
  if (.not. open_file(one_read_broadcast_obj, filename, "read", pelist=pes)) &
    call mpp_error(FATAL, "Unable to open the file!")

  do i = 1, ntimes
    call read_data(one_read_broadcast_obj, "fuel", var1, unlim_dim_level=i)
  enddo

  call close_file(one_read_broadcast_obj)
  call mpp_clock_end(one_read_broadcast)

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! METHOD 3
  call mpp_clock_begin(pnetcdf)
  pnetcdf_obj%use_collective = .true.
  pnetcdf_obj%tile_comm = mpp_communicator
  if (.not. open_file(pnetcdf_obj, filename, "read")) &
    call mpp_error(FATAL, "Unable to open the file!")

  do i = 1, ntimes
    call read_data(pnetcdf_obj, "fuel", var1, unlim_dim_level=i)
  enddo

  call close_file(pnetcdf_obj)
  call mpp_clock_end(pnetcdf)

  call fms_end()
  
  end program test