program test_parallel_netcdf_writes
  use   mpp_domains_mod
  use   mpp_mod
  use   fms2_io_mod,     only: open_file, register_axis, register_variable_attribute, close_file, &
                               FmsNetcdfDomainFile_t, write_data, register_field, read_data, &
                               parse_mask_table, unlimited, FMSnetcdfFile_t
  use   fms_mod,         only: fms_init, fms_end, check_nml_error
  use memutils_mod, only: print_memuse_stats
  use   platform_mod,    only: r4_kind, r8_kind, i4_kind, i8_kind
  use netcdf


  implicit none

  !< Namelist variables (configuration)
  integer, dimension(2) :: layout = (/1,6/)           !< Layout of the domain
  integer, dimension(2) :: io_layout = (/1,1/)        !< Io layout (currently overwritten to be the same as the
                                                      !! layout for reading, and 1,1 for writting)
  integer               :: nx = 96                    !< Size of the "x" dimension
  integer               :: ny = 96                    !< Size of the "y" dimension
  integer               :: nz = 65                    !< Size of the "z" dimension
  integer               :: ntimes = 2                 !< Number of time levels
  integer               :: test_case = 1              !< 1 use fms2io domain writes
                                                      !! 2 use netcdf collective writes

  character(len=10)     :: nc_format = "netcdf4"

  !< Domain stuff
  integer                               :: is, ie, js, je !< Starting and ending indices
  type(domain2d)                        :: Domain_write !< Domain of the data for when writing the file
                                                        !! (it uses an io_layout of 1,1)

  !< FMS2 io stuff
  character(len=6), dimension(4)        :: names        !< Dimension names for the dummy variables
  type(FmsNetcdfDomainFile_t)           :: fileobj             !< fms2io fileobj for domain decomposed
  type(FMSnetcdfFile_t)                 :: fileobj2

  !< Parallel netcdf stuff
  integer :: ncid
  integer :: x_dimid, y_dimid, z_dimid, t_dimid
  integer :: dimids(4)
  integer :: varid
  integer :: corners(4)
  integer :: edge_lengths (4)
  integer, allocatable :: pelist(:)
  logical :: is_root

  !< Clocks
  integer :: fms2_io_writes
  integer :: pnetcdf_writes

  !< Data
  real, allocatable, dimension(:,:,:)   :: sst_in         !< Data to be written
  real, allocatable, dimension(:,:,:)   :: sst_global         !< Data to be written
  integer                               :: i, j, k
  integer                               :: io_status    !< Status after reading the namelist

  namelist / test_parallel_netcdf_writes_nml / layout, io_layout, nx, ny, nz, ntimes, nc_format, test_case

  call fms_init()
  read (input_nml_file, test_parallel_netcdf_writes_nml, iostat=io_status)
  if (io_status > 0) call mpp_error(FATAL,'=>test_parallel_netcdf_writes_nml: Error reading input.nml')

  if (test_case .eq. 3) then
    ! Hack to test the io new mpp gather routines
    ! The io_layout is 1, Y  but only 1 file will be created

    ny = ny/ io_layout(2)
    layout(2) = layout(2) / io_layout(2)
    io_layout = (/ 1,1 /)
  endif

  ! Get all of the pes:
  allocate(pelist(mpp_npes()))
  call mpp_get_current_pelist(pelist)

  is_root = .false.
  if (mpp_pe() .eq. mpp_root_pe()) is_root = .true.

  ! Create a domain
  call mpp_domains_set_stack_size(17280000)
  call mpp_define_domains( (/1,nx,1,ny/), layout, Domain_write)
  call mpp_define_io_domain(Domain_write, io_layout) !< io_layout is only relevant for fms2_io
  call mpp_get_compute_domain(Domain_write, is, ie, js, je)

  ! Dummy data
  allocate(sst_in(is:ie, js:je, nz))
  do i = is, ie
    do j = js, je
      do k = 1, nz
        sst_in(i,j,k) = i*10000. + j + k/100.
      enddo
    enddo
  enddo

  names(1) = "lon"
  names(2) = "lat"
  names(3) = "level"
  names(4) = "time"

  call print_memuse_stats('Begin')
  fms2_io_writes = mpp_clock_id( 'WriteClock' )
  call mpp_clock_begin(fms2_io_writes)
  if (test_case .eq. 1) then
    if (open_file(fileobj, "test_domain_io.nc", "overwrite", Domain_write, nc_format=nc_format)) then
      call register_axis(fileobj, names(1), "x")
      call register_axis(fileobj, names(2), "y")
      call register_axis(fileobj, names(3), nz)
      call register_axis(fileobj, names(4), unlimited)

      call register_field(fileobj, "sst_3d", "double", names(1:4))
      do i = 1, ntimes
        call write_data(fileobj, "sst_3d", sst_in, unlim_dim_level = i)
      enddo

      call close_file(fileobj)
    else
      call mpp_error(FATAL, "Unable to open the file for writing")
    endif
  else if (test_case .eq. 2) then
    call check(nf90_create("test_parallel_netcdf.nc", IOR(NF90_NETCDF4, NF90_MPIIO), ncid, &
       comm = mpp_get_domain_tile_commid(Domain_write), info = MPP_INFO_NULL))

    ! Define axis
    call check(nf90_def_dim(ncid, "lon", nx, x_dimid))
    call check(nf90_def_dim(ncid, "lat", ny, y_dimid))
    call check(nf90_def_dim(ncid, "level", nz, z_dimid))
    call check(nf90_def_dim(ncid, "time", unlimited, t_dimid))

    dimids = (/x_dimid, y_dimid, z_dimid, t_dimid/)
    call check(nf90_def_var(ncid, "sst_3d", NF90_DOUBLE, dimids, varid))

    call check(nf90_enddef(ncid))

    corners = (/is, js, 1, 1/)
    edge_lengths = (/size(sst_in, 1), size(sst_in, 2), size(sst_in, 3), 1/)

    call check(nf90_var_par_access(ncid, varid, nf90_collective))
    do i = 1, ntimes
      corners(4) = i !< Update the unlimited dimension
      call check(nf90_put_var(ncid, varid, sst_in, start = corners, &
        count = edge_lengths))
    enddo
    call check(nf90_close(ncid))
  else

    ! Only the root pe is going to write data!
    if (mpp_pe() .eq. mpp_root_pe()) then
      if (open_file(fileobj2, "test_domain_mppgather.nc.0001", "overwrite")) then
        call register_axis(fileobj2, names(1), nx)
        call register_axis(fileobj2, names(2), ny)
        call register_axis(fileobj2, names(3), nz)
        call register_axis(fileobj2, names(4), unlimited)

        call register_field(fileobj2, "sst_3d", "double", names(1:4))
      else
        call mpp_error(FATAL, "Unable to open the file for writing")
      endif
    endif

    do i = 1, ntimes
      ! Root pe is going to collect all of the data
      if (mpp_pe() .eq. mpp_root_pe()) then
        ! Allocate a buffer big enough for all of the data
        allocate(sst_global(nx, ny, nz))
        sst_global = -999.999
      endif

      call mpp_gather(is, ie, js, je, nz, pelist, sst_in, sst_global, is_root)

      if (mpp_pe() .eq. mpp_root_pe()) then
        call write_data(fileobj2, "sst_3d", sst_global, unlim_dim_level = i)
        deallocate(sst_global)
      endif
    enddo

    if (mpp_pe() .eq. mpp_root_pe()) call close_file(fileobj2)
  endif
  call mpp_clock_end(fms2_io_writes)

  call mpp_sync()
  call print_memuse_stats('End')
  call fms_end()

  contains

  subroutine check(status)
    integer, intent ( in) :: status

    if(status /= nf90_noerr) then
      call mpp_error(FATAL, trim(nf90_strerror(status)))
    end if
  end subroutine check
end program test_parallel_netcdf_writes
