program test
  use fms_mod, only: fms_init, fms_end, string, check_nml_error
  use platform_mod
  use mpp_mod
  use mpp_domains_mod
  use offloading_io_mod
  use fms2_io_mod 
  use memutils_mod


  implicit none
  integer :: nmodel_pes
  logical :: is_root_pe
  integer, allocatable :: full_pes(:)
  integer, allocatable :: model_pes(:)
  integer, allocatable :: offload_pes(:)
  integer :: i
  logical :: is_model_pe
  logical :: is_offload_pe
  character(len=30) :: filename
  type(domain2D) :: model_domain
  real(kind=r4_kind), allocatable :: var_r4(:,:,:)
  real(kind=r4_kind), allocatable :: var_r4_read(:,:,:)
  type(FmsNetcdfDomainFile_t) :: fileobj
  type(FmsNetcdfDomainFile_t) :: fileobj_read
  integer, parameter :: ntiles = 6

  integer :: timesteps = 1
  integer :: nx = 96
  integer :: ny = 96
  integer :: nz = 65
  integer :: layout(2) = (/1,1/)
  integer :: noffload_pes = 6
  integer :: wut(10)

  namelist /io_offloading_driver_nml/ nx, ny, nz, layout, noffload_pes, timesteps

  ! call TAU_CREATE_REGION("Initial Set up", wut(1))
  ! call TAU_CREATE_REGION("Open file", wut(2))
  ! call TAU_CREATE_REGION("Register Axis", wut(3))
  ! call TAU_CREATE_REGION("Register Field", wut(4))
  ! call TAU_CREATE_REGION("Writing Data", wut(5))
  ! call TAU_CREATE_REGION("Closing File", wut(6))
  ! call TAU_CREATE_REGION("End", wut(7))

  ! call TAU_START_REGION(wut(1))
  call fms_init()
  call offloading_io_init()

  nmodel_pes = layout(1) * layout(2) * ntiles
  if (mpp_npes() .ne. nmodel_pes + noffload_pes) &
    call mpp_error(FATAL, "The total number of PEs "//string(mpp_npes())//&
      " is not equal to model_pes + noffload_pes")

  if (mod(noffload_pes, ntiles) .ne. 0) then
    call mpp_error(FATAL, "The number of offload pes "//string(noffload_pes)//&
      " must be envenly divisible by the number of tiles "//string(ntiles))
  endif

  is_root_pe = mpp_pe() .eq. mpp_root_pe()
  ! Set up a pelist with all of the model pes + offloading pes
  allocate(full_pes(mpp_npes()))
  call mpp_get_current_pelist(full_pes)

  ! Set up a pelist with all of the model pes
  allocate(model_pes(nmodel_pes))
  model_pes(1) = 0
  do i = 2, nmodel_pes
    model_pes(i) = model_pes(i-1) + 1
  enddo
  if (is_root_pe) print *, "Model PEs:", model_pes
  call mpp_declare_pelist(model_pes, "model_pes")

  ! Set up a pelist with all of the offloading pes
  allocate(offload_pes(noffload_pes))
  offload_pes(1) = model_pes(nmodel_pes) + 1
  do i = 2, noffload_pes
    offload_pes(i) = offload_pes(i-1) + 1
  enddo
  if (is_root_pe) print *, "Offload PEs:", offload_pes
  call mpp_declare_pelist(offload_pes, "offload_pes")

  is_model_pe = .false.
  is_offload_pe = .false.
  if (any(model_pes .eq. mpp_pe())) is_model_pe = .true.
  if (any(offload_pes .eq. mpp_pe())) is_offload_pe = .true.

  ! The model pes create the domain, allocate the data, and have the filename assigned
  if (is_model_pe) then
    call mpp_set_current_pelist( model_pes)
    model_domain = create_cubic_domain(nx, ny, 6, (/1, 1/), &
      nhalos=0, layout=layout)

    filename = "offload_output.nc"
    var_r4 = create_dummy_data(model_domain)
    var_r4_read = create_dummy_data(model_domain)
    var_r4_read = -999.999
  endif

  ! All of the pes need to be involved in the offloading calls
  call mpp_set_current_pelist(full_pes)

  call mpp_sync()
  write(mpp_pe() + 100, *) "BEGIN"

  call open_file_offload(fileobj, filename, &
    model_domain, &
    model_pes, offload_pes)

  write(mpp_pe() + 100, *) "FINISHED OPENING THE FILE"

  write(mpp_pe() + 100, *) ""
  call register_axis_offload(fileobj, "lon", "x")
  write(mpp_pe() + 100, *) "--"
  call register_axis_offload(fileobj, "lat", "y")
  write(mpp_pe() + 100, *) "--"
  call register_axis_offload(fileobj, "level", nz)
  write(mpp_pe() + 100, *) "--"
  call register_axis_offload(fileobj, "time", unlimited)
  write(mpp_pe() + 100, *) "--"

  write(mpp_pe() + 100, *) ""
  call register_field_offload(fileobj, "daily_3d", "double", (/"lon", "lat", "level", "time"/))
  write(mpp_pe() + 100, *) ""

   do i = 1, timesteps
     write(mpp_pe() + 100, *) "--> writing timestep i=", i
     call write_data_offload(fileobj, "daily_3d", var_r4, unlim_dim_level=i)
     write(mpp_pe() + 100, *) "--"
   enddo

   write(mpp_pe() + 100, *) ""
   write(mpp_pe() + 100, *) "Closing the file"
   call close_file_offload(fileobj)

   if (is_root_pe) print *, "File has been created"


!   ! Make sure the data was written as expected
!   if (is_model_pe) then
!     call mpp_set_current_pelist(model_pes)
!     if (is_root_pe) print *, "Reading the file back"
!     if (open_file(fileobj_read, filename, "read", model_domain)) then
!         call register_axis(fileobj_read, "lon", "x")
!         call register_axis(fileobj_read, "lat", "y")
!         call read_data(fileobj_read, 'daily_3d', var_r4_read)

!         if (sum(var_r4_read) .ne. sum(var_r4)) then
!             call mpp_error(FATAL, "The data does not match")
!         endif
!         call close_file(fileobj_read)
!     else
!         call mpp_error(FATAL, "Unable to open file")
!     endif
!   endif

  call mpp_set_current_pelist(full_pes)
  call fms_end()

  contains
  
  function create_dummy_data(domain) &
    result(dummy_data)
  
    type(domain2D), intent(in) :: domain
    real(kind=r4_kind), allocatable :: dummy_data(:,:,:)

    integer :: is !< Starting x index
    integer :: ie !< Ending x index
    integer :: js !< Starting y index
    integer :: je !< Ending y index

    integer :: j, k

    !Allocate the data to the size of the data domain but only fill the compute domain with data
    call mpp_get_data_domain(domain, is, ie, js, je)
    allocate(dummy_data(is:ie, js:je, nz))
    dummy_data = -999_r4_kind

    call mpp_get_compute_domain(domain, is, ie, js, je)
    do j = is, ie
      do k = js, je
        do i = 1, nz
          dummy_data(j, k, i) = real(j, kind=r4_kind)* 100_r4_kind + &
            real(k, kind=r4_kind) + real(i, kind=r4_kind)/100_r4_kind
        enddo
      enddo
    enddo
  end function

end program