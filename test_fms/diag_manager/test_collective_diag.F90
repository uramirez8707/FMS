program test_collective_diag_fields
  use   mpp_domains_mod
  use   diag_manager_mod
  use   fms_mod
  use  time_manager_mod, only: time_type, set_calendar_type, set_date, NOLEAP, JULIAN, operator(+), set_time, print_time

  implicit none

  type(time_type)                   :: Time             !< Time of the simulation
  type(time_type)                   :: Time_step        !< Time of the simulation
  type(domain2d)                    :: Domain           !< Domain of the data
  real, dimension(:), allocatable   :: x, y, z          !< Dummy data for the x, y, and z axis
  integer                           :: i, j             !< For looping indicing
  integer                           :: is, ie, js, je   !< Starting and Ending indices for the x and y dimensions
  real, allocatable                 :: var1(:,:,:)      !< Dummy data for var1
  integer                           :: id_x, id_y, id_z !< Ids of the x, y and z axis
  integer                           :: id_static        !< Id for the static variable
  integer, allocatable              :: id_var(:)        !< Id for all the time-dependent variables
  logical                           :: used             !< Set to .True. if the send data call was successful
  integer                           :: io_status        !< Error code when reading the namelist

  !! Namelist variables
  integer, dimension(2)             :: layout = (/1,2/) !< Layout to use
  integer                           :: nx = 20          !< Number of points in the x axis
  integer                           :: ny = 30          !< Number of points in the y axis
  integer                           :: nz = 5           !< Number of points in the z axis
  integer                           :: nt = 24          !< Number of hours to run the simulation
  integer                           :: nvariables = 1   !< Number of time-dependent variables to run with
  
  namelist / test_collective_diag_fields_nml / nx, ny, nz, nt, layout, nvariables

  call fms_init
  call set_calendar_type(JULIAN)
  call diag_manager_init

  ! Read in the namelist
  read (input_nml_file, test_collective_diag_fields_nml, iostat=io_status)
  if (io_status > 0) call mpp_error(FATAL,'=>test_modern_diag: Error reading input.nml')

  ! Define the Domain to use
  call mpp_domains_set_stack_size(17280000)
  call mpp_define_domains( (/1,nx,1,ny/), layout, Domain, name='test_diag_manager')
  call mpp_define_io_domain(Domain, (/1,1/))
  call mpp_get_compute_domain(Domain, is, ie, js, je)

  ! Set up dummy data
  allocate(x(nx), y(ny), z(nz))
  allocate(var1(is:ie,js:je,1:nz))

  x = [(i, i=1,nx)]
  y = [(i, i=1,ny)]
  z = [(i, i=1,nz)]

  ! Set up the time variables
  Time = set_date(2,1,1,0,0,0)
  Time_step = set_time (3600,0)
  call diag_manager_set_time_end(set_date(2,1,2,0,0,0))

  ! Register the axis
  id_x  = diag_axis_init('x',  x,  'point_E', 'x', long_name='point_E', Domain2=Domain)
  id_y  = diag_axis_init('y',  y,  'point_N', 'y', long_name='point_N', Domain2=Domain)
  id_z  = diag_axis_init('z',  z,  'point_Z', 'z', long_name='point_Z')

  ! Register the variables
  id_static = register_static_field ('atm_mod', 'var_static', (/id_x, id_y/), "Dummy static variable", "none")
  allocate(id_var(nvariables))
  do i = 1, nvariables
    id_var(i) = register_diag_field   ('atm_mod', 'var'//string(i), (/id_x, id_y/), Time, &
                                       'Dummy variable #'//string(i), 'none')
  enddo

  var1 = 5.43
  used = send_data(id_static, var1(:,:,1), Time)
  do i = 1, nt
    Time = Time + Time_step
    do j =  1, nvariables
      used = send_data(id_var(j), var1(:,:,1), Time)
    enddo
    call diag_send_complete(Time)
  enddo

  call diag_manager_end(Time)
  call fms_end
end program test_collective_diag_fields
