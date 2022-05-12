!***********************************************************************
!*                   GNU Lesser General Public License
!*
!* This file is part of the GFDL Flexible Modeling System (FMS).
!*
!* FMS is free software: you can redistribute it and/or modify it under
!* the terms of the GNU Lesser General Public License as published by
!* the Free Software Foundation, either version 3 of the License, or (at
!* your option) any later version.
!*
!* FMS is distributed in the hope that it will be useful, but WITHOUT
!* ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
!* FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
!* for more details.
!*
!* You should have received a copy of the GNU Lesser General Public
!* License along with FMS.  If not, see <http://www.gnu.org/licenses/>.
!***********************************************************************

!> @defgroup fms_diag_axis_object_mod fms_diag_axis_object_mod
!> @ingroup diag_manager
!! @brief fms_diag_axis_object_mod stores the diag axis object, a diag domain
!! object, and a subaxis object.

!> @file
!> @brief File for @ref diag_axis_object_mod

!> @addtogroup fms_diag_axis_object_mod
!> @{
module fms_diag_axis_object_mod
  use mpp_domains_mod, only:  domain1d, domain2d, domainUG, mpp_get_compute_domain, CENTER, &
                            & mpp_get_compute_domain
  use platform_mod,    only:  r8_kind, r4_kind
  use diag_data_mod,   only:  diag_atttype
  use mpp_mod,         only:  FATAL, mpp_error
  use fms_mod,         only: lowercase
  use fms2_io_mod,     only: FmsNetcdfFile_t, FmsNetcdfDomainFile_t, FmsNetcdfUnstructuredDomainFile_t, &
                            &register_axis
  implicit none

  PRIVATE

  public :: diagAxis_t, set_subaxis, modern_diag_axis_init, fms_diag_axis_object_init, fms_diag_axis_object_end
  public :: diagDomain_t, determine_the_domain_type, diagDomain2d_t, diagDomainUG_t, write_axis_metadata
  public :: axis_obj, get_axis_length
  !> @}

  !> @brief Type to hold the domain info for an axis
  !! This type was created to avoid having to send in "Domain", "Domain2", "DomainUG" as arguments into subroutines
  !! and instead only 1 class(diagDomain_t) argument can be send
  !> @ingroup diag_axis_object_mod
  type diagDomain_t
    contains
      procedure :: set => set_axis_domain
      procedure :: length => get_length
      procedure :: get_domain_type
  end type diagDomain_t

  !> @brief Type to hold the 1d domain
  type, extends(diagDomain_t) :: diagDomain1d_t
     type(domain1d) :: Domain !< 1d Domain of the axis
  end type

  !> @brief Type to hold the 2d domain
  type, extends(diagDomain_t) :: diagDomain2d_t
    type(domain2d) :: Domain2 !< 2d Domain of an "X" or "Y" axis
  end type

  !> @brief Type to hold the unstructured domain
  type, extends(diagDomain_t) :: diagDomainUg_t
    type(domainUG) :: DomainUG !< Domain of "U" axis
  end type

  !> @brief Type to hold the subaxis
  !> @ingroup diag_axis_object_mod
  TYPE subaxis_t
    CHARACTER(len=:), ALLOCATABLE :: subaxis_name   !< Name of the subaxis
    INTEGER                       :: starting_index !< Starting index of the subaxis relative to the parent axis
    INTEGER                       :: ending_index   !< Ending index of the subaxis relative to the parent axis
    class(*)        , ALLOCATABLE :: bounds         !< Bounds of the subaxis (lat/lon or indices)
    contains
      procedure :: exists => check_if_subaxis_exists
  END TYPE subaxis_t

  !> @brief Type to hold the diagnostic axis description.
  !> @ingroup diag_axis_object_mod
  TYPE diagAxis_t
     CHARACTER(len=:),   ALLOCATABLE, private :: axis_name       !< Name of the axis
     CHARACTER(len=:),   ALLOCATABLE, private :: units           !< Units of the axis
     CHARACTER(len=:),   ALLOCATABLE, private :: long_name       !< Long_name attribute of the axis
     CHARACTER(len=1)               , private :: cart_name       !< Cartesian name "X", "Y", "Z", "T", "U", "N"
     CLASS(*),           ALLOCATABLE, private :: axis_data(:)    !< Data of the axis
     !< TO DO this can be a dlinked to avoid having limits
     type(subaxis_t)                , private :: subaxis(3)      !< Array of subaxis
     integer                        , private :: nsubaxis        !< Number of subaxis
     class(diagDomain_t),ALLOCATABLE, private :: axis_domain     !< Domain
     character(len=2)               , private :: domain_type
     INTEGER                        , private :: length          !< Global axis length
     INTEGER                        , private :: direction       !< Direction of the axis 0, 1, -1
     INTEGER                        , private :: edges           !< Axis ID for the previously defined "edges axis"
     CHARACTER(len=128)             , private :: aux             !< Auxiliary name, can only be <TT>geolon_t</TT>
                                                                 !! or <TT>geolat_t</TT>
     CHARACTER(len=128)             , private :: req             !< Required field names.
     INTEGER                        , private :: tile_count      !< The number of tiles
     TYPE(diag_atttype),allocatable , private :: attributes(:)   !< Array to hold user definable attributes
     INTEGER                        , private :: num_attributes  !< Number of defined attibutes
     INTEGER                        , private :: domain_position !< The position in the doman (NORTH, EAST or CENTER)

     contains

     PROCEDURE :: register => diag_axis_init
     PROCEDURE :: get_axis_length
     PROCEDURE :: set_subaxis

     ! TO DO:
     PROCEDURE :: write_the_axis_metadata
     ! PROCEDURE :: write_axis_data
     ! PROCEDURE :: get_fileobj_type_needed (use the domain to figure out what fms2 fileobj to use)
     ! Get/has/is subroutines as needed
  END TYPE diagAxis_t

  integer                        :: number_of_axis !< Number of axis that has been registered
  type(diagAxis_t), ALLOCATABLE, SAVE  :: axis_obj(:)    !< Diag_axis objects
  logical                        :: module_is_initialized !< Flaf indicating if the module is initialized

  !> @addtogroup fms_diag_yaml_mod
  !> @{
  contains

  !!!!!!!!!!!!!!!!! DIAG AXIS PROCEDURES !!!!!!!!!!!!!!!!!
  !> @brief Initialize the axis
  subroutine diag_axis_init(obj, axis_name, axis_data, units, cart_name, long_name, direction,&
  & set_name, edges, Domain, Domain2, DomainU, aux, req, tile_count, domain_position )
    class(diagAxis_t),  INTENT(out)          :: obj             !< Diag_axis obj
    CHARACTER(len=*),   INTENT(in)           :: axis_name       !< Name of the axis
    class(*),           INTENT(in)           :: axis_data(:)    !< Array of coordinate values
    CHARACTER(len=*),   INTENT(in)           :: units           !< Units for the axis
    CHARACTER(len=1),   INTENT(in)           :: cart_name       !< Cartesian axis ("X", "Y", "Z", "T", "U", "N")
    CHARACTER(len=*),   INTENT(in), OPTIONAL :: long_name       !< Long name for the axis.
    CHARACTER(len=*),   INTENT(in), OPTIONAL :: set_name        !< Name of the parent axis, if it is a subaxis
    INTEGER,            INTENT(in), OPTIONAL :: direction       !< Indicates the direction of the axis
    INTEGER,            INTENT(in), OPTIONAL :: edges           !< Axis ID for the previously defined "edges axis"
    TYPE(domain1d),     INTENT(in), OPTIONAL :: Domain          !< 1D domain
    TYPE(domain2d),     INTENT(in), OPTIONAL :: Domain2         !< 2D domain
    TYPE(domainUG),     INTENT(in), OPTIONAL :: DomainU         !< Unstructured domain
    CHARACTER(len=*),   INTENT(in), OPTIONAL :: aux             !< Auxiliary name, can only be <TT>geolon_t</TT>
                                                                !! or <TT>geolat_t</TT>
    CHARACTER(len=*),   INTENT(in), OPTIONAL :: req             !< Required field names.
    INTEGER,            INTENT(in), OPTIONAL :: tile_count      !< Number of tiles
    INTEGER,            INTENT(in), OPTIONAL :: domain_position !< Domain position, "NORTH" or "EAST"

    obj%axis_name = trim(axis_name)
    obj%units = trim(units)
    obj%cart_name = trim(cart_name) !< TO DO Check for valid cart_names
    if (present(long_name)) obj%long_name = trim(long_name)

    select type (axis_data)
    type is (real(kind=r8_kind))
      allocate(real(kind=r8_kind) :: obj%axis_data(size(axis_data)))
      obj%axis_data = axis_data
    type is (real(kind=r4_kind))
      allocate(real(kind=r4_kind) :: obj%axis_data(size(axis_data)))
      obj%axis_data = axis_data
    class default
      call mpp_error(FATAL, "The axis_data in your diag_axis_init call is not a supported type. &
                          &  Currently only r4 and r8 data is supported.")
    end select

    !< TO DO check the presence of multiple Domains
    obj%domain_type = "ND"
    if (present(Domain)) then
      allocate(diagDomain1d_t :: obj%axis_domain)
      call obj%axis_domain%set(Domain=Domain)
    else if (present(Domain2)) then
      allocate(diagDomain2d_t :: obj%axis_domain)
      call obj%axis_domain%set(Domain2=Domain2)
      obj%domain_type = "2D"
    else if (present(DomainU)) then
      allocate(diagDomainUg_t :: obj%axis_domain)
      call obj%axis_domain%set(DomainU=DomainU)
      obj%domain_type = "UG"
    endif

    obj%tile_count = 1
    if (present(tile_count)) obj%tile_count = tile_count

    !< TO DO Check for valid domain_position
    obj%domain_position = CENTER
    if (present(domain_position)) obj%domain_position = domain_position

    obj%length = size(axis_data)

    !< TO DO Check for valid direction
    obj%direction = 0
    if (present(direction)) obj%direction = direction

    !< TO DO Check if id is valid and with the same parameters
    obj%edges = 0
    if (present(edges)) obj%edges = edges

    if (present(aux)) obj%aux = trim(aux)
    if (present(req)) obj%req = trim(req)

    obj%nsubaxis = 0
  end subroutine diag_axis_init

  !> @brief Get the length of the axis
  !> @return axis length
  function get_axis_length(obj) &
  result (axis_length)
    class(diagAxis_t), intent(inout) :: obj !< diag_axis obj
    integer                           :: axis_length

    !< If the axis is domain decomposed axis_length will be set to the length for the current PE:
    if (allocated(obj%axis_domain)) then
      axis_length = obj%axis_domain%length(obj%cart_name, obj%domain_position, obj%length)
    else
      axis_length = obj%length
    endif

  end function

  !> @brief Set the subaxis of the axis obj
  subroutine set_subaxis(obj, bounds)
    class(diagAxis_t), INTENT(INOUT) :: obj       !< diag_axis obj
    class(*),           INTENT(INOUT) :: bounds(:) !< bound of the subaxis

    integer :: i !< For do loops

    !< Check if the subaxis for this bouds already exists
    do i = 1, obj%nsubaxis
      if (obj%subaxis(i)%exists(bounds)) return
    enddo

    !< TO DO: everything
    obj%nsubaxis = obj%nsubaxis + 1
  end subroutine

  !!!!!!!!!!!!!!!!!! SUB AXIS PROCEDURES !!!!!!!!!!!!!!!!!
  !> @brief Check if a subaxis was already defined
  !> @return Flag indicating if a subaxis is already defined
  function check_if_subaxis_exists(obj,bounds) &
  result(exists)
    class(subaxis_t), INTENT(INOUT) :: obj       !< diag_axis obj
    class(*),         INTENT(IN)    :: bounds(:) !< bounds of the subaxis
    logical                         :: exists

    !< TO DO: compare bounds
    exists = .false.
  end function

  !> @brief Get the length of a 2D domain
  !> @return Length of the 2D domain
  function get_length(obj, cart_axis, domain_position, global_length) &
  result (length)
    class(diagDomain_t), INTENT(INOUT) :: obj       !< diag_axis obj
    character(len=*),    INTENT(IN)    :: cart_axis !< cart_axis of the axis
    integer,             INTENT(IN)    :: domain_position !< Domain position (CENTER, NORTH, EAST)
    integer,             INTENT(IN)    :: global_length !< global_length of the axis

    integer :: length

    select type (obj)
    type is(diagDomain2d_t)
      if (lowercase(trim(cart_axis)) == "x") call mpp_get_compute_domain(obj%Domain2, xsize=length, position=domain_position)
      if (lowercase(trim(cart_axis)) == "y") call mpp_get_compute_domain(obj%Domain2, ysize=length, position=domain_position)
    class default
      !< If domain is 1D or UG, just set it to the global length
      length = global_length
    end select
  end function

  !!!!!!!!!!!!!!!!! FMS_DOMAIN PROCEDURES !!!!!!!!!!!!!!!!!

  !> @brief Set the axis domain
  subroutine set_axis_domain(obj, Domain, Domain2, DomainU)
    class(diagDomain_t) :: obj !< fms_domain obj
    TYPE(domain1d),     INTENT(in),  OPTIONAL :: Domain  !< 1d domain
    TYPE(domain2d),     INTENT(in),  OPTIONAL :: Domain2 !< 2d domain
    TYPE(domainUG),     INTENT(in),  OPTIONAL :: DomainU !< Unstructured domain

    select type(obj)
    type is (diagDomain1d_t)
      obj%Domain = Domain
    type is (diagDomain2d_t)
      obj%Domain2 = Domain2
    type is (diagDomainUg_t)
      obj%DomainUG = DomainU
    end select
  end subroutine set_axis_domain

  function get_domain_type(obj) &
  result(domain_type)
    class(diagDomain_t) :: obj !< fms_domain obj
    character(len=2) :: domain_type

    domain_type = "ND"
    select type(obj)
    type is (diagDomain2d_t)
      domain_type = "2D"
    type is (diagDomainUg_t)
      domain_type = "UG"
    class default
      domain_type = "ND"
    end select
  end function get_domain_type

  subroutine fms_diag_axis_object_init()

    if (module_is_initialized) return

    number_of_axis = 0
    allocate(axis_obj(20))

    module_is_initialized = .true.
  end subroutine fms_diag_axis_object_init

  subroutine fms_diag_axis_object_end()
    deallocate(axis_obj)

    module_is_initialized = .false.
  end subroutine fms_diag_axis_object_end

  !> @brief Wrapper for the register_diag_axis subroutine. This is needed to keep the diag_axis_init
  !! interface the same
  !> @return Axis id
  FUNCTION modern_diag_axis_init(axis_name, axis_data, units, cart_name, long_name, direction,&
    & set_name, edges, Domain, Domain2, DomainU, aux, req, tile_count, domain_position ) &
    & result(id)

    CHARACTER(len=*),   INTENT(in)           :: axis_name       !< Name of the axis
    REAL,               INTENT(in)           :: axis_data(:)    !< Array of coordinate values
    CHARACTER(len=*),   INTENT(in)           :: units           !< Units for the axis
    CHARACTER(len=1),   INTENT(in)           :: cart_name       !< Cartesian axis ("X", "Y", "Z", "T", "U", "N")
    CHARACTER(len=*),   INTENT(in), OPTIONAL :: long_name       !< Long name for the axis.
    CHARACTER(len=*),   INTENT(in), OPTIONAL :: set_name        !< Name of the parent axis, if it is a subaxis
    INTEGER,            INTENT(in), OPTIONAL :: direction       !< Indicates the direction of the axis
    INTEGER,            INTENT(in), OPTIONAL :: edges           !< Axis ID for the previously defined "edges axis"
    TYPE(domain1d),     INTENT(in), OPTIONAL :: Domain          !< 1D domain
    TYPE(domain2d),     INTENT(in), OPTIONAL :: Domain2         !< 2D domain
    TYPE(domainUG),     INTENT(in), OPTIONAL :: DomainU         !< Unstructured domain
    CHARACTER(len=*),   INTENT(in), OPTIONAL :: aux             !< Auxiliary name, can only be <TT>geolon_t</TT>
                                                                !! or <TT>geolat_t</TT>
    CHARACTER(len=*),   INTENT(in), OPTIONAL :: req             !< Required field names.
    INTEGER,            INTENT(in), OPTIONAL :: tile_count      !< Number of tiles
    INTEGER,            INTENT(in), OPTIONAL :: domain_position !< Domain position, "NORTH" or "EAST"
    integer :: id

    number_of_axis = number_of_axis + 1

    call axis_obj(number_of_axis)%register(axis_name, axis_data, units, cart_name, long_name=long_name, &
    & direction=direction, set_name=set_name, edges=edges, Domain=Domain, Domain2=Domain2, DomainU=DomainU, aux=aux, &
    & req=req, tile_count=tile_count, domain_position=domain_position)

    id = number_of_axis
  end function

  subroutine determine_the_domain_type(axes, domain_type, var_domain)
    integer :: axes(:)
    character(len=2) :: domain_type
    CLASS(diagDomain_t), ALLOCATABLE :: var_domain

    integer :: i
    domain_type = "ND"
    do i = 1, size(axes)
      if (.not. allocated(axis_obj(axes(i))%axis_domain)) cycle
      if (domain_type .ne. axis_obj(axes(i))%axis_domain%get_domain_type()) then
        if (domain_type .eq. "ND") then
          var_domain = axis_obj(axes(i))%axis_domain
          domain_type = axis_obj(axes(i))%axis_domain%get_domain_type()
        else
          call mpp_error(FATAL, "No GOOD")
        endif
      endif
    end do

  end subroutine determine_the_domain_type

  subroutine write_axis_metadata(fileobj, axes_id)
    class(FmsNetcdfFile_t) :: fileobj
    integer :: axes_id(:)

    integer :: i

    print *, "Working on these axis:", axes_id
    do i = 1, size(axes_id)
      call axis_obj(axes_id(i))%write_the_axis_metadata(fileobj)
    end do
  end subroutine

  subroutine write_the_axis_metadata(obj, fileobj)
    class(diagAxis_t), INTENT(INOUT) :: obj       !< diag_axis obj
    class(FmsNetcdfFile_t) :: fileobj

    print *, "the domain type is:", obj%domain_type
    select type (fileobj)
      type is (FmsNetcdfFile_t)
        call register_axis(fileobj, obj%axis_name, obj%length)
      type is (FmsNetcdfDomainFile_t)
        print *, "I am here"
        select case (obj%domain_type)
        case ("ND")
          call register_axis(fileobj, obj%axis_name, obj%length)
        case ("2D")
          call register_axis(fileobj, obj%axis_name, obj%cart_name, domain_position=obj%domain_position)
        end select
      type is (FmsNetcdfUnstructuredDomainFile_t)
        select case (obj%domain_type)
        case ("ND")
          call register_axis(fileobj, obj%axis_name, obj%length)
        case ("UG")
          call register_axis(fileobj, obj%axis_name)
        end select
    end select
  end subroutine
end module fms_diag_axis_object_mod
!> @}
! close documentation grouping
