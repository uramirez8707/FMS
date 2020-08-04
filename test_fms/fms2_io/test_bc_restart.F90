program test_bc_restart

use   mpp_mod
use   fms2_io_mod
use   mpp_domains_mod
use   mpi
use   netcdf

implicit none

type atm_type
   type(FmsNetcdfFile_t)              :: fileobj_sw       !< fms2io netcdf file obj for south west bc
   type(FmsNetcdfFile_t)              :: fileobj_ne       !< fms2io netcdf file obj for north east bc
   logical                            :: BCfile_sw_open   !< flag indicating if the sourth west file is
                                                          !! opened
   logical                            :: BCfile_ne_open   !< flag indicating if the sourth west file is
                                                          !! opened
   type(domain2d)                     :: Domain           !< Domain with halos
end type

integer, dimension(2)                 :: layout = (/4,4/) !< Domain layout
integer                               :: nlon             !< Number of points in x axis
integer                               :: nlat             !< Number of points in y axis
integer                               :: isd, jsd         !< Starting x/y index (data_domain)
integer                               :: ied, jed         !< Ending x/y index (data_domain)
real, allocatable, dimension(:,:)     :: sst     !< Data to be written
real, allocatable, dimension(:,:)     :: sst_in     !< Data to be written
integer, allocatable, dimension(:)    :: all_pelist
integer   :: err, ncid, n
type(atm_type)                        :: atm

call mpp_init
call fms2_io_init

nlon = 144
nlat = 144

call mpp_define_domains( (/1,nlon,1,nlat/), layout, atm%Domain, xhalo=3, yhalo=3, symmetry=.true., name='test_bc_restart')
call mpp_get_data_domain(atm%domain, isd, ied, jsd, jed )

allocate(sst(isd:ied,jsd:jed))
sst = real(mpp_pe())

!< Create array of current pelist
allocate  (all_pelist(layout(1)*layout(2)))
do n = 1, layout(1)*layout(2)
   all_pelist(n) = n - 1
end do

atm%BCfile_sw_open = open_file(atm%fileobj_sw, "BCfile_sw.nc", "overwrite", is_restart=.true., pelist=all_pelist)
atm%BCfile_ne_open = open_file(atm%fileobj_ne, "BCfile_ne.nc", "overwrite", is_restart=.true., pelist=all_pelist)

call register_bcs_2d(atm, atm%fileobj_ne, atm%fileobj_sw, "sst", sst, layout)

if (atm%BCfile_sw_open) then
    call write_restart_bc(atm%fileobj_sw)
    call close_file(atm%fileobj_sw)
endif

if (atm%BCfile_ne_open) then
    call write_restart_bc(atm%fileobj_ne)
    call close_file(atm%fileobj_ne)
endif

if (mpp_pe() .eq. mpp_root_pe()) then
   err = nf90_open("BCfile_ne.res.nc", nf90_nowrite, ncid)
   allocate(sst_in(3, nlat))

   err = nf90_get_var(ncid, 1, sst_in)
   print *, 'SST_in: ' , sst_in
endif

call mpi_barrier(mpi_comm_world, err)

contains

subroutine register_bcs_2d(atm, fileobj_ne, fileobj_sw, var_name, var, layout, istag, jstag)
  type(atm_type), intent(in) :: atm
  type(FmsNetcdfFile_t), intent(inout) :: fileobj_sw  !< fms2io netcdf file obj for south west bc
  type(FmsNetcdfFile_t), intent(inout) :: fileobj_ne  !< fms2io netcdf file obj for north east bc
  character(len=*),         intent(in) :: var_name
  real, dimension(:,:),     intent(in) :: var
  integer, dimension(2),    intent(in) :: layout      !< Domain layout

  integer,                  intent(in), optional :: istag, jstag

  integer                              :: isd, jsd         !< Starting x/y index (data_domain)
  integer                              :: ied, jed         !< Ending x/y index (data_domain)
  integer                              :: is, js           !< Starting x/y index (compute_domain)
  integer                              :: ie, je           !< Ending x/y index (compute_domain)
  integer                              :: i_stag, j_stag   !< Extra x/y?
  integer                              :: npx, npy         !< Number of points in x/y (global_domain)
  integer                              :: x_halo, y_halo   !< Number of halos in x and y
  integer                              :: x_halo_ns
  integer, allocatable, dimension(:)   :: x1_pelist, y1_pelist
  integer, allocatable, dimension(:)   :: x2_pelist, y2_pelist
  integer                              :: n
  integer, dimension(2)                :: global_size
  integer, dimension(4)                :: indices
  logical                              :: is_root_pe

  i_stag = 0
  j_stag = 0
  if (present(istag)) i_stag = i_stag
  if (present(jstag)) j_stag = j_stag

  call mpp_get_global_domain(atm%domain, xsize = npx, ysize = npy, position=CORNER )
  call mpp_get_data_domain(atm%domain, isd, ied, jsd, jed )
  call mpp_get_compute_domain(atm%domain, is, ie, js, je )

  !< Defaults
  x_halo = is-isd
  y_halo = js-jsd
  i_stag = 0
  j_stag = 0

  allocate (x1_pelist(layout(1)))
  allocate (y1_pelist(layout(2)))
  allocate (x2_pelist(layout(1)))
  allocate (y2_pelist(layout(2)))

  !< Define west and east pelist
  do n = 1,layout(2)
     y1_pelist(n)=mpp_root_pe()+layout(1)*n-1
     y2_pelist(n)=mpp_root_pe()+layout(1)*(n-1)
  enddo

  !< Define south and north pelist
  do n = 1,layout(1)
     x1_pelist(n)=mpp_root_pe()+layout(1)*(layout(2)-1)+(n-1)
     x2_pelist(n)=mpp_root_pe()+(n-1)
  enddo

  !< EAST & WEST
  !< Set defaults for west/east halo regions
  indices(1) = 1
  indices(2) = x_halo
  indices(3) = jsd
  indices(4) = jed + j_stag
  global_size(1) = x_halo
  global_size(2) = npy-1+2*y_halo + j_stag

  !< Define west root_pe
  is_root_pe = .FALSE.
  if (is.eq.1 .and. js.eq.1) is_root_pe = .TRUE.

  if (atm%BCfile_sw_open) call register_restart_field(fileobj_sw, trim(var_name)//'_west', var, &
                                                indices, global_size, y2_pelist, &
                                                is_root_pe, jshift=y_halo)

  !< Define east root_pe
  is_root_pe = .FALSE.
  if (ie.eq.npx-1 .and. je.eq.npy-1) is_root_pe = .TRUE.

  !< Reset indices for prognostic variables in the east halo
  indices(1) = ied-x_halo+1+i_stag
  indices(2) = ied+i_stag

  if (atm%BCfile_ne_open) call register_restart_field(fileobj_ne, trim(var_name)//'_east', var, &
                                                indices, global_size, y1_pelist, &
                                                is_root_pe, jshift=y_halo, &
                                                x_halo=(size(var,1)-x_halo), ishift=-(ie+i_stag))

  !< NORTH & SOUTH
  !< set defaults for north/south halo regions
  indices(1) = isd
  indices(2) = ied+i_stag
  indices(3) = 1
  indices(4) = y_halo
  global_size(1) = npx-1+i_stag
  global_size(2) = y_halo

  !< Modify starts and ends for certain pes
  if (is.eq.1)     indices(1) = is
  if (ie.eq.npx-1) indices(2) = ie+i_stag
  x_halo_ns = 0
  if (is.eq.1) x_halo_ns=x_halo

  !define south root_pe
  is_root_pe = .FALSE.
  if (is.eq.1 .and. js.eq.1) is_root_pe = .TRUE.

  if (atm%BCfile_sw_open) call register_restart_field(fileobj_sw, trim(var_name)//'_south', var, &
                                                indices, global_size, x2_pelist, &
                                                is_root_pe, x_halo=x_halo_ns)

  !< Define north root_pe
  is_root_pe = .FALSE.
  if (ie.eq.npx-1 .and. je.eq.npy-1) is_root_pe = .TRUE.

  !< Reset indices for prognostic variables in the north halo
  indices(3) = jed-y_halo+1+j_stag
  indices(4) = jed+j_stag

  if (atm%BCfile_ne_open) call register_restart_field(fileobj_ne, trim(var_name)//'_north', var, &
                                                indices, global_size, x1_pelist, &
                                                is_root_pe, x_halo=x_halo_ns, &
                                                y_halo=(size(var,2)-y_halo), jshift=-(je+j_stag))

end subroutine register_bcs_2d

end program test_bc_restart

!< .----.----.----.----.
!< |PE 0|PE 1|PE 2|PE 3| <- x2_pelist
!< .----.----.----.----.
!< |PE 4|PE 5|PE 6|PE 7|
!< .----.----.----.----.
!< |PE 8|PE 9|PE10|PE11|
!< .----.----.----.----.
!< |PE12|PE13|PE14|PE15| <- x1_pelist
!< .----.----.----.----.
!<    ^y2_pelist     ^y1_pelist

