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

!> @brief Add a restart variable to a netcdf file.
subroutine netcdf_add_restart_variable_0d(fileobj, variable_name, vdata, dimensions, &
                                          is_optional)

  class(FmsNetcdfFile_t), intent(inout) :: fileobj !< File object.
  character(len=*), intent(in) :: variable_name !< Variable name.
  class(*), intent(in), target :: vdata !< Pointer to
                                                              !! variable data.
  character(len=*), dimension(1), intent(in), optional :: dimensions !< Dimension names.
  logical, intent(in), optional :: is_optional !< Prevent errors in read-only files
                                               !! if a variable does not exist.

  character(len=8) :: buf

  if (fileobj%is_readonly .and. present(is_optional)) then
    if (is_optional .and. .not. variable_exists(fileobj, variable_name)) then
      return
    endif
  endif
  call add_restart_var_to_array(fileobj, variable_name)
  fileobj%restart_vars(fileobj%num_restart_vars)%data0d => vdata
  if (.not. fileobj%is_readonly) then
    call get_data_type_string(vdata, buf)
    if (present(dimensions)) then
      if (.not. is_dimension_unlimited(fileobj, dimensions(1), .true.)) then
        call error("a scalar input variable can only have an unlimited dimension.")
      endif
    endif
    call netcdf_add_variable(fileobj, variable_name, buf, dimensions)
  endif
end subroutine netcdf_add_restart_variable_0d


!> @brief Wrapper to distinguish interfaces.
subroutine netcdf_add_restart_variable_0d_wrap(fileobj, variable_name, vdata, &
                                               dimensions, is_optional)
  type(FmsNetcdfFile_t), intent(inout) :: fileobj !< File object.
  character(len=*), intent(in) :: variable_name !< Variable name.
  class(*),  intent(in), target :: vdata !< Pointer to
                                                              !! variable data.
  character(len=*), dimension(1), intent(in), optional :: dimensions !< Dimension names.
  logical, intent(in), optional :: is_optional !< Prevent errors in read-only files
                                               !! if a variable does not exist.

  call netcdf_add_restart_variable(fileobj, variable_name, vdata, dimensions, is_optional)
end subroutine netcdf_add_restart_variable_0d_wrap


!> @brief Add a restart variable to a netcdf file.
subroutine netcdf_add_restart_variable_1d(fileobj, variable_name, vdata, &
                                          dimensions, is_optional)
  class(FmsNetcdfFile_t), intent(inout) :: fileobj !< File object.
  character(len=*), intent(in) :: variable_name !< Variable name.
  class(*), dimension(:), intent(in), target :: vdata !< Pointer to
                                                              !! variable data.
  character(len=*), dimension(:), intent(in), optional :: dimensions !< Dimension names.
  logical, intent(in), optional :: is_optional !< Prevent errors in read-only files
                                               !! if a variable does not exist.

  character(len=8) :: buf
  integer :: ndims
  integer :: vdata_rank

  if (fileobj%is_readonly .and. present(is_optional)) then
    if (is_optional .and. .not. variable_exists(fileobj, variable_name)) then
      return
    endif
  endif
  call add_restart_var_to_array(fileobj, variable_name)
  fileobj%restart_vars(fileobj%num_restart_vars)%data1d => vdata
  if (.not. fileobj%is_readonly) then
    call get_data_type_string(vdata, buf)
    if (.not. present(dimensions)) then
      call error("dimension names required if the file is not read-only.")
    endif
    ndims = size(dimensions)
    vdata_rank = size(shape(vdata))
    if (ndims .eq. vdata_rank+1) then
      if (.not. is_dimension_unlimited(fileobj, dimensions(ndims), .true.)) then
        call error("the slowest dimension must be unlimited.")
      endif
    elseif (ndims .ne. vdata_rank) then
      call error("rank mismatch between vdata and dimensions arrays.")
    endif
    call netcdf_add_variable(fileobj, variable_name, buf, dimensions)
  endif
end subroutine netcdf_add_restart_variable_1d


!> @brief Wrapper to distinguish interfaces.
subroutine netcdf_add_restart_variable_1d_wrap(fileobj, variable_name, vdata, &
                                               dimensions, is_optional)
  type(FmsNetcdfFile_t), intent(inout) :: fileobj !< File object.
  character(len=*), intent(in) :: variable_name !< Variable name.
  class(*), dimension(:), intent(in), target :: vdata !< Pointer to
                                                              !! variable data.
  character(len=*), dimension(:), intent(in), optional :: dimensions !< Dimension names.
  logical, intent(in), optional :: is_optional !< Prevent errors in read-only files
                                               !! if a variable does not exist.

  call netcdf_add_restart_variable(fileobj, variable_name, vdata, dimensions, is_optional)
end subroutine netcdf_add_restart_variable_1d_wrap


!> @brief Add a restart variable to a netcdf file.
subroutine netcdf_add_restart_variable_2d(fileobj, variable_name, vdata, &
                                          dimensions, is_optional)
  class(FmsNetcdfFile_t), intent(inout) :: fileobj !< File object.
  character(len=*), intent(in) :: variable_name !< Variable name.
  class(*), dimension(:,:), intent(in), target :: vdata !< Pointer to
                                                              !! variable data.
  character(len=*), dimension(:), intent(in), optional :: dimensions !< Dimension names.
  logical, intent(in), optional :: is_optional !< Prevent errors in read-only files
                                               !! if a variable does not exist.

  character(len=8) :: buf
  integer :: ndims
  integer :: vdata_rank

  if (fileobj%is_readonly .and. present(is_optional)) then
    if (is_optional .and. .not. variable_exists(fileobj, variable_name)) then
      return
    endif
  endif
  call add_restart_var_to_array(fileobj, variable_name)
  fileobj%restart_vars(fileobj%num_restart_vars)%data2d => vdata
  if (.not. fileobj%is_readonly) then
    call get_data_type_string(vdata, buf)
    if (.not. present(dimensions)) then
      call error("dimension names required if the file is not read-only.")
    endif
    ndims = size(dimensions)
    vdata_rank = size(shape(vdata))
    if (ndims .eq. vdata_rank+1) then
      if (.not. is_dimension_unlimited(fileobj, dimensions(ndims), .true.)) then
        call error("the slowest dimension must be unlimited.")
      endif
    elseif (ndims .ne. vdata_rank) then
      call error("rank mismatch between vdata and dimensions arrays.")
    endif
    call netcdf_add_variable(fileobj, variable_name, buf, dimensions)
  endif
end subroutine netcdf_add_restart_variable_2d


!> @brief Wrapper to distinguish interfaces.
subroutine netcdf_add_restart_variable_2d_wrap(fileobj, variable_name, vdata, &
                                               dimensions, is_optional)
  type(FmsNetcdfFile_t), intent(inout) :: fileobj !< File object.
  character(len=*), intent(in) :: variable_name !< Variable name.
  class(*), dimension(:,:), intent(in), target :: vdata !< Pointer to
                                                              !! variable data.
  character(len=*), dimension(:), intent(in), optional :: dimensions !< Dimension names.
  logical, intent(in), optional :: is_optional !< Prevent errors in read-only files
                                               !! if a variable does not exist.

  call netcdf_add_restart_variable(fileobj, variable_name, vdata, dimensions, is_optional)
end subroutine netcdf_add_restart_variable_2d_wrap


!> @brief Add a restart variable to a netcdf file.
subroutine netcdf_add_restart_variable_3d(fileobj, variable_name, vdata, &
                                          dimensions, is_optional)
  class(FmsNetcdfFile_t), intent(inout) :: fileobj !< File object.
  character(len=*), intent(in) :: variable_name !< Variable name.
  class(*), dimension(:,:,:), intent(in), target :: vdata !< Pointer to
                                                              !! variable data.
  character(len=*), dimension(:), intent(in), optional :: dimensions !< Dimension names.
  logical, intent(in), optional :: is_optional !< Prevent errors in read-only files
                                               !! if a variable does not exist.

  character(len=8) :: buf
  integer :: ndims
  integer :: vdata_rank

  if (fileobj%is_readonly .and. present(is_optional)) then
    if (is_optional .and. .not. variable_exists(fileobj, variable_name)) then
      return
    endif
  endif
  call add_restart_var_to_array(fileobj, variable_name)
  fileobj%restart_vars(fileobj%num_restart_vars)%data3d => vdata
  if (.not. fileobj%is_readonly) then
    call get_data_type_string(vdata, buf)
    if (.not. present(dimensions)) then
      call error("dimension names required if the file is not read-only.")
    endif
    ndims = size(dimensions)
    vdata_rank = size(shape(vdata))
    if (ndims .eq. vdata_rank+1) then
      if (.not. is_dimension_unlimited(fileobj, dimensions(ndims), .true.)) then
        call error("the slowest dimension must be unlimited.")
      endif
    elseif (ndims .ne. vdata_rank) then
      call error("rank mismatch between vdata and dimensions arrays.")
    endif
    call netcdf_add_variable(fileobj, variable_name, buf, dimensions)
  endif
end subroutine netcdf_add_restart_variable_3d


!> @brief Wrapper to distinguish interfaces.
subroutine netcdf_add_restart_variable_3d_wrap(fileobj, variable_name, vdata, &
                                               dimensions, is_optional)
  type(FmsNetcdfFile_t), intent(inout) :: fileobj !< File object.
  character(len=*), intent(in) :: variable_name !< Variable name.
  class(*), dimension(:,:,:), intent(in), target :: vdata !< Pointer to
                                                              !! variable data.
  character(len=*), dimension(:), intent(in), optional :: dimensions !< Dimension names.
  logical, intent(in), optional :: is_optional !< Prevent errors in read-only files
                                               !! if a variable does not exist.

  call netcdf_add_restart_variable(fileobj, variable_name, vdata, dimensions, is_optional)
end subroutine netcdf_add_restart_variable_3d_wrap


!> @brief Add a restart variable to a netcdf file.
subroutine netcdf_add_restart_variable_4d(fileobj, variable_name, vdata, &
                                          dimensions, is_optional)
  class(FmsNetcdfFile_t), intent(inout) :: fileobj !< File object.
  character(len=*), intent(in) :: variable_name !< Variable name.
  class(*), dimension(:,:,:,:), intent(in), target :: vdata !< Pointer to
                                                              !! variable data.
  character(len=*), dimension(:), intent(in), optional :: dimensions !< Dimension names.
  logical, intent(in), optional :: is_optional !< Prevent errors in read-only files
                                               !! if a variable does not exist.

  character(len=8) :: buf
  integer :: ndims
  integer :: vdata_rank

  if (fileobj%is_readonly .and. present(is_optional)) then
    if (is_optional .and. .not. variable_exists(fileobj, variable_name)) then
      return
    endif
  endif
  call add_restart_var_to_array(fileobj, variable_name)
  fileobj%restart_vars(fileobj%num_restart_vars)%data4d => vdata
  if (.not. fileobj%is_readonly) then
    call get_data_type_string(vdata, buf)
    if (.not. present(dimensions)) then
      call error("dimension names required if the file is not read-only.")
    endif
    ndims = size(dimensions)
    vdata_rank = size(shape(vdata))
    if (ndims .eq. vdata_rank+1) then
      if (.not. is_dimension_unlimited(fileobj, dimensions(ndims), .true.)) then
        call error("the slowest dimension must be unlimited.")
      endif
    elseif (ndims .ne. vdata_rank) then
      call error("rank mismatch between vdata and dimensions arrays.")
    endif
    call netcdf_add_variable(fileobj, variable_name, buf, dimensions)
  endif
end subroutine netcdf_add_restart_variable_4d


!> @brief Wrapper to distinguish interfaces.
subroutine netcdf_add_restart_variable_4d_wrap(fileobj, variable_name, vdata, &
                                               dimensions, is_optional)
  type(FmsNetcdfFile_t), intent(inout) :: fileobj !< File object.
  character(len=*), intent(in) :: variable_name !< Variable name.
  class(*), dimension(:,:,:,:), intent(in), target :: vdata !< Pointer to
                                                              !! variable data.
  character(len=*), dimension(:), intent(in), optional :: dimensions !< Dimension names.
  logical, intent(in), optional :: is_optional !< Prevent errors in read-only files
                                               !! if a variable does not exist.

  call netcdf_add_restart_variable(fileobj, variable_name, vdata, dimensions, is_optional)
end subroutine netcdf_add_restart_variable_4d_wrap


!> @brief Add a restart variable to a netcdf file.
subroutine netcdf_add_restart_variable_5d(fileobj, variable_name, vdata, &
                                          dimensions, is_optional)
  class(FmsNetcdfFile_t), intent(inout) :: fileobj !< File object.
  character(len=*), intent(in) :: variable_name !< Variable name.
  class(*), dimension(:,:,:,:,:), intent(in), target :: vdata !< Pointer to
                                                              !! variable data.
  character(len=*), dimension(:), intent(in), optional :: dimensions !< Dimension names.
  logical, intent(in), optional :: is_optional !< Prevent errors in read-only files
                                               !! if a variable does not exist.

  character(len=8) :: buf
  integer :: ndims
  integer :: vdata_rank

  if (fileobj%is_readonly .and. present(is_optional)) then
    if (is_optional .and. .not. variable_exists(fileobj, variable_name)) then
      return
    endif
  endif
  call add_restart_var_to_array(fileobj, variable_name)
  fileobj%restart_vars(fileobj%num_restart_vars)%data5d => vdata
  if (.not. fileobj%is_readonly) then
    call get_data_type_string(vdata, buf)
    if (.not. present(dimensions)) then
      call error("dimension names required if the file is not read-only.")
    endif
    ndims = size(dimensions)
    vdata_rank = size(shape(vdata))
    if (ndims .eq. vdata_rank+1) then
      if (.not. is_dimension_unlimited(fileobj, dimensions(ndims), .true.)) then
        call error("the slowest dimension must be unlimited.")
      endif
    elseif (ndims .ne. vdata_rank) then
      call error("rank mismatch between vdata and dimensions arrays.")
    endif
    call netcdf_add_variable(fileobj, variable_name, buf, dimensions)
  endif
end subroutine netcdf_add_restart_variable_5d


!> @brief Wrapper to distinguish interfaces.
subroutine netcdf_add_restart_variable_5d_wrap(fileobj, variable_name, vdata, &
                                               dimensions, is_optional)
  type(FmsNetcdfFile_t), intent(inout) :: fileobj !< File object.
  character(len=*), intent(in) :: variable_name !< Variable name.
  class(*), dimension(:,:,:,:,:), intent(in), target :: vdata !< Pointer to
                                                              !! variable data.
  character(len=*), dimension(:), intent(in), optional :: dimensions !< Dimension names.
  logical, intent(in), optional :: is_optional !< Prevent errors in read-only files
                                               !! if a variable does not exist.

  call netcdf_add_restart_variable(fileobj, variable_name, vdata, dimensions, is_optional)
end subroutine netcdf_add_restart_variable_5d_wrap

subroutine do_some_magic(fileobj, variable_name, vdata, indices, global_size, pelist, is_root_pe, &
                         x_halo, y_halo, jshift, ishift, is_optional)
  type(FmsNetcdfFile_t), intent(inout) :: fileobj !< File object.
  character(len=*), intent(in) :: variable_name !< Variable name.
  real, dimension(:,:), intent(in), target :: vdata !< Pointer to
  integer, dimension(4), intent(in)                 :: indices
  integer, dimension(2), intent(in)                 :: global_size
  integer, dimension(:), intent(in)                 :: pelist
  logical, intent(in)                               :: is_root_pe
  integer, intent(in), optional :: x_halo
  integer, intent(in), optional :: y_halo
  integer, intent(in), optional :: jshift
  integer, intent(in), optional :: ishift
  logical, intent(in), optional :: is_optional !< Prevent errors in read-only files
                                               !! if a variable does not exist.

  integer :: current_var
  character(len=7) :: dimnames(5)

  if (fileobj%is_readonly .and. present(is_optional)) then
    if (is_optional .and. .not. variable_exists(fileobj, variable_name)) then
      return
    endif
  endif

  call set_dimensions(fileobj, fileobj%bc_dimensions, dimnames, global_size)
  call netcdf_add_restart_variable_2d(fileobj, variable_name, vdata, dimnames(1:3), is_optional)

  current_var = fileobj%num_restart_vars
  fileobj%restart_vars(current_var)%is_bc_variable = .true.
  fileobj%restart_vars(current_var)%bc_info%is_root_pe = is_root_pe

  allocate(fileobj%restart_vars(current_var)%bc_info%indices(4))
  fileobj%restart_vars(current_var)%bc_info%indices = indices

  allocate(fileobj%restart_vars(current_var)%bc_info%global_size(2))
  fileobj%restart_vars(current_var)%bc_info%global_size = global_size

  allocate(fileobj%restart_vars(current_var)%bc_info%pelist(size(pelist)))
  fileobj%restart_vars(current_var)%bc_info%pelist = pelist

  fileobj%restart_vars(current_var)%bc_info%x_halo = 0
  fileobj%restart_vars(current_var)%bc_info%y_halo = 0
  fileobj%restart_vars(current_var)%bc_info%ishift = 0
  fileobj%restart_vars(current_var)%bc_info%jshift = 0

  if (present(x_halo)) fileobj%restart_vars(current_var)%bc_info%x_halo = x_halo
  if (present(y_halo)) fileobj%restart_vars(current_var)%bc_info%y_halo = y_halo
  if (present(ishift)) fileobj%restart_vars(current_var)%bc_info%ishift = ishift
  if (present(jshift)) fileobj%restart_vars(current_var)%bc_info%jshift = jshift

end subroutine do_some_magic

subroutine set_dimensions(fileobj, bc_dimensions, dimnames, global_size)
  type(FmsNetcdfFile_t), intent(inout) :: fileobj !< File object.
  type(dimension_information), intent(inout) :: bc_dimensions
  character(len=7), intent(inout) :: dimnames(5)
  integer, dimension(:), intent(in) :: global_size

  character(len=1) :: suffix
  character(len=7) :: axisname

  if (sum(bc_dimensions%cur_dim_len) .eq. 0) call netcdf_add_dimension(fileobj, "Time", unlimited)

  if (bc_dimensions%cur_dim_len(1) .eq. 0) then
     bc_dimensions%cur_dim_len(1) = 1
     bc_dimensions%xlen(1) = global_size(1)
     call netcdf_add_dimension(fileobj, "xaxis_1", bc_dimensions%xlen(1))
     dimnames(1) = "xaxis_1"
  else
     if ( .not. any(bc_dimensions%xlen(1:bc_dimensions%cur_dim_len(1)) .eq. global_size(1))) then
        bc_dimensions%cur_dim_len(1) = bc_dimensions%cur_dim_len(1) + 1
        bc_dimensions%xlen(bc_dimensions%cur_dim_len(1)) = global_size(1)
        write(suffix,'(I1)') bc_dimensions%cur_dim_len(1)
        axisname = 'xaxis_'//suffix
        call netcdf_add_dimension(fileobj, axisname, global_size(1))
        dimnames(1) = axisname
     endif
  endif

  if (bc_dimensions%cur_dim_len(2) .eq. 0) then
     bc_dimensions%cur_dim_len(2) = 1
     bc_dimensions%ylen(1) = global_size(2)
     call netcdf_add_dimension(fileobj, "yaxis_1", bc_dimensions%ylen(1))
     dimnames(2) = "yaxis_1"
  else
     if ( .not. any(bc_dimensions%xlen(1:bc_dimensions%cur_dim_len(2)) .eq. global_size(2))) then
        bc_dimensions%cur_dim_len(2) = bc_dimensions%cur_dim_len(2) + 1
        bc_dimensions%ylen(bc_dimensions%cur_dim_len(2)) = global_size(2)
        write(suffix,'(I1)') bc_dimensions%cur_dim_len(2)
        axisname = 'yaxis_'//suffix
        call netcdf_add_dimension(fileobj, axisname, global_size(2))
        dimnames(2) = axisname
     endif
  endif

  dimnames(size(global_size)+1) = "Time"

!  if (.not. dimension_exists(fileobj, "Time", broadcast=.false.)) then
!      call netcdf_add_dimension(fileobj, "Time", unlimited)
!  endif
! TO DO: Handle the 3D case
!  if (bc_dimensions%cur_dim_len(3) .eq. 0) then
!     bc_dimensions%cur_dim_len(3) = 1
!     bc_dimensions%zlen = global_size(3)
!  else
!     if ( .not. any(bc_dimensions%xlen(1:bc_dimensions%cur_dim_len(3)) .eq. global_size(3)) then
!     bc_dimensions%cur_dim_len(3) = bc_dimensions%cur_dim_len(3) + 1
!     bc_dimensions%zlen = global_size(3)
!     endif
!  endif

end subroutine
