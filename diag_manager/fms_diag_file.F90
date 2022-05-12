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
!> @defgroup fms_diag_output_yaml_mod fms_diag_output_yaml_mod
!> @ingroup diag_manager
!! @brief fms_diag_file_mod handles the file objects data, functions, and subroutines.
!! @author Tom Robinson
!! @description The fmsDiagFile_type contains the information for each history file to be written.  It has
!! a pointer to the information from the diag yaml, additional metadata that comes from the model, and a
!! list of the variables and their variable IDs that are in the file.
module fms_diag_file_mod
!use mpp_mod
use fms2_io_mod, only: FmsNetcdfFile_t, FmsNetcdfUnstructuredDomainFile_t, FmsNetcdfDomainFile_t
#ifdef use_yaml
use fms_diag_yaml_mod, only: diagYamlObject_type, get_diag_yaml_obj, diagYamlFiles_type, varList_type
#endif
use fms_string_utils_mod, only: fms_array_to_pointer, fms_sort_this, fms_find_my_string
use fms_diag_axis_object_mod, only: diagDomain_t, diagDomain2d_t
use diag_data_mod, only: diag_null
use, intrinsic :: iso_c_binding, only : c_ptr, c_null_char
use mpp_mod, only: mpp_error, FATAL

implicit none
private

public :: fmsDiagFile_type, FMS_diag_files, fms_diag_file_init, fms_diag_file_end, set_field_as_registered
public :: set_domain_type, open_all_files

integer, parameter :: var_string_len = 25

type :: fmsDiagFile_type
 private
  integer :: id !< The number associated with this file in the larger array of files
  class(FmsNetcdfFile_t), allocatable :: fileobj !< fms2_io file object for this history file 
  character(len=2) :: file_domain_type !< 
  CLASS(diagDomain_t), POINTER :: domain
#ifdef use_yaml
  type(diagYamlFiles_type) :: diag_yam_file !< Pointer to the diag_yaml file data
#endif
  character(len=:) , dimension(:), allocatable :: file_metadata_from_model !< File metadata that comes from
                                                                           !! the model.
  type(varList_type) :: var_list
  integer, dimension(:), private, allocatable :: var_index !< An array of the variable indicies in the 
                                                                 !! diag_object.  This should be the same size as
                                                                 !! `file_varlist`
  logical, dimension(:), private, allocatable :: var_reg   !< Array corresponding to `file_varlist`, .true. 
                                                                 !! if the variable has been registered and 
                                                                 !! `file_var_index` has been set for the variable

 contains
  procedure :: open_the_file
  procedure, public :: has_file_metadata_from_model
  procedure, public :: has_fileobj
  procedure, public :: get_id
! TODO  procedure, public :: get_fileobj ! TODO
  procedure, public :: get_file_domain_type
! TODO  procedure, public :: get_diag_yaml ! TODO
  procedure, public :: get_file_metadata_from_model

end type fmsDiagFile_type

type(fmsDiagFile_type), dimension (:), allocatable, target :: FMS_diag_files !< The array of diag files

contains

subroutine fms_diag_file_init()
  type(diagYamlObject_type) :: my_yaml
  type(diagYamlFiles_type), allocatable, dimension (:) :: diag_files !< Files from the diag_yaml
  integer :: i, j
  integer :: nvar
  character (len=:), allocatable :: varlist(:)

  my_yaml = get_diag_yaml_obj()
  diag_files = my_yaml%get_diag_files()
  allocate(FMS_diag_files(size(diag_files)))

  do i = 1, size(diag_files)
    FMS_diag_files(i)%id = i
    FMS_diag_files(i)%diag_yam_file = diag_files(i)
    varlist = FMS_diag_files(i)%diag_yam_file%get_file_varlist()
    nvar = size(varlist)
    allocate(FMS_diag_files(i)%var_list%var_name(nvar))
    allocate(FMS_diag_files(i)%var_list%var_pointer(nvar))
    allocate(FMS_diag_files(i)%var_list%var_ids(nvar))
    allocate(FMS_diag_files(i)%var_index(nvar))
    allocate(FMS_diag_files(i)%var_reg(nvar))

    do j = 1, nvar
      FMS_diag_files(i)%var_list%var_ids(j) = j
      FMS_diag_files(i)%var_list%var_name = trim(varlist(j))//c_null_char
    enddo

    FMS_diag_files(i)%var_list%var_pointer =  &
      & fms_array_to_pointer(FMS_diag_files(i)%var_list%var_name)

    call fms_sort_this(FMS_diag_files(i)%var_list%var_pointer,  &
      & nvar, FMS_diag_files(i)%var_list%var_ids)

    FMS_diag_files%file_domain_type = "ND"
  enddo
end subroutine fms_diag_file_init

subroutine fms_diag_file_end()
  deallocate(FMS_diag_files)
end subroutine fms_diag_file_end

subroutine set_field_as_registered(file_ids, varname, diag_obj_id)
  integer :: file_ids(:)
  CHARACTER(len=*) :: varname
  integer :: diag_obj_id

  integer, allocatable :: var_ids(:)
  type(fmsDiagFile_type) :: file_type
  integer :: i
  integer :: id

  do  i = 1, size(file_ids)
    file_type = FMS_diag_files(file_ids(i))
    var_ids = fms_find_my_string(file_type%var_list%var_pointer, size(file_type%var_list%var_pointer), &
      & trim(varname)//c_null_char)

    id = file_type%var_list%var_ids(var_ids(1))

    file_type%var_reg(id) = .true.
    file_type%var_index = diag_obj_id
  enddo


end subroutine

subroutine set_domain_type(file_ids, domain_type, domain)
  integer :: file_ids(:)
  character(len=2) :: domain_type
  CLASS(diagDomain_t), TARGET :: domain

  integer :: i
  
  do i = 1, size(file_ids)
    if (FMS_diag_files(i)%file_domain_type .ne. domain_type) then
      if (FMS_diag_files(i)%file_domain_type .eq. "ND") then
        FMS_diag_files(i)%file_domain_type = domain_type
        FMS_diag_files(i)%domain => domain
      else
        call mpp_error (FATAL, "No good")
      endif
    endif
  end do
end subroutine

subroutine open_all_files()
  integer :: i

  do i = 1, size(FMS_diag_files)
    call FMS_diag_files(i)%open_the_file(FMS_diag_files(i)%domain)
  enddo
end subroutine

subroutine open_the_file(obj, domain)
  class(fmsDiagFile_type), intent(in) :: obj !< The file object
  CLASS(diagDomain_t) :: domain

  select type (domain)
  type is (diagDomain2d_t)
      print *, "2D domain"
  end select
end subroutine open_the_file

!> \brief Logical function to determine if the variable file_metadata_from_model has been allocated or associated
!! \return .True. if file_metadata_from_model exists .False. if file_metadata_from_model has not been set
pure logical function has_file_metadata_from_model (obj)
  class(fmsDiagFile_type), intent(in) :: obj !< The file object
  has_file_metadata_from_model = allocated(obj%file_metadata_from_model)
end function has_file_metadata_from_model
!> \brief Logical function to determine if the variable fileobj has been allocated or associated
!! \return .True. if fileobj exists .False. if fileobj has not been set
pure logical function has_fileobj (obj)
  class(fmsDiagFile_type), intent(in) :: obj !< The file object
  has_fileobj = allocated(obj%fileobj)
end function has_fileobj
!> \brief Returns a copy of the value of id
!! \return A copy of id
pure function get_id (obj) result (res)
  class(fmsDiagFile_type), intent(in) :: obj !< The file object
  integer :: res
  res = obj%id
end function get_id
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! TODO
!> \brief Returns a copy of the value of fileobj
!! \return A copy of fileobj
!pure function get_fileobj (obj) result (res)
!  class(fmsDiagFile_type), intent(in) :: obj !< The file object
!  class(FmsNetcdfFile_t) :: res
!  res = obj%fileobj
!end function get_fileobj
!> \brief Returns a copy of the value of file_domain_type
!! \return A copy of file_domain_type
pure function get_file_domain_type (obj) result (res)
  class(fmsDiagFile_type), intent(in) :: obj !< The file object
  character(1) :: res
  res = obj%file_domain_type
end function get_file_domain_type
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! TODO
!!> \brief Returns a copy of the value of diag_yaml
!!! \return A copy of diag_yaml
!#ifdef use_yaml
!pure function get_diag_yaml (obj) result (res)
!  class(fmsDiagFile_type), intent(in) :: obj !< The file object
!  type(diagYamlFiles_type) :: res
!  res = obj%diag_yaml
!end function get_diag_yaml
!#endif
!> \brief Returns a copy of the value of file_metadata_from_model
!! \return A copy of file_metadata_from_model
pure function get_file_metadata_from_model (obj) result (res)
  class(fmsDiagFile_type), intent(in) :: obj !< The file object
  character(len=:), dimension(:), allocatable :: res
  res = obj%file_metadata_from_model
end function get_file_metadata_from_model


end module fms_diag_file_mod
