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

program test_register_axis
!> @brief  This programs tests functionality of:
!!         register_axis (netcdf_add_dimension) interface
!!         get_num_dimensions
!!         get_dimension_names
!!         get_unlimited_dimension_name
!!         get_dimension_size
use   fms2_io_mod,     only: open_file, close_file, FmsNetcdfFile_t, &
                             register_axis, unlimited, get_num_dimensions, &
                             get_dimension_names, get_unlimited_dimension_name, &
                             get_dimension_size
use   fms_mod,         only: fms_init, fms_end
use   mpp_mod,         only: mpp_npes, mpp_get_current_pelist, mpp_pe, &
                             mpp_error, FATAL, mpp_sync
implicit none

type(FmsNetcdfFile_t)              :: fileobj         !< fms2io fileobj
integer                            :: dimlength       !< dimension length
integer                            :: dimlength_in    !< dimension length read in
integer, allocatable, dimension(:) :: all_pelist      !< list of all the pes
integer                            :: ndims           !< number of dimension read in
character(len=20), allocatable     :: dimnames(:)     !< dimension names read in
character(len=20)                  :: unlimit_dimname !< unlimited dimension name read in
integer                            :: dim_size        !< size of dimension read in

call fms_init

allocate(all_pelist(mpp_npes()))
call mpp_get_current_pelist(all_pelist)

!< Open the file with multiple pes
if (open_file(fileobj, "test_register_axis.nc", "overwrite", pelist=all_pelist)) then
   !< Try creating a normal dimension of length dimlength
   dimlength = 1
   call register_axis(fileobj, "dim1", dimlength)

   !< Try creating an unlimited dimension
   call register_axis(fileobj, "dim2", unlimited)

   dimlength = mpp_pe()+1
   !< Try creating a compressed dimension of length equal to sum of dimlength
   !for all pes, in this case npes!
   call register_axis(fileobj, "dim3", dimlength, is_compressed=.true.)

   call close_file(fileobj)
endif

!> Wait for the pes to catch up
call mpp_sync()

!< Error checking:
if (open_file(fileobj, "test_register_axis.nc", "read", pelist=all_pelist)) then
   !< Get the number of dimensions
   ndims = get_num_dimensions(fileobj)
   if (ndims .ne. 3) call mpp_error(FATAL, "The number of dimensions is not correct")

   !< Get the dimension names
   allocate(dimnames(ndims))
   call get_dimension_names(fileobj, dimnames)
   if (trim(dimnames(1)) .ne. "dim1") &
       call mpp_error(FATAL, "dimnames(1) is not correct: "//trim(dimnames(1))//"")

   if (trim(dimnames(2)) .ne. "dim2") &
       call mpp_error(FATAL, "dimnames(2) is not correct: "//trim(dimnames(2))//"")

   if (trim(dimnames(3)) .ne. "dim3") &
       call mpp_error(FATAL, "dimnames(3) is not correct: "//trim(dimnames(3))//"")

   !< Get the name of the unlimited dimension
   call get_unlimited_dimension_name(fileobj, unlimit_dimname)
   if (trim(unlimit_dimname) .ne. "dim2") &
       call mpp_error(FATAL, "unlimited dimension name is not correct")

   !< Get the dimensions
   call get_dimension_size(fileobj, dimnames(1), dim_size)
   if (dim_size .ne. 1) call mpp_error(FATAL, "dim1 was not read correctly")

   call get_dimension_size(fileobj, dimnames(2), dim_size)
   if (dim_size .ne. 0) call mpp_error(FATAL, "dim2 was not read correctly")

   call get_dimension_size(fileobj, dimnames(3), dim_size)
   if (dim_size .ne. sum(all_pelist+1)) call mpp_error(FATAL, "dim3 was not read correctly")

   call close_file(fileobj)
else
   call mpp_error(FATAL, "Error opening file fo read")
endif

call fms_end

end program test_register_axis
