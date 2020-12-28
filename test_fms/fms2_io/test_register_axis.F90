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
!> @brief  This programs tests register_axis functionality
use   fms2_io_mod,     only: open_file, close_file, FmsNetcdfFile_t, &
                             register_axis, unlimited
use   fms_mod,         only: fms_init, fms_end
use   mpp_mod,         only: mpp_npes, mpp_get_current_pelist, mpp_pe, &
                             mpp_error, FATAL
implicit none

type(FmsNetcdfFile_t)              :: fileobj         !< fms2io fileobj
integer                            :: dimlength       !< dimension length
integer                            :: dimlength_in    !< dimension length read in
integer, allocatable, dimension(:) :: all_pelist      !< list of all the pes
character(len=20), allocatable     :: dimnames(:)     !< dimension names read in

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

!< Error checking:
if (open_file(fileobj, "test_register_axis.nc", "read", pelist=all_pelist)) then
   !< Get the number of dimensions
   ndims = get_num_dimensions(fileobj)
   if (ndims .ne. 3) call mpp_error(FATAL, "The number of dimensions is not correct")

   !< Get the dimension names
   call get_dimension_names(fileobj, dimnames)

   call close_file(fileobj)
else
   call mpp_error(FATAL, "Error opening file fo read")
endif

call fms_end

end program test_register_axis
