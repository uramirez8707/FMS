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

!> @brief  This programs tests the "register_variable_attribute" and
!! "get_variable_attribute" interfaces
program test_var_att
use fms_mod,      only: fms_init, fms_end
use mpp_mod,      only: mpp_error, FATAL
use fms2_io_mod,  only: FmsNetcdfFile_t, open_file, close_file, register_field, register_variable_attribute, &
                        get_variable_attribute
use platform_mod, only: r8_kind, r4_kind, i8_kind, i4_kind

implicit NONE

type(FmsNetcdfFile_t)  :: fileobj           !< fms2io netcd file obj
real(kind=r8_kind)     :: buf_r8_kind       !< r8_kind buffer
real(kind=r8_kind)     :: buf_r8_kind_1d(2) !< r8_kind 1D buffer
real(kind=r4_kind)     :: buf_r4_kind       !< r4_kind buffer
real(kind=r4_kind)     :: buf_r4_kind_1d(2) !< r4_kind 1D buffer
integer(kind=i4_kind)  :: buf_i4_kind       !< i4_kind buffer
integer(kind=i4_kind)  :: buf_i4_kind_1d(2) !< i4_kind 1D buffer
integer(kind=i8_kind)  :: buf_i8_kind       !< i8_kind buffer
integer(kind=i8_kind)  :: buf_i8_kind_1d(2) !< i8_kind 1D buffer
character (len = 120)  :: my_format(3)      !< Array of formats to try.
character(len=20)      :: buf_str           !< character buffer
integer                :: i                 !< For Do loop

call fms_init

my_format(1) = '64bit'
my_format(2) = 'classic'
my_format(3) = 'netcdf4'

do i = 1, size(my_format)
   !< Write out the different possible variable attributes to a netcdf file
   if (.not. open_file(fileobj, "test_var_att_"//trim(my_format(i))//".nc", "overwrite", nc_format=my_format(i))) &
     call mpp_error(FATAL, "test_var_att: error opening the file for writting")

   call register_field(fileobj, "var", "double")

   call register_variable_attribute(fileobj, "var", "buf_r8_kind", real(7., kind=r8_kind))
   call register_variable_attribute(fileobj, "var", "buf_r8_kind_1d", (/ real(7., kind=r8_kind), real(9., kind=r8_kind) /))

   call register_variable_attribute(fileobj, "var", "buf_r4_kind", real(4., kind=r4_kind))
   call register_variable_attribute(fileobj, "var", "buf_r4_kind_1d", (/ real(4., kind=r4_kind), real(6., kind=r4_kind)/) )

   call register_variable_attribute(fileobj, "var", "buf_i4_kind", int(3, kind=i4_kind))
   call register_variable_attribute(fileobj, "var", "buf_i4_kind_1d", (/ int(3, kind=i4_kind), int(5, kind=i4_kind) /) )

   !< int8 is only supported with the "netcdf4" type
   if(i .eq. 3) then
     call register_variable_attribute(fileobj, "var", "buf_i8_kind", int(2, kind=i8_kind))
     call register_variable_attribute(fileobj, "var", "buf_i8_kind_1d", (/ int(2, kind=i8_kind), int(4, kind=i8_kind) /) )
   endif

   call register_variable_attribute(fileobj, "var", "buf_str", "some text"//char(0), str_len=10)

   call close_file(fileobj)

   !< Read the var attributes from the netcdf file
   if (open_file(fileobj, "test_var_att_"//trim(my_format(i))//".nc", "read", nc_format=my_format(i))) &
     call mpp_error(FATAL, "test_var_att: error opening the file for reading")

   call get_variable_attribute(fileobj, "var", "buf_r8_kind", buf_r8_kind)
   call get_variable_attribute(fileobj, "var", "buf_r8_kind_1d", buf_r8_kind_1d)

   call get_variable_attribute(fileobj, "var", "buf_r4_kind", buf_r4_kind)
   call get_variable_attribute(fileobj, "var", "buf_r4_kind_1d", buf_r4_kind_1d)

   call get_variable_attribute(fileobj, "var", "buf_i4_kind", buf_i4_kind)
   call get_variable_attribute(fileobj, "var", "buf_i4_kind_1d", buf_i4_kind_1d)

   !< int8 is only supported with the "netcdf4" type
   if(i .eq. 3) then
     call get_variable_attribute(fileobj, "var", "buf_i8_kind", buf_i8_kind)
     call get_variable_attribute(fileobj, "var", "buf_i8_kind_1d", buf_i8_kind_1d)
   endif

   call get_variable_attribute(fileobj, "var", "buf_str", buf_str)

   call close_file(fileobj)

   !< Compares the values read with the expected values
   if (buf_r8_kind /= real(7., kind=r8_kind)) call mpp_error(FATAL, "test_var_att-"// &
     & trim(my_format(i))//": error reading buf_r8_kind")
   if (buf_r8_kind_1d(1) /= real(7., kind=r8_kind) .or. buf_r8_kind_1d(2) /= real(9., kind=r8_kind)) &
     call mpp_error(FATAL, "test_var_att-"//trim(my_format(i))//": error reading buf_r8_kind_1d")

   if (buf_r4_kind /= real(4., kind=r4_kind)) call mpp_error(FATAL, "test_var_att-"// &
     & trim(my_format(i))//": error reading buf_r4_kind")
   if (buf_r4_kind_1d(1) /= real(4., kind=r4_kind) .or. buf_r4_kind_1d(2) /= real(6., kind=r4_kind)) &
     call mpp_error(FATAL, "test_var_att-"//trim(my_format(i))//": error reading buf_r4_kind_1d")

   if (buf_i4_kind /= int(3, kind=i4_kind)) call mpp_error(FATAL, "test_var_att-"// &
     & trim(my_format(i))//": error reading buf_i4_kind")
   if (buf_i4_kind_1d(1) /= int(3, kind=i4_kind) .or. buf_i4_kind_1d(2) /= int(5, kind=i4_kind)) &
     call mpp_error(FATAL, "test_var_att-"//trim(my_format(i))//": error reading buf_i4_kind_1d")

!< int8 is only supported with the "netcdf4" type
   if(i .eq. 3) then
     if (buf_i8_kind /= int(2, kind=i8_kind)) call mpp_error(FATAL, "test_var_att-"// &
       & trim(my_format(i))//": error reading buf_i8_kind")
     if (buf_i8_kind_1d(1) /= int(2, kind=i8_kind) .or. buf_i8_kind_1d(2) /= int(4, kind=i8_kind)) &
       & call mpp_error(FATAL, "test_var_att-"//trim(my_format(i))//": error reading buf_i8_kind_1d")
   endif

   if (trim(buf_str) /= "some text") then
     print *, "buf_str read in = ", trim(buf_str)
     call mpp_error(FATAL, "test_var_att-"//trim(my_format(i))//": error reading buf_str")
   endif
enddo

call fms_end

end program test_var_att