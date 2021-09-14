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

!> @brief  This programs tests diag_manager with no domain decomposed
!! variables

program test_no_domain

use fms_mod, only: fms_init, fms_end
use mpp_mod
use diag_manager_mod
use time_manager_mod

implicit none

integer :: i !< For do loops

type(time_type) :: Time

integer :: id_dim1, id_dim2, id_dim3 !< IDs for diag axis
integer :: id_var3d, id_var2d, id_var1d, id_varscalar !< IDs for non static variables
integer :: id_var3ds, id_var2ds, id_var1ds !< IDs for static variables
integer :: id_nodata !< ID for a variable that send_data is never called

real :: var3d(10,20,25), var2d(10,20), var1d(10), varscalar !< Data to be send to send data

logical :: used !< Flag indicating if a send data was successful

call fms_init ()
call set_calendar_type(JULIAN)
call diag_manager_init ()

Time = set_date(2,1,1,0,0,0)

!< Register the axis
id_dim1  = diag_axis_init('dim1',  (/(real(i),i=1,10)/),  'dim1 units', 'x')
id_dim2  = diag_axis_init('dim2',  (/(real(i),i=1,20)/),  'dim2 units', 'y')
id_dim3  = diag_axis_init('dim3',  (/(real(i),i=1,25)/),  'dim3 units', 'z')

!< Register the non static fields
id_var3d = register_diag_field  ('test_diag_manager_mod', 'var3d', (/id_dim1, id_dim2, id_dim3/), Time, 'var3d', 'apples')
id_var2d = register_diag_field  ('test_diag_manager_mod', 'var2d', (/id_dim1, id_dim2/), Time, 'var2d', 'apples')
id_var1d = register_diag_field  ('test_diag_manager_mod', 'var1d', (/id_dim1/), Time, 'var1d', 'apples')
id_varscalar = register_diag_field  ('test_diag_manager_mod', 'varscalar', Time, 'varscalar', 'apples')
id_nodata = register_diag_field  ('test_diag_manager_mod', 'nodata', Time, 'nodata', 'apples')

!< Register the static fields
id_var3ds = register_static_field  ('test_diag_manager_mod', 'var3ds', (/id_dim1, id_dim2, id_dim3/), 'var3ds', 'apples')
id_var2ds = register_static_field  ('test_diag_manager_mod', 'var2ds', (/id_dim1, id_dim2/), 'var2ds', 'apples')
id_var1ds = register_static_field  ('test_diag_manager_mod', 'var1ds', (/id_dim1/), 'var1ds', 'apples')

!< Send the data for the static fields
var3d = 0.
var2d = 0.
var1d = 0.
varscalar = 0.

if ( id_var3ds > 0 ) used = send_data(id_var3ds, var3d)
if ( id_var2ds > 0 ) used = send_data(id_var2ds, var2d)
if ( id_var1ds > 0 ) used = send_data(id_var1ds, var1d)

!< Change the time and send data for the non static fields
do i = 1, 46
   Time = increment_date(Time, 0, 0, 0, 0, 30, 0) !Increase the time by 30 min
   var3d = var3d + 0.5
   var2d = var2d + 0.5
   var1d = var1d + 0.5
   varscalar = varscalar + 0.5

   if ( id_var3d > 0 ) used = send_data(id_var3d, var3d, Time)
   if ( id_var2d > 0 ) used = send_data(id_var2d, var2d, Time)
   if ( id_var1d > 0 ) used = send_data(id_var1d, var1d, Time)
   if ( id_varscalar > 0 ) used = send_data(id_varscalar, varscalar, Time)

end do

call diag_manager_end(Time)
call fms_end ()

end program test_no_domain
