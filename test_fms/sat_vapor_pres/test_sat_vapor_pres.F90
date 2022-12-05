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

!> @brief This tests stuff
program test_sat_vapor_pres
use fms_mod, only: fms_init, fms_end
use sat_vapor_pres_mod, only: compute_qs, sat_vapor_pres_init

implicit none

integer :: my_nblocks = 2
integer :: block_start(2)
integer :: block_end(2)
integer :: is, ie
real, dimension(10) :: temperature, pressure, q, qs
integer :: l

call fms_init()
call sat_vapor_pres_init()

block_start = (/1, 6/)
block_end = (/5, 10/)

temperature = 20.
pressure = 10.
q = 10.
qs = 10.

!$OMP parallel do default(none) shared(my_nblocks,block_start,block_end, &
!$OMP                                  temperature, pressure, q, qs) &
!$OMP                           private(is,ie)
do l = 1, my_nblocks
    is=block_start(l)
    ie=block_end(l)

    call compute_qs(temperature(is:ie), pressure(is:ie), qs(is:ie), q=q(is:ie), &
                    es_over_liq_and_ice=.true.)
enddo

call fms_end()

end program