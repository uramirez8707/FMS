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

!> @file
!! @brief Tests the read_ascii_file subroutine
!! @author Colin Gladue
!! @email gfdl.climate.model.info@noaa.gov

program test_read_ascii_file

  use fms_mod, only : fms_init, fms_end
  use mpp_mod, only : mpp_error, FATAL, NOTE, mpp_pe, mpp_root_pe, mpp_sync
  use fms2_io_mod, only : fms2_io_init, ascii_read, read_ascii_distributed

  implicit none

  call fms_init()
  call test_ascii_read()
  call test_read_ascii_distributed()
  call fms_end()

  contains

  !> @brief Test fms2_io's ascii_read
  subroutine test_ascii_read()
    character(len=:), dimension(:), allocatable :: test_array !< Content array
    character(len=256) :: filename !< Name of ascii file to be read
    character(len=256) :: line !< Content of a line of the read ascii file
    integer :: num_lines !< Number of lines in the ascii file
    integer, dimension(2) :: stat !< IOSTATUS from the read method

    filename = "ascii_test1"
    call ascii_read(filename, test_array)
    read(test_array(1), *) stat
    if (stat(1)*6 - (stat(2)+3) /= 13) call mpp_error(FATAL, "test_read_ascii: failed to read integers")
    read(test_array(2), *) num_lines
    if (num_lines-11 /= 12) call mpp_error(FATAL, "test_read_ascii: failed to read integer")
    read(test_array(3), *) line
    if (trim(line)//"wut" /= "forlendulawut") call mpp_error(FATAL, "test_read_ascii: failed to read string")

  end subroutine

  !> @brief Test fms2_io's read_ascii_distributed
  subroutine test_read_ascii_distributed()
    integer          :: unit_number !< Unit number of the file to read
    integer          :: error_code  !< The error code after doing io
    character(len=4) :: line1(2)    !< buffer to read an array of strings to
    integer          :: line2(8)    !< buffer to read an array of integers to
    real             :: line3(2)    !< buffer to read an array of reals to

    error_code = 0

    if (mpp_pe() .eq. mpp_root_pe()) then
      open(newunit=unit_number, FILE="ascii_distributed", ACTION='READ', IOSTAT=error_code)
      if (error_code .ne. 0) call mpp_error(FATAL, "Error opening the file, ascii_distributed")
    endif

    call read_ascii_distributed(unit_number, '*', line1, error_code)
    if (error_code .ne. 0) call mpp_error(FATAL, "Error reading the first line, ascii_distributed")
    if (trim(line1(1)) .ne. "uno" .or. trim(line1(2)) .ne. "dos") &
      call mpp_error(FATAL, "The data read is not correct")
    call mpp_sync()

    call read_ascii_distributed(unit_number, '(4i1)', line2, error_code)
    if (error_code .ne. 0) call mpp_error(FATAL, "Error reading the second line, ascii_distributed")
    if (line2(1) .ne. 1 .or. line2(2) .ne. 2 .or. line2(3) .ne. 3 .or. line2(4) .ne. 4 .or. &
        line2(5) .ne. 5 .or. line2(6) .ne. 6 .or. line2(7) .ne. 7 .or. line2(8) .ne. 8) &
        call mpp_error(FATAL, "The data read is not correct")

    call read_ascii_distributed(unit_number, '(2f1.3)', line3, error_code)
    if (error_code .ne. 0) call mpp_error(FATAL, "Error reading the third line, ascii_distributed")
    if (real(line3(1)) .eq. real(6.19) .and. real(line3(2)) .eq. real(6.66)) &
      call mpp_error(FATAL, "The data read is not correct")

    if (mpp_pe() .eq. mpp_root_pe()) close(unit_number)
  end subroutine
end program test_read_ascii_file
