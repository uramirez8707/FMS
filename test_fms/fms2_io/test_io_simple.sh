#!/bin/sh

#***********************************************************************
#*                   GNU Lesser General Public License
#*
#* This file is part of the GFDL Flexible Modeling System (FMS).
#*
#* FMS is free software: you can redistribute it and/or modify it under
#* the terms of the GNU Lesser General Public License as published by
#* the Free Software Foundation, either version 3 of the License, or (at
#* your option) any later version.
#*
#* FMS is distributed in the hope that it will be useful, but WITHOUT
#* ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
#* FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
#* for more details.
#*
#* You should have received a copy of the GNU Lesser General Public
#* License along with FMS.  If not, see <http://www.gnu.org/licenses/>.
#***********************************************************************

# This is part of the GFDL FMS package. This is a shell script to
# execute tests in the test_fms/fms2_io directory.

# Author: Ed Hartnett 6/10/20
#
# Set common test settings.
. ../test_common.sh

# make an input.nml for mpp_init to read
touch input.nml

# run the tests
run_test test_io_simple 6

echo "Test the get_mosaic_tile_grid functionality"
run_test test_get_mosaic_tile_grid 6

echo "Test the get_valid is_valid functionality"
run_test test_get_is_valid 1
run_test test_get_is_valid 2

echo "Test the register_axis functionality"
run_test test_register_axis 1
run_test test_register_axis 2

echo "Running test_register_axis_domain with io_layout = 1,1"
[ ! -d "OUTPUT" ] && mkdir -p "OUTPUT"
printf "&test_register_axis_domain_nml \n layout=2,3 \n io_layout=1,1\n/" | cat > input.nml
run_test test_register_axis_domain 6
rm -rf "OUTPUT"

echo "Running test_register_axis_domain with io_layout = 2,3, this creates 6 files"
mkdir "OUTPUT"
printf "&test_register_axis_domain_nml \n layout=2,3 \n io_layout=2,3\n/" | cat > input.nml
run_test test_register_axis_domain 6
rm -rf "OUTPUT"

echo "Running test_register_axis_domain with a 6 tile cubesphere and an io_layout=1,1"
mkdir "OUTPUT"
printf "&test_register_axis_domain_nml \n csphere=.true.\n/" | cat > input.nml
run_test test_register_axis_domain 6
rm -rf "OUTPUT"

echo "Running test_register_axis_domain with a 6 tile cubesphere and an io_layout=1,2"
mkdir "OUTPUT"
printf "&test_register_axis_domain_nml \n csphere=.true. \n io_layout=1,2\n/" | cat > input.nml
run_test test_register_axis_domain 12
rm -rf "OUTPUT"
