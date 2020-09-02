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

# Author: Uriel Ramirez 6/30/20
#
# Set common test settings.
. ../test_common.sh

# make an input.nml for mpp_init to read
touch input.nml

echo Test 1: netcdf 4 file type
printf "&fms2_io_nml \n netcdf_default_format = netcdf4  \n/" | cat > input.nml
run_test test_global_att 1

#Check that file was written in the correct type
ncdump test_global_att.nc -k

if [ `ncdump test_global_att.nc -k | grep netCDF-4 | wc -l` == 0 ]; then
   echo "ERROR: test_global_att.nc should be type netCDF-4"
   exit 12
fi

echo Test 2: classic file type
printf "&fms2_io_nml \n netcdf_default_format = classic  \n/" | cat > input.nml
run_test test_global_att 1

#Check that file was written in the correct type
if [ `ncdump test_global_att.nc -k | grep classic | wc -l` == 0 ]; then
   echo "ERROR: test_global_att.nc should type classic"
   exit 12
fi

echo Test 3: 64bit file type
printf "&fms2_io_nml \n netcdf_default_format = 64bit  \n/" | cat > input.nml
run_test test_global_att 1

#Check that file was written in the correct type
if [ `ncdump test_global_att.nc -k | grep 64-bit | wc -l` == 0 ]; then
   echo "ERROR: test_global_att.nc should type 64bit"
   exit 12
fi
