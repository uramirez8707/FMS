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

# Authors: Raymond Menzel
# Jessica Liptak
#
# Set common test settings.
. ../test_common.sh

echo CASE 1: Do not use fms2io namelist
printf "EOF\n&dummy\nEOF" | cat > input.nml
run_test test_open_file 1
if [ `ncdump ncformat_classic.res.nc -k | grep classic | wc -l` == 0 ]; then
   echo "ERROR: ncformat_classic.res.nc should type classic"
   exit 12
fi 

if [ `ncdump ncformat_64bit.res.nc -k | grep "64-bit" | wc -l` == 0 ]; then
   echo "ERROR: ncformat_64bit.res.nc should type 64-bit"
   exit 12
fi 

if [ `ncdump ncformat_netcdf4.res.nc -k | grep "netCDF-4" | wc -l` == 0 ]; then
   echo "ERROR: ncformat_netcdf4.res.nc should type netCDF-4"
   exit 12
fi 

if [ `ncdump default.res.nc -k | grep "64-bit" | wc -l` == 0 ]; then
   echo "ERROR: ncdump default.res.nc should type 64 bit"
   exit 12
fi 

echo CASE 2: Set netcdf_default_format to clasic
printf "&fms2_io_nml\n netcdf_default_format = classic\n/" | cat > input.nml
run_test test_open_file 1

if [ `ncdump ncformat_classic.res.nc -k | grep classic | wc -l` == 0 ]; then
   echo "ERROR: ncformat_classic.res.nc should type classic"
   exit 12
fi

if [ `ncdump ncformat_64bit.res.nc -k | grep "64-bit" | wc -l` == 0 ]; then
   echo "ERROR: ncformat_64bit.res.nc should type 64-bit"
   exit 12
fi

if [ `ncdump ncformat_netcdf4.res.nc -k | grep "netCDF-4" | wc -l` == 0 ]; then
   echo "ERROR: ncformat_netcdf4.res.nc should type netCDF-4"
   exit 12
fi

if [ `ncdump default.res.nc -k | grep "classic" | wc -l` == 0 ]; then
   echo "ERROR: ncdump default.res.nc should type classic"
   exit 12
fi

echo CASE 3: Set netcdf_default_format to netcdf4
printf "&fms2_io_nml\n netcdf_default_format = netcdf4\n/" | cat > input.nml
run_test test_open_file 1

if [ `ncdump ncformat_classic.res.nc -k | grep classic | wc -l` == 0 ]; then
   echo "ERROR: ncformat_classic.res.nc should type classic"
   exit 12
fi

if [ `ncdump ncformat_64bit.res.nc -k | grep "64-bit" | wc -l` == 0 ]; then
   echo "ERROR: ncformat_64bit.res.nc should type 64-bit"
   exit 12
fi

if [ `ncdump ncformat_netcdf4.res.nc -k | grep "netCDF-4" | wc -l` == 0 ]; then
   echo "ERROR: ncformat_netcdf4.res.nc should type netCDF-4"
   exit 12
fi

if [ `ncdump default.res.nc -k | grep "netCDF-4" | wc -l` == 0 ]; then
   echo "ERROR: ncdump default.res.nc should type netCDF-4"
   exit 12
fi

echo CASE 4: Set netcdf_default_format to 64bit
printf "&fms2_io_nml\n netcdf_default_format = 64bit\n/" | cat > input.nml
run_test test_open_file 1

if [ `ncdump ncformat_classic.res.nc -k | grep classic | wc -l` == 0 ]; then
   echo "ERROR: ncformat_classic.res.nc should type classic"
   exit 12
fi

if [ `ncdump ncformat_64bit.res.nc -k | grep "64-bit" | wc -l` == 0 ]; then
   echo "ERROR: ncformat_64bit.res.nc should type 64-bit"
   exit 12
fi

if [ `ncdump ncformat_netcdf4.res.nc -k | grep "netCDF-4" | wc -l` == 0 ]; then
   echo "ERROR: ncformat_netcdf4.res.nc should type netCDF-4"
   exit 12
fi

if [ `ncdump default.res.nc -k | grep "64-bit" | wc -l` == 0 ]; then
   echo "ERROR: ncdump default.res.nc should type 64-bit"
   exit 12
fi
