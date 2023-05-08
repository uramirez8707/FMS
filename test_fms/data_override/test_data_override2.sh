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
#
# Copyright (c) 2019-2021 Ed Hartnett, Uriel Ramirez, Seth Underwood

# Set common test settings.
. ../test-lib.sh

printf '"ICE", "sic_obs"        , "SIC"         ,"./INPUT/sst_ice_clim.nc"       , .false. ,  0.00' | cat > data_table
touch input.nml
[ ! -d "INPUT" ] && mkdir -p "INPUT"

cp /lustre/f2/scratch/Uriel.Ramirez/wut/MOM6-examples/ice_ocean_SIS2/Baltic/INPUT/grid_spec.nc INPUT/.
cp /lustre/f2/scratch/Uriel.Ramirez/wut/MOM6-examples/ice_ocean_SIS2/Baltic/INPUT/ocean_mosaic.nc INPUT/.
cp /lustre/f2/scratch/Uriel.Ramirez/wut/MOM6-examples/ice_ocean_SIS2/Baltic/INPUT/ocean_hgrid.nc INPUT/.
cp /lustre/f2/pdata/gfdl/gfdl_O/datasets/GOLD_SIS/riga/INPUT/sst_ice_clim.nc INPUT/.

test_expect_success "data_override on grid with 2 halos in x and y" '
  mpirun -n 1 ./test_data_override_ongrid
'

test_done
