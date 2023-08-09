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

# Copyright (c) 2019-2020 Ed Hartnett, Seth Underwood

# Set common test settings.
. ../test-lib.sh

# create and enter directory for in/output files
output_dir

cat <<_EOF > diag_table
test_none
2 1 1 0 0 0

"test_none",      6,  "hours", 1, "hours", "time"

"ocn_mod", "var0", "var0_none", "test_none", "all", .false., "none", 2
"ocn_mod", "var1", "var1_none", "test_none", "all", .false., "none", 2
"ocn_mod", "var2", "var2_none", "test_none", "all", .false., "none", 2
"ocn_mod", "var3", "var3_none", "test_none", "all", .false., "none", 2
_EOF

my_test_count=1
printf "&test_reduction_methods_nml \n test_case = 0 \n \n/" | cat > input.nml
test_expect_success "Running diag_manager with "none" reduction method (test $my_test_count)" '
  mpirun -n 6 ../test_reduction_methods
'
test_expect_success "Checking answers for the "none" reduction method (test $my_test_count)" '
  mpirun -n 1 ../check_time_none
'

export OMP_NUM_THREADS=2
printf "&test_reduction_methods_nml \n test_case = 1 \n \n/" | cat > input.nml
test_expect_success "Running diag_manager with "none" reduction method with openmp (test $my_test_count)" '
  mpirun -n 6 ../test_reduction_methods
'
test_expect_success "Checking answers for the "none" reduction method with openmp (test $my_test_count)" '
  mpirun -n 1 ../check_time_none
'
export OMP_NUM_THREADS=1

printf "&test_reduction_methods_nml \n test_case = 2 \n \n/" | cat > input.nml
test_expect_success "Running diag_manager with "none" reduction method with halo output (test $my_test_count)" '
  mpirun -n 6 ../test_reduction_methods
'
test_expect_success "Checking answers for the "none" reduction method with halo output (test $my_test_count)" '
  mpirun -n 1 ../check_time_none
'

test_done
#No need to test the masking because it is not used for instantaneous output