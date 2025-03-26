#!/bin/sh
set -e

gather_data(){
  nx=$1
  ny=$2
  nz=$3
  layouts=$4
  io_layouts=$5
  test_case=$6
  ntimes=$7
  niterations=$8

  if [ $test_case = 1 ]
  then
    prefix="F"
  else
    prefix="P"
  fi

  for layout in $layouts
  do
    for io in $io_layouts
    do
      if [ $io = -999 ]
      then
        io_layout=$layout
      else
        io_layout=$io
      fi

      IFS='.' read -r layoutx layouty <<< "$layout"
      npes=$((layoutx * layouty))

      rm -rf input.nml
      rm -rf *.nc*

      lay="${layout//./,}"
      iolay="${io_layout//./,}"

cat <<_EOF > input.nml
&fms_nml
  print_memory_usage = .true.
/

&test_parallel_netcdf_writes_nml
nx = $nx
ny = $ny
nz = $nz
io_layout=$iolay
layout=$lay
test_case=$test_case
ntimes=$ntimes
/
_EOF

      for i in $(seq 1 $niterations); do
        filename="${prefix}-c${nx}L${nz}_${layout}_${io_layout}.out.${i}"
        mpirun -n ${npes} ../test_parallel_netcdf_writes |& tee ${filename}
      done

      if [ ${PIPESTATUS[0]} = 0 ]
      then
        echo "Finished writing data"
      else
        echo "write failure" | tee FAIL
        exit 1
      fi
    done
  done
}

# Shared parameters
ntimes=2
niterations=2

nx=96
ny=$nx
nz=65
layouts="4.24" # 8.24 24.24"
io_layouts="1.1 1.4 -999"

echo "Testing :: c96L65 with fms2io domain writes"
gather_data $nx $ny $nz "$layouts" "$io_layouts" 1 $ntimes $niterations
rm -rf *.nc*

io_layouts="1.1"
echo "Testing :: c96L65 with parallel netcdf writes"
gather_data $nx $ny $nz "$layouts" "$io_layouts" 2 $ntimes $niterations
rm -rf *.nc*

nx=384
ny=$nx
nz=65
layouts="24.24 24.32 24.64 24.96 32.32 32.63 32.96 64.64 64.96 96.96"
io_layouts="1.1"

#gather_data $nx $ny $nz "$layouts" "$io_layouts"

