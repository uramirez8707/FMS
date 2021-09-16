printf "&test_performance_nml \n nlat=2881 \n nlon=1921 \n nz=64 \n layout=40,30 \n io_layout=1,1/" | cat > input.nml
echo "io_layout = 1,1: OLDIO"
srun -n 1200 ./test_performance_fmsio

echo "io_layout = 1,1: NEWIO"
srun -n 1200 ./test_performance

rm -rf `find -name "*.nc*"`

echo "io_layout = 1,6: NEWIO"
printf "&test_performance_nml \n nlat=2881 \n nlon=1921 \n nz=64 \n layout=40,30 \n io_layout=1,6/" | cat > input.nml
srun -n 1200 ./test_performance
rm -rf `find -name "*.nc*"`

echo "io_layout = 4,6: NEWIO"
printf "&test_performance_nml \n nlat=2881 \n nlon=1921 \n nz=64 \n layout=40,30 \n io_layout=4,6/" | cat > input.nml
srun -n 1200 ./test_performance
rm -rf `find -name "*.nc*"`

echo "io_layout = 40,30: NEWIO"
printf "&test_performance_nml \n nlat=2881 \n nlon=1921 \n nz=64 \n layout=40,30 \n io_layout=40,30/" | cat > input.nml
srun -n 1200 ./test_performance
rm -rf `find -name "*.nc*"`

