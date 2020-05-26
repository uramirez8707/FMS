program main

use fms2_io_mod
use, intrinsic :: iso_fortran_env

type(FmsNetcdfFile_t) :: fileobj

call fms2_io_init()

!! The file type for default.nc should be whatever is set in the namelist
if (open_file(fileobj, "default.nc", "overwrite", is_restart=.true.)) then

   call register_axis(fileobj, "Time", unlimited)
   call register_field(fileobj, "Time", "double", (/"Time"/))
   call register_variable_attribute(fileobj, "Time", "units", "time level")
   call register_variable_attribute(fileobj, "Time", "long_name", "Time")
   call register_variable_attribute(fileobj, "Time", "cartesian_axis", "T")
   call write_data(fileobj, "Time", 1)

   call close_file(fileobj)
endif

!! The file type for ncformat_64bit.nc should be 64 bit, whatever is set in the
!! namelist is overwritten

if (open_file(fileobj, "ncformat_64bit.nc", "overwrite", nc_format="64bit", is_restart=.true.)) then

   call register_axis(fileobj, "Time", unlimited)
   call register_field(fileobj, "Time", "double", (/"Time"/))
   call register_variable_attribute(fileobj, "Time", "units", "time level")
   call register_variable_attribute(fileobj, "Time", "long_name", "Time")
   call register_variable_attribute(fileobj, "Time", "cartesian_axis", "T")
   call write_data(fileobj, "Time", 1)

   call close_file(fileobj)
endif

!! The file type for ncformat_classic.nc should be classic, whatever is set in the
!! namelist is overwritten

if (open_file(fileobj, "ncformat_classic.nc", "overwrite", nc_format="classic", is_restart=.true.)) then

   call register_axis(fileobj, "Time", unlimited)
   call register_field(fileobj, "Time", "double", (/"Time"/))
   call register_variable_attribute(fileobj, "Time", "units", "time level")
   call register_variable_attribute(fileobj, "Time", "long_name", "Time")
   call register_variable_attribute(fileobj, "Time", "cartesian_axis", "T")
   call write_data(fileobj, "Time", 1)

   call close_file(fileobj)
endif

!! The file type for ncformat_netcdf4.nc should be netcdf4, whatever is set in the
!! namelist is overwritten

if (open_file(fileobj, "ncformat_netcdf4.nc", "overwrite", nc_format="netcdf4", is_restart=.true.)) then

   call register_axis(fileobj, "Time", unlimited)
   call register_field(fileobj, "Time", "double", (/"Time"/))
   call register_variable_attribute(fileobj, "Time", "units", "time level")
   call register_variable_attribute(fileobj, "Time", "long_name", "Time")
   call register_variable_attribute(fileobj, "Time", "cartesian_axis", "T")
   call write_data(fileobj, "Time", 1)

   call close_file(fileobj)
endif

end program main
