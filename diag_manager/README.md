## Diag Table Yaml Format:

The purpose of this documents is to explain the diag_table yaml format. 

To convert the legacy ascii data_table format to this yaml format, the python script [**diag_table_to_yaml.py**](https://github.com/NOAA-GFDL/fms_yaml_tools/blob/main/diag_table/diag_table_to_yaml.py) can be used. To confirm that your diag_table.yaml was created correctly, the python script [**is_valid_diag_table_yaml.py**](https://github.com/NOAA-GFDL/fms_yaml_tools/blob/main/diag_table/is_valid_diag_table_yaml.py) can be used.

The diag_table.yaml is organized by file.  Each file has the required and optional key/values for the file.  There is subsection for all of the variables in the file.  The hierarchical structure looks like this:

```yaml
title:
base_date:
diag_files:
- file1
  varlist:
  - VarsForFile1
- file2
  varlist:
  - VarsForFile2
```

## Diag table yaml sections

### 1. Global Section
The diag_yaml requires “title” and the “baseDate”.
- The **title** is a string that labels the diag yaml.  The equivalent in the diag table would be the experiment.  It will be recommended that each diag_yaml have a separate title label that is descriptive of the experiment that is using it.
- The **basedate** is an array of 6 integer indicating the base_date in the format [year month day hour minute second].

**Example:** 

In the YAML format:
```yaml
title: ESM4_piControl
base_date: 2022 5 26 12 3 1
```

In the legacy ascii format:
```
ESM4_piControl
2022 5 26 12 3 1
```

### 2. File Section
The files are listed under the diagFiles section as a dashed array. 

Below are the required keys needed to define each file.
- **filename** is a string that defines the name of the file. Avoid adding ".nc" and "tileX" to the filename as this will handle by FMS. 
- **freq** is an integer that defines the frequency that data will be written. The acceptable values are:
  - =-1: output at the end of the run only 
  - =0: output every timestep 
  - \>0: output frequency
- **freq_units** is a string that defines the units of the frequency from above. The acceptable values are seconds, minuts, hours, days, months, years. 
- **time_units** is a string that defines units for time. The acceptable values are seconds, minuts, hours, days, months, years. 
- **unlimdim** is a string that defines the name of the unlimited dimension, usually “time”.
- **varlist** is a subsection that list all of the variable in the file

**Example:** The following creates a file with data written every 6 hours. 

In the YAML format:
```yaml
diag_files:
- file_name: atmos_6hours
  freq: 6
  freq_units: hours
  time_units: hours
  unlimdim: time
  varlist:
  - varinfo
```

In the legacy ascii format:
```
"atmos_6hours",      6,  "hours", 1, "hours", "time"
```

**NOTE:** The fourth column (file_format) has be deprecated. Netcdf files will always be written.

Below are some *optional* keys that may be added. 
- **write_file** is a logical that indicates if you want the file to be created (default is true). This is a new feature that is not supported by the legacy ascii data_table.
- **new_file_freq** is a integer that defines the frequency for closing the existing file
- **new_file_freq_units** is a string that defines the time units for creating a new file. Required if “new_file_freq” used. The acceptable values are seconds, minuts, hours, days, months, years. 
- **start_time** is an array of 6 integer indicating when to start the file for the first time. It is in the format [year month day hour minute second]. Requires “new_file_freq”

**Example:** The following will create a new file every 6 hours starting at Jan 1 2020. Variable data will be written to the file every 6 hours.

In the YAML format:
```yaml
- file_name: ocn%4yr%2mo%2dy%2hr
  freq: 6
  freq_units: hours
  time_units: hours
  unlimdim: time
  new_file_freq: 6
  new_file_freq_units: hours
  start_time: 2020 1 1 0 0 0
```

In the legacy ascii format:
```
"ocn%4yr%2mo%2dy%2hr",      6,  "hours", 1, "hours", "time", 6, "hours", "1901 1 1 0 0 0"
```

**NOTE** If using the new_file_freq, there must be a way to distinguish each file, as it was done in the example above. 

- **file_duration** is an integer that defines how long the file should receive data after start time in “file_duration_units”.  This optional field can only  be used if the start_time field is present.  If this field is absent, then the file duration will be equal to the frequency for creating new files.  NOTE: The file_duration_units field must also be present if this field is present.
- **file_duration_units** is a string that defines the file duration units. The acceptable values are seconds, minuts, hours, days, months, years. 
- **global_meta** is a subsection that lists any additional global metadata to add to the file. This is a new feature that is not supported by the legacy ascii data_table.
- **sub_region** is a subsection that defines the four corners of a subregional section to capture, and the bounds of the z axis.

### 2.1 Variable Section
The variables in each file are listed under the varlist section as a dashed array.

- **var_name:**  is a string that defines the variable name as it is defined in the register_diag_field call in the model
- **reduction:** is a string that describes the data reduction method to perform prior to writing data to disk. Acceptable values are average, diurnalXX, powXX, min, max, none, rms.  
- **module:**  is a string that defines the module where the variable is registered in the model code
- **kind:** is a string that defines the type of variable  as it will be written out in the file. Acceptable values are r4 r8 i4 i8.

**Example:**

In the YAML format:
```yaml
  varlist:
  - module: moist
    var_name: precip
    reduction: average
    kind: r4
```

In the legacy ascii format:
```
"moist",     "precip",                         "precip",           "atmos_8xdaily",   "all", .true.,  "none", 2
```
**NOTE:** The fifth column (time_sampling) has be deprecated. The reduction_method (`.true.`) has been replaced with `average`. The output name was not included in the yaml because it is the same as the var_name. 

which corresponds to the following model code
```F90
id_precip = register_diag_field ( 'moist', 'precip', axes, Time)
```
where:
- `moist` corresonds to the module key in the diag_table.yaml
- `precip` corresponds to the var_name key in the diag_table.yaml
- `axes` are the ids of the axes the variable is a function of
- `Time` is the model time

Below are some *optional* keys that may be added. 
- **write_var:** is a logical that is set to false if the user doesn’t want the variable to be written to the file (default: true).
- **out_name:** is a string that defines the name of the variable that will be written to the file (default same as var_name)
- **long_name:** is a string defining the long_name attribute of the variable. It overwrites the long_name in the variable's register_diag_field call
- **attributes:** is a subsection with any additional metadata to add to variable. This is a new feature that is not supported by the legacy ascii data_table.

### 2.1.1 Variable Metadata Section
Any aditional variable attributes can be added for each varible can be listed under the attributes section as a dashed array. The key is attribute name and the value is the attribute value.

**Example:**

```yaml
    attributes:
    - attribute_name: attribute_value
      attribute_name: attribute_value
```

Although this was not supported by the legacy ascii data_table, with the legacy diag_manager, a call to `diag_field_add_attribute` could have been used to do the same thing.

```F90
call diag_field_add_attribute(diag_field_id, attribute_name, attribute_value)
```

### 2.2 Global Meta Data Section
Any aditional global attributes can be added for each file can be listed under the global_meta section as a dashed array.  The key is the attribute name and the value is the attribute value.

```yaml
  global_meta:
  - attribute_name: attribute_value
    attribute_name: attribute_value
```

### 2.3 Sub_region Section
The sub region can be listed under the sub_region section as a dashed array. The legacy ascii diag_table only allows regions to be defined using the latitude and longitude, and it only allowed rectangular sub regions. With the yaml diag_table, you can use indices to defined the sub_region and you can define **any** four corner shape. Each file can only have 1 sub_region defined. These are keys that can be used:
- **grid_type:** is a string defining the method used to define the  fourth sub_region corners. The acceptable values are "latlon" if using latitude/longitude or "indices" if using the indices of the corners.
- **corner1:** is a 2 member array of reals if using (grid_type="latlon") or integers if using (grid_type="indices") defining the x and y points of the first corner of a sub_grid.
- **corner2:** is a 2 member array of reals if using (grid_type="latlon") or integers if using (grid_type="indices") defining the x and y points of the second corner of a sub_grid.
- **corner3:** is a 2 member array of reals if using (grid_type="latlon") or integers if using (grid_type="indices") defining the x and y points of the third corner of a sub_grid.
- **corner4:** is a 2 member array of reals if using (grid_type="latlon") or integers if using (grid_type="indices") defining the x and y points of the fourth corner of a sub_grid.
- **zbounds:** is a 2 member array of integers that define the bounds of the z axis (zmin, zmin), optional default is no limits. 
- **tile:** is an integer defining the tile number the sub_grid is on. It is **required** only if using (grid_type="indices").

**Exampe:**

```yaml
  sub_region:
  - grid_type: latlon
    corner1: -80, 0
    corner2: -80, 75
    corner3: -60, 0
    corner4: -60, 75
```

### 3. More examples
Bellow is a complete example of diag_table.yaml:
```yaml
title: test_diag_manager
base_date: 2 1 1 0 0 0
diag_files:
- file_name: wild_card_name%4yr%2mo%2dy%2hr
  freq: 6
  freq_units: hours
  time_units: hours
  unlimdim: time
  new_file_freq: 6
  new_file_freq_units: hours
  start_time: 2 1 1 0 0 0
  file_duration: 12
  file_duration_units: hours
  varlist:
  - module: test_diag_manager_mod
    var_name: sst
    reduction: average
    kind: r4
  global_meta:
  - is_a_file: true
- file_name: normal
  freq: 24
  freq_units: days
  time_units: hours
  unlimdim: records
  varlist:
  - module: test_diag_manager_mod
    var_name: sst
    reduction: average
    kind: r4
    write_var: true
    attributes:
    - do_sst: .true.
  sub_region:
  - grid_type: latlon
    corner1: -80, 0
    corner2: -80, 75
    corner3: -60, 0
    corner4: -60, 75
- file_name: normal2
  freq: -1
  freq_units: days
  time_units: hours
  unlimdim: records
  write_file: true
  varlist:
  - module: test_diag_manager_mod
    var_name: sstt
    reduction: average
    kind: r4
    long_name: S S T
  - module: test_diag_manager_mod
    var_name: sstt2
    reduction: average
    kind: r4
    write_var: false
  sub_region:
  - grid_type: index
    tile: 1
    corner1: 10, 15
    corner2: 20, 15
    corner3: 10, 25
    corner4: 20, 25
- file_name: normal3
  freq: -1
  freq_units: days
  time_units: hours
  unlimdim: records
  write_file: false
```
