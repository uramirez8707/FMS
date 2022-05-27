## Diag Table Yaml Format:

The purpose of this documents is to explain the diag_table yaml format. 

To convert the legacy ascii data_table format to this yaml format, the python script [**diag_table_to_yaml.py**](https://github.com/NOAA-GFDL/fms_yaml_tools/blob/main/diag_table/diag_table_to_yaml.py). To confirm that your diag_table.yaml was created correctly, the python script [**is_valid_diag_table_yaml.py**](https://github.com/NOAA-GFDL/fms_yaml_tools/blob/main/diag_table/is_valid_diag_table_yaml.py).

The diag_table.yamlis organized by file.  Each file has the required and optional key/values for the file.  There is subsection for all of the variables in the file.  The hierarchical structure looks like this:

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
The files will be listed under the diagFiles section as a dashed array. 

Below are the required keys needed to define each file.
- **filename** is a string that defines the name of the file. Avoid adding ".nc" and "tileX" to the filename as this will handle by the io. 
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
"atmos_6hours",      6,  "hours", 6, "hours", "time"
```

Below are some *optional* keys that may be added. 
- **write_file** is a logical that indicates if you want the file to be created (default is true).
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
- **file_duration_units**is a string that defines the file duration units. The acceptable values are seconds, minuts, hours, days, months, years. 

### 2.1 Variable Section
### 2.1.1 Variable Metadata Section
### 2.2 Global Meta Data Section
### 2.3 Sub_region Section
