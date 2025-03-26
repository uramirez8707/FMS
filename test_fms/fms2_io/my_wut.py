import glob
import csv

print("HELLO THERE")

logfiles=glob.glob("*c*.out*")

output=[]
print(logfiles)

for filename in logfiles:
  print("Working on " + filename)
  file_output={}
  file_output['filename'] = filename

  if filename[0] == "F":
    file_output['case'] = "FMS2io"
  else:
    file_output['case'] = "ParallelNetcdf"

  start = filename.find("-") + 1
  end = filename.find("_")
  file_output['grid_resolution'] = filename[start:end]

  start = filename.find("_") + 1
  end = filename[start:].find("_") + start
  layout = filename[start:end].replace(".", " ")
  file_output['layout'] = layout

  start = filename[end+1:].find("_") + end + 2
  end = filename[start:].find(".out") + start
  io_layout = filename[start:end].replace(".", " ")
  file_output['io_layout'] = io_layout

  file_output['iteration']=filename[end+5:]

  with open(filename, 'r') as file:
    for line in file:
      tf = " ".join(str(line).split())
      if "WriteClock" in line:
        lol = tf.split(' ')
        file_output['min'] = lol[2]
        file_output['max'] = lol[3]
      elif "Begin" in line:
        lol = tf.split(' ')
        membegin = lol[4]
      elif "End" in line:
        lol = tf.split(' ')
        memend = lol[4]
    dt_mem = float(memend) - float(membegin)
    file_output['mem'] = round(dt_mem)

  output.append(file_output)

print(output)
with open("wut", mode='w', newline='') as file:
    writer = csv.writer(file)
    writer.writerow(output[0].keys())
    for test_case in output:
      writer.writerow(test_case.values())  # Write rows
