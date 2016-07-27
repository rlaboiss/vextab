#!/usr/bin/python

import os
import sys
import fnmatch
import yaml
import csv

class CsvReader:
    def __init__ (self, fname):
        self.header = None
        self.data = []
        try:
            for row in csv.reader (open (fname, 'r')):
                if self.header:
                    self.data.append (row)
                else:
                    self.header = row
        except:
            self.data = None

### Get the program name and the current directory
prog = os.path.basename (sys.argv [0])
cur_dir = os.path.dirname (sys.argv [0])

### Get the directory with the results
results_dir = os.path.join (cur_dir, "..", "experiment", "results")

### Initialize lists with the column names
header_response = None
header_presentation = None

### Find files
info_files = []
for root, dirnames, filenames in os.walk (results_dir):
    for filename in fnmatch.filter (filenames, 'info.yaml'):
        info_files.append(os.path.join (root, filename))

db_resp = open ('obj-stab-resp.csv', 'w')
db_resp.write ('subject,background,stimulus,chair,'
               + 'object,angle,response,reaction.time\n')

db_info = open ('obj-stab-info.csv', 'w')
db_info.write ('subject,background,stimulus,chair,date,hour\n')

for f in info_files:

    time_dir = os.path.dirname (f)

    if os.path.exists (os.path.join (time_dir, 'hide')):
        continue

    info = yaml.load (open (f, 'r'))
    subject = info  ['subject']
    stimulus = info ['stimulus']
    info_str = 'S%03d,%s,%s,%s' % (subject, info ['background'], stimulus,
                                   info ['chair'])

    time_str = os.path.basename (time_dir).replace ('-', ':')
    date_dir = os.path.dirname (time_dir)
    date_str = os.path.basename (date_dir)

    if not subject in [999]:

        s = '%s,%s,%s\n' % (info_str, date_str, time_str)
        db_info.write (s)
        sys.stdout.write (s)
        sys.stdout.flush ()

        d = os.path.dirname (f)
        response = CsvReader (os.path.join (d, 'response.csv'))
        presentation = CsvReader (os.path.join (d, 'presentation.csv'))

        for i in range (len (response.data)):
            pt = float (presentation.data [i] [4])
            if stimulus == 'object':
                db_resp.write ('%s,%s,%f,%s,%f\n'
                               % (info_str,
                                  response.data [i] [0],
                                  float (response.data [i] [2]),
                                  response.data [i] [3],
                                  float (response.data [i] [4]) - pt))
            else:
                db_resp.write ('%s,NA,%f,%s,%f\n'
                               % (info_str,
                                  float (response.data [i] [1]),
                                  response.data [i] [2],
                                  float (response.data [i] [3]) - pt))

db_info.close ()
db_resp.close ()
