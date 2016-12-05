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

### Get the program name
prog = os.path.basename (sys.argv [0])

### Initialize lists with the column names
header_response = None
header_presentation = None

### Find files
session_dirs = open ('sessions-list.txt').readlines ()

first_columns = 'subject,experiment,background,stimulus,object.side,table.side,chair'

db_resp = open ('obj-stab-resp.csv', 'w')
db_resp.write (first_columns + ',object,angle,response,reaction.time\n')

db_info = open ('obj-stab-info.csv', 'w')
db_info.write (first_columns + ',date,hour\n')

count = 0

for s in session_dirs:

    f = os.path.join (s.rstrip (), 'info.yaml')
    sys.stdout.flush ()
    time_dir = os.path.dirname (f)

    if os.path.exists (os.path.join (time_dir, 'hide')):
        continue

    info = yaml.load (open (f, 'r'))
    subject = info  ['subject']
    stimulus = info ['stimulus']
    info_str = 'S%03d,%s,%s,%s,%s,%s,%s' % (subject,
                                            info ['experiment'],
                                            info ['background'],
                                            stimulus,
                                            info ['object-side'],
                                            info ['table-side'],
                                            info ['chair'])

    time_str = os.path.basename (time_dir).replace ('-', ':')
    date_dir = os.path.dirname (time_dir)
    date_str = os.path.basename (date_dir)

    if not subject in [999]:

        s = '%s,%s,%s\n' % (info_str, date_str, time_str)
        db_info.write (s)
        sys.stdout.write (".")
        sys.stdout.flush ()
        count += 1

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

sys.stdout.write ("\nProcessed %d sessions\n" % count)
sys.stdout.flush ()
