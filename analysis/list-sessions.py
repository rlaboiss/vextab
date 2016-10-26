#!/usr/bin/python

import os
import sys
import fnmatch

### Get the program name and the current directory
prog = os.path.basename (sys.argv [0])
cur_dir = os.path.dirname (sys.argv [0])

### Get the directory with the results
results_dir = os.path.join (cur_dir, "..", "experiment", "results")

### Find files
info_files = []
for root, dirnames, filenames in os.walk (results_dir):
    for filename in fnmatch.filter (filenames, 'info.yaml'):
        print root
