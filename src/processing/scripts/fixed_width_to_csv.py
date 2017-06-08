#!/usr/bin/env python
#
# A simple script to convert a state's raw txt files to .csv.
# Used for NC
# Run from the /data folder with:
#     python ../src/processing/fixed_width_to_csv.py NC

from __future__ import print_function
import sys, argparse, os, pandas, csv

state = sys.argv[1]
path_raw = state + '/' + state + '_original_raw/'
path_csv = state + '/' + state + '_original_csv/'
files = os.listdir(path_raw)

for filename in files:
    if 'format.txt' in filename or '.txt' not in filename:
        continue
    
    format_fname = os.path.join(path_raw, filename.replace('.txt', '_format.txt'))
    filename_raw = os.path.join(path_raw, filename)
    output_filename = os.path.join(path_csv, filename.replace('.txt', '.csv'))
    if not os.path.isfile(format_fname):
      print('Cannot find format file: ' + format_fname)
      continue
    
    cols = []
    format_file = open(format_fname)
    
    format_file.readline()
    format_file.readline()
    for line in format_file:
        toks = line.split()
        cols.append({'name': toks[6], 'width': int(float(toks[3]))})
    widths = [x['width'] for x in cols]
    names = [x['name'] for x in cols]
    data = pandas.read_fwf(filename_raw, names = names, widths = widths)
    
    # replace ',' in field values to make export to csv work
    if 'AgencyDescription' in list(data.columns.values):
      data['AgencyDescription'] = data['AgencyDescription'].str.replace(',','')

    data.to_csv(output_filename, sep = ',', quoting=csv.QUOTE_NONE, index=False, quotechar='',escapechar='\\')


