#!/usr/bin/env python
#
# A simple script to convert a state's raw \t, ~ or | -separated files to .csv.
# Used for IA, IL, SC, WI
# Run from the /data folder with:
#     python ../src/processing/scripts/tsv_to_csv.py WY

import os
import sys
import csv

state = sys.argv[1]
path_raw = state + '/' + state + '_original_raw/'
path_csv = state + '/' + state + '_original_csv/'

# Convert tsv to csv
for file in os.listdir(path_raw):
	if '.txt' in file:
		# make filenames
		fn_in  = path_raw + file
		fn_out = path_csv + file[:-4].replace(' ', '_' ) + '.csv'
		# open file connections
		f_in = open(fn_in, 'rb')
		f_out = open(fn_out, 'wb')
		reader = csv.reader(f_in, delimiter='\t', quoting=csv.QUOTE_NONE)
		writer = csv.writer(f_out, delimiter=',', quoting=csv.QUOTE_MINIMAL)
		print "Doing", fn_in
		# iterate rows
		for row in reader:
			writer.writerow(row)
		# close file connections
		f_in.close()
		f_out.close()
