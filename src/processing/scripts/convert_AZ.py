#!/usr/bin/env python
#
# A simple script to convert a state's raw .xls files to .csv.
# Special double-folder case for AZ
# It depends on ssconvert:
#     http://linuxcommand.org/man_pages/ssconvert1.html
# Run from the /data folder with:
#     python ../src/processing/xls_to_csv.py AZ

import os
import sys

state = sys.argv[1]
path_raw = state + '/' + state + '_original_raw/'
path_csv = state + '/' + state + '_original_csv/'

# Convert xls to csv with ssconvert
for file1 in os.listdir(path_raw):
	if len(file1) != 4:
		continue
	path_raw2 = path_raw + '/' + file1 + '/'
	path_csv2 = path_csv + '/' + file1 + '/'
	#os.makedirs(path_csv2)
	for file in os.listdir(path_raw2):
		if '.xls' in file:
			if '.xlsx' in file:
				# do .xlsx
				base = file[:-5]
				fn_in  = path_raw2 + base.replace(' ', '\ ') + '.xlsx'
			else:
				# do .xls
				base = file[:-4]
				fn_in  = path_raw2 + base.replace(' ', '\ ') + '.xls'
			fn_out = path_csv2 + base.replace(' ', '_' ) + '.csv'
			cmd = 'ssconvert --export-file-per-sheet %s %s' % (fn_in, fn_out)
			print cmd
			os.system(cmd)

# Rename files
for file1 in os.listdir(path_csv):
	if len(file1) != 4:
		continue
	path_csv2 = path_csv + '/' + file1 + '/'
	for fn_in in os.listdir(path_csv2):
		(fn, id) = fn_in.split('.csv.')
		fn_out = '%s_sheet%d.csv' % (fn, int(id)+1)
		print "Rename from %s to %s" % (fn_in, fn_out)
		os.rename(path_csv2 + fn_in, path_csv2 + fn_out)
