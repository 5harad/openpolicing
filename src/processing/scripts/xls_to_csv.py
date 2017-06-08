#!/usr/bin/env python
#
# A simple script to convert a state's raw .xls files to .csv.
# Used for FL, ND, NH, NV, OR, SD, VT, WY
# It depends on ssconvert:
#     http://linuxcommand.org/man_pages/ssconvert1.html
# Run from the /data folder with:
#     python ../src/processing/xls_to_csv.py WY

import os
import sys

state = sys.argv[1]
path_raw = state + '/' + state + '_original_raw/'
path_csv = state + '/' + state + '_original_csv/'

# Convert xls to csv with ssconvert
for file in os.listdir(path_raw):
	if '.xls' in file:
		if '.xlsx' in file:
			# do .xlsx
			base = file[:-5]
			fn_in  = path_raw + base.replace(' ', '\ ') + '.xlsx'
		else:
			# do .xls
			base = file[:-4]
			fn_in  = path_raw + base.replace(' ', '\ ') + '.xls'
		fn_out = path_csv + base.replace(' ', '_' ) + '.csv'
		cmd = 'ssconvert --export-file-per-sheet %s %s' % (fn_in, fn_out)
		print cmd
		os.system(cmd)

# Rename files
for fn_in in os.listdir(path_csv):
	(fn, id) = fn_in.split('.csv.')
	fn_out = '%s_sheet%d.csv' % (fn, int(id)+1)
	print "Rename from %s to %s" % (fn_in, fn_out)
	os.rename(path_csv + fn_in, path_csv + fn_out)
