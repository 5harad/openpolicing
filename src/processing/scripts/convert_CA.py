# California has some encoding issues

import os
import sys
import csv
import codecs

state = "CA"
path_raw = state + '/' + state + '_original_raw/'
path_csv = state + '/' + state + '_original_csv/'

# Convert tsv to csv
for file in os.listdir(path_raw):
	if '.txt' in file:
		# make filenames
		fn_in  = path_raw + file
		fn_out = path_csv + file[:-4].replace(' ', '_' ) + '.csv'
		print "Doing", fn_in
		# open writer connection
		f_out = open(fn_out, 'wb')
		writer = csv.writer(f_out, delimiter=',', quoting=csv.QUOTE_MINIMAL)
		# open reader connection and read first line to test
		try:
			f_in = codecs.open(fn_in, 'rb', 'utf-16')
			reader = csv.reader(f_in, delimiter=',', quoting=csv.QUOTE_MINIMAL)
			header = reader.next()
		except UnicodeError:
			f_in = open(fn_in, 'rb')
			reader = csv.reader(f_in, delimiter=',', quoting=csv.QUOTE_MINIMAL)
			header = reader.next()
		# write header
		writer.writerow(header)
		# iterate rows
		for row in reader:
			writer.writerow(row)
		# close file connections
		f_in.close()
		f_out.close()
