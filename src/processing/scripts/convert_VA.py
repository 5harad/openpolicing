#!/usr/bin/env python
#
# Custom script for Virginia's strange encoding format
# Used for VA

import os
import sys
import csv

f_in  = open('VA_original_raw/Stanford Traffic Data.txt', 'rb')
f_out = open('VA_original_csv/data.csv', 'wb')
writer = csv.writer(f_out, delimiter=',', quoting=csv.QUOTE_MINIMAL)

i = 0
while True:
	line = f_in.readline()
	if not line:
		break
	i += 1
	# get header
	if i == 3:
		row = [x.strip().replace(' ', '_') for x in line[1:].split('.')][:-1]
		writer.writerow(row)
	# don't read other header rows
	if i < 5:
		continue
	# parse line
	row = [x.strip() for x in line[1:].split(',')][:-1]
	writer.writerow(row)

f_in.close()
f_out.close()
