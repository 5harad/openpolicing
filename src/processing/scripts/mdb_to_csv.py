#!/usr/bin/env python
#
# A simple script to convert a state's raw .mdb files to .csv.
# Used for NE, MO, TX
# The source is based on this gist:
#     https://gist.github.com/mywarr/9908044
# It depends on the mdbtools suite:
#     http://sourceforge.net/projects/mdbtools/
 
import sys
import subprocess

DATABASE = sys.argv[1]

# Get the list of table names with "mdb-tables"
table_names = subprocess.Popen(["mdb-tables", "-1", DATABASE], 
							   stdout=subprocess.PIPE).communicate()[0]
tables = table_names.split('\n')

# Dump each table as a CSV file using "mdb-export",
# converting " " in table names to "_" for the CSV filenames.
for table in tables:
	if table != '':
		filename = table.replace(" ","_") + ".csv"
		file = open(filename, 'w')
		print("Dumping " + table)
		contents = subprocess.Popen(["mdb-export", DATABASE, table],
									stdout=subprocess.PIPE).communicate()[0]
		file.write(contents)
		file.close()
