# Script to geocode a list of locations for a specific state, to retrieve a county.

import os
import json
import sys
import requests
import time

state = 'NJ'
lines = open('in.txt', 'r').readlines()
f_out = open('out.txt', 'w', 0)
for line in lines:
	input_str = line[:-1]
	# contstruct the search string
	search_str = "%s, %s" % (input_str, state)
	search_str = search_str.replace(" ", "%20")
	#print '\n', line,
	# make the request
	url = "https://maps.googleapis.com/maps/api/geocode/json?address=%s" % search_str
	res = requests.get(url).json()
	# try to parse the result
	county = ''
	if 'County' not in county and len(res['results'][0]['address_components']) > 1:
		county = res['results'][0]['address_components'][1]['long_name']
	if 'County' not in county and len(res['results'][0]['address_components']) > 2:
		county = res['results'][0]['address_components'][2]['long_name']
	if 'County' not in county and len(res['results'][0]['address_components']) > 3:
		county = res['results'][0]['address_components'][3]['long_name']
	if 'County' not in county and len(res['results'][0]['address_components']) > 4:
		county = res['results'][0]['address_components'][4]['long_name']
	if 'County' not in county and len(res['results'][1]['address_components']) > 1:
		county = res['results'][1]['address_components'][2]['long_name']
	if 'County' not in county and len(res['results'][1]['address_components']) > 2:
		county = res['results'][1]['address_components'][2]['long_name']
	if 'County' not in county and len(res['results'][1]['address_components']) > 3:
		county = res['results'][1]['address_components'][3]['long_name']
	if 'County' not in county:
		county = 'ERROR'
	#print county
	f_out.write("%s,\"%s\",\"%s\"\n" % (state, input_str, county))
	# wait 1.5 seconds, to not get blocked
	time.sleep(1.5)

f_out.close()
