# Processing code for Georgia

# Set-up
this_state <- 'GA'
change_path(this_state)

# Read in and combine data
print(sprintf("[%s] reading in the data", this_state))


# column names draft
colnames(d)[1] = 'id'
colnames(d)[4] = 'date_time'
colnames(d)[10] = 'county'
colnames(d)[13] = 'agency'
colnames(d)[14] = 'birthdate'
colnames(d)[17] = 'sex'
colnames(d)[20] = 'hair_color'
colnames(d)[21] = 'eye_color'
colnames(d)[25] = 'license_plate_state'
colnames(d)[26] = 'vehicle_commercial'
colnames(d)[29] = 'vehicle_year'
colnames(d)[30] = 'vehicle_make'
colnames(d)[31] = 'vehicle_model'
colnames(d)[33] = 'vehicle_color'
colnames(d)[42] = 'location'
colnames(d)[48] = 'lat'
colnames(d)[49] = 'long'
colnames(d)[53] = 'officer_rank'
colnames(d)[54] = 'officer_last_name'
colnames(d)[55] = 'officer_first_name??'
colnames(d)[56] = 'officer_id??'
colnames(d)[103] = 'warning,violation'
