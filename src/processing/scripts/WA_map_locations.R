##################################################################################################
# README
# 
# This is a one-off script that converts Washington's mileposts to lat/long coordinates.
# 
# WA data has stops on various road types: State highways (S), Interstate highways (I), County roads (C), Park roads (P), and Weigh stations (W).
# For the purpose of this analysis, stops on Park roads, County roads and Weigh stations are removed. 
# Park roads are removed because there are very few of them and they cannot be mapped; weigh stations are removed because they are 
# violations specific to truck weigh stations, not traffic stops; and county roads are removed because when we consulted the 
# Washington State County Road Administration Board (CRAB) they told us county roads could not be unambiguously mapped to location. 
# The mapping code below maps S and I roads separately, either from an external file, or by manual coding (locating roads on google maps).
#
# Step 1.
#   S - external database in "mileposts/wa_state_route_mileposts/", 
#     - short roads not found in database, manually located on google maps: "mileposts/wa_short_routes.csv"
#   I - manually located on google maps: "wa_interstate.csv"
#
# Step 2.
#   Interpolate unknown mileposts (those left over from Step 1) from mileposts on known roads.
#
# Step 3.
#   Load shape files which map lat/long to county. The lat/long files must first be converted to a spacial point object according to the 
#     projection systems in which the lat/long coordinates were encoded. Two systems are used:
#     - State roads use the "Coordinate reference systems (CRS)"
#     - All other roads (I, and short S roads -- anything that was manually coded using google maps) uses a projected 
#       coordinate system that is based on the "wgs84 datum"
#
# Step 4. 
#   Stops on the border of the state must be fixed. These are not recognized as being in WA, although they exist in our database
#   These are manually coded on google maps: "mileposts/wa_border_stops.csv"
#
# Step 5. 
#   Combine together all interstate and state roads and save to file.
##################################################################################################

# Set-up
this_state <- 'WA'
change_path(this_state)
load('../tmp/raw_data_with_ids.RData') # outputted by scripts/WA.R script 

##################################################################################################
#                                STATE HIGHWAYS "S"
#
# 17 roads not found in shape file:
#     "CMP" "NSC" "WSU" "97A" "28B" "285" "543" "523" "171" "304" "433"  
#     "528" "599" "20S" "519" "901" "906" "704" "513"
#
# change road numbers to match "maker" database 
#        97A --> 097AR
#        28B --> 028
#        20S --> 020SPANACRT
#
# short S roads, found manually on google maps. For these roads, take midpoint of road as lat long coordinates
# 92, 285, 543, 523, 171, 304, 433, 528, 599, 519, 906, 704, 513
# 
# S roads not found: 901, CMP, WSU, NSC
#
# re-code milepost_id and edit road numbers to match "marker" database
##################################################################################################

washington <- grouped_d %>%
  mutate(road_number = as.character(road_number),
         road_number = ifelse(nchar(road_number)==1, str_pad(road_number, 2, pad = "0"), road_number),
         road_number = ifelse(nchar(road_number)==2, str_pad(road_number, 3, pad = "0"), road_number),
         road_number = replace(road_number, road_number == "97A", "097AR"),
         road_number = replace(road_number, road_number == "28B", "028"),
         road_number = replace(road_number, road_number == "20S", "020SPANACRT"), 
         milepost_id = str_c(highway_type, "-", road_number, "-", milepost))



# ------------------------------------------------------------------------------------------------
# load state milepost data
# ------------------------------------------------------------------------------------------------
state_markers <- readOGR(dsn='../mileposts/wa_state_route_mileposts/', layer='wastate_mp') %>% as.data.frame()

# ------------------------------------------------------------------------------------------------ 
# MP_VALUE: Milepost Value, indicates the number that appears on the milepost marker.
#
# DIRECTION: For data with source code of 1, indicates the direction of travel the data was collected in. 
#            For data with source code of 2, indicates the 24k LRS dataset used.
#            i: Data collected in the increasing lane of travel with GPS or placed on a increasing 24k LRS.
#            d: Data collected in the decreasing lane of travel with GPS or placed on a decreasing 24k LRS.
#            b: Data collected in both directions of travel using GPS for a single milepost marker.
#
# SOURCE: Indicates how the X,Y coordinates where obtained.
#         1 - Data collected with GPS
#         2 - X,Y coordinates derived from the 24k LRS database
#
# SR:     Washington State Highways are defined by 3 descriptors that uniquely identify every piece of highway system: 
#         1) State Route (SR) number (a three-digit number (e.g., 002, 020, 509 etc.), Alternate Route (AR))
#         2) Related Roadway Type (RRT) (two-character abbreviation for a type of roadway, eg. Couplet (CO), Reversible Lane (RL)) 
#         3) Related Roadway Qualifier (RRQ) (maximum six-character field that further uniquely identifies the RRT, (e.g., 101COABERDN)
#
# source: http://www.wsdot.wa.gov/mapsdata/geodatacatalog/Maps/noscale/DOT_TDO/StateRouteMilepostMarkersofWashingtonState.htm#Entity_and_Attribute_Information
# ------------------------------------------------------------------------------------------------ 

# create milepost id
state_markers <- state_markers %>% 
  select(MP_VALUE, SR, LONGITUDE, LATITUDE) %>% 
  rename(road_number = SR,
         milepost    = MP_VALUE,
         longitude   = LONGITUDE,
         latitude    = LATITUDE) %>%
  mutate(road_number = as.character(road_number),
         milepost    = as.character(milepost),
         milepost_id = str_c("S-", road_number, "-", milepost))

# we have multiple mileposts with same ID for each direction; group these together + take average. 
wa_state <- state_markers %>% 
  group_by(milepost_id) %>%
  summarise(road_number = first(road_number), 
            milepost    = first(milepost), 
            longitude   = mean(longitude, na.rm = TRUE), 
            latitude    = mean(latitude, na.rm = TRUE)) %>% ungroup()
            

# ------------------------------------------------------------------------------------------------
# load interstate milepost data
# format road_number with leading zeros
# ------------------------------------------------------------------------------------------------
wa_interstate <- read.csv('../mileposts/wa_interstate.csv', header=TRUE)

wa_interstate <- wa_interstate %>% 
  mutate(road_number = replace(road_number, grepl("I-082", milepost_id), "082"),
         road_number = replace(road_number, grepl("I-005", milepost_id), "005"),
         road_number = replace(road_number, grepl("I-05E", milepost_id), "05E"),
         road_number = replace(road_number, grepl("I-090", milepost_id), "090"),
         road_number = replace(road_number, grepl("I-90E", milepost_id), "90E"))


# ------------------------------------------------------------------------------------------------
# join state & interstate markers
# ------------------------------------------------------------------------------------------------
markers <- rbind(wa_state, wa_interstate)

markers_for_join <- markers %>% select(milepost_id, longitude, latitude)


# ------------------------------------------------------------------------------------------------
# add latitude/longitude to dataset
# ------------------------------------------------------------------------------------------------
washington <- left_join(washington, markers_for_join, by = "milepost_id") 


# ------------------------------------------------------------------------------------------------
# add short state routes
# ------------------------------------------------------------------------------------------------
short_routes <- read.csv('../mileposts/wa_short_routes.csv', header=TRUE)

short_routes <- short_routes %>%
  mutate(road_number = as.character(road_number),
         road_number = replace(road_number, nchar(road_number)==2, "092")) %>%
  select(road_number, latitude, longitude)

for(i in 1:nrow(short_routes)){
  info <- short_routes[i,]
  washington$longitude[washington$road_number == info$road_number] <- info$longitude
  washington$latitude[washington$road_number == info$road_number]  <- info$latitude
}



##################################################################################################
# Estimate missing mileposts
##################################################################################################

find_nearest_milepost <- function(washington, markers, road_type){
  # this function finds the nearest milepost marker for mileposts which are missing, interpolating from known mileposts. 
  # arguments: the stops dataframe (washington); the dataframe of known markers (markers); and the road type (I or S)
  stopifnot(road_type %in% c('I', 'S'))
  missing_location_info = filter(washington, highway_type == road_type & is.na(latitude)) # find the stops which are missing location information. 
  missing_roads       <- unique(missing_location_info$road_number)
  roads_available <- unique(filter(markers, str_sub(milepost_id, 1, 1) == road_type)$road_number)
  
  # roads not in washington database are unknown_roads. There are very few of these. 
  unknown_roads <- setdiff(missing_roads, roads_available)
  known_roads   <- setdiff(missing_roads, unknown_roads)
  count <- 0
  df <- data.frame(milepost_id_orig = numeric(),
                   milepost_id = numeric(),
                   error = numeric(),
                   stringsAsFactors=FALSE) 
  for(i in 1:length(known_roads)){
    road <- known_roads[i]
    
    # get vector of mileposts missing for road and mileposts available for road in database
    needed_mileposts  <- unique(as.character(filter(missing_location_info, road_number == road)$milepost))
    all_mileposts     <- unique(filter(markers, road_number == road, str_sub(milepost_id, 1, 1) == road_type)$milepost)
    missing_mileposts <- needed_mileposts[!(needed_mileposts %in% all_mileposts)]
    
    l <- length(all_mileposts)
    
    # find closest milepost marker. 
    for (j in 1:length(missing_mileposts)){
      count <- count + 1
      
      mile <- as.character(missing_mileposts[j])
      milepost_id_orig <- str_c(road_type, "-", road, "-", mile)
      diff <- abs(as.numeric(rep(mile,l))- as.numeric(all_mileposts))
      
      # index of closest milepost 
      closest_mile_index <- match(min(diff), diff)
      closest_mile <- all_mileposts[closest_mile_index]
      
      # estimated milepost id 
      milepost_id <- str_c(road_type, "-", road, "-", closest_mile)
      
      df[count,] <- c(milepost_id_orig, milepost_id, min(diff))
      message(sprintf("Marker %i for roadtype %s has been remapped (%i away from true marker).", count, road_type, min(diff)))
    }
  }
  df = filter(df, error < 5)
  return(df)
  #some of these missing mileposts are very far away from the mileposts they're being matched to. 
}

df_state      = find_nearest_milepost(washington, markers, road_type = 'S')
df_interstate = find_nearest_milepost(washington, markers, road_type = 'I')

# ------------------------------------------------------------------------------------------------
# add estimated mileposts to database
# ------------------------------------------------------------------------------------------------
estimated_mileposts <- rbind(df_state, df_interstate)

estimated_mileposts <- left_join(estimated_mileposts, markers_for_join, by = "milepost_id") # add latitude / longitude info. 

estimated_mileposts <- estimated_mileposts %>%
  select(-milepost_id) %>%
  rename(milepost_id = milepost_id_orig, 
         interpolated_latitude = latitude, 
         interpolated_longitude = longitude) #rename these fields so we don't get confused with original values. 

washington <- left_join(washington, estimated_mileposts, by = "milepost_id")

washington <- washington %>%
  mutate(longitude = ifelse(is.na(longitude), interpolated_longitude, longitude),
         latitude = ifelse(is.na(latitude), interpolated_latitude, latitude))




##################################################################################################
# load county polygons, state routes 
# map all mileposts to county
##################################################################################################
state_markers <- readOGR(dsn='../mileposts/wa_state_route_mileposts/', layer='wastate_mp')
wa_counties   <- readOGR(dsn='../mileposts/gz_2010_us_050_00_5m/gz_2010_us_050_00_5m/', layer='gz_2010_us_050_00_5m') %>% subset(STATE == "53")


# ------------------------------------------------------------------------------------------------
# split trooper dataset by projection type (lat/long vs. state route file)
# ------------------------------------------------------------------------------------------------
short_roads <- c("285", "543", "523", "171", "304", "433", "528", "599", "519", "906", "704", "513")

state_geo <- washington %>%
  filter(highway_type == "S")  %>%
  filter(! is.na(latitude)) %>%
  filter(! road_number %in% short_roads)

other_geo <- washington %>% 
  filter(highway_type == "I" | (highway_type=="S" & road_number %in% short_roads)) %>%
  filter(! is.na(latitude))   

# stops with no latitude/longitude 
missing_roads <- subset(washington, is.na(latitude))
missing_roads$county_name <- NA
missing_roads$county_fips <- NA


# ------------------------------------------------------------------------------------------------
# state stops
# ------------------------------------------------------------------------------------------------
# bring lat/long together in a data frame & turn into a Spatial Point object
state_points <- as.data.frame(cbind(as.numeric(state_geo$longitude), as.numeric(state_geo$latitude)))
names(state_points) <- c("longitude", "latitude")

# assign a projection so that R knows how the lon/lat coords are set up
coordinates(state_points) <- c("longitude", "latitude")

# assign project to that of the file from which they were taken
proj4string(state_points) <- proj4string(state_markers)

# transform the Coordinate reference systems (CRS) to match that in the county file
state_points <- spTransform(state_points, CRS(proj4string(wa_counties)))

# find which county (polygon) each point is in 
state_point_county <- over(state_points, wa_counties)

# add county code, name & area to database
state_geo$county_fips <- state_point_county$COUNTY
state_geo$county_name <- state_point_county$NAME

# check
county_na <- state_geo %>%
  select(milepost_id, highway_type, road_number, latitude, longitude, county_fips, county_name) %>%
  filter(is.na(county_fips))

unique(county_na$highway_type)
unique(county_na$road_number)

# ------------------------------------------------------------------------------------------------
# fix stops on border
# state highway "S" & road numbers: "101" "020" "401" "112" "011"
# ------------------------------------------------------------------------------------------------
border_stops <- read.csv('../mileposts/wa_border_stops.csv', header=TRUE)

border_stops <- border_stops %>% 
  mutate(road_number = as.character(road_number),
         county_fips = str_pad(as.character(county_fips), 3, pad = '0'),
         road_number = str_pad(as.character(road_number), 3, pad = '0'))

# add missing county FIPS & name
for(i in 1:nrow(border_stops)){
  info <- border_stops[i,]
  state_geo$county_fips[(state_geo$highway_type=="S") & (state_geo$road_number == info$road_number) & (state_geo$milepost==info$milepost)] <- info$county_fips
  state_geo$county_name[(state_geo$highway_type=="S") & (state_geo$road_number == info$road_number) & (state_geo$milepost==info$milepost)] <- info$county_name
}

# check
county_na <- state_geo %>%
  select(milepost_id, highway_type, road_number, latitude, longitude, county_fips, county_name) %>%
  filter(is.na(county_fips))

unique(county_na$highway_type)
unique(county_na$road_number)


# ------------------------------------------------------------------------------------------------
# interstate stops
# ------------------------------------------------------------------------------------------------
geo_points        <- as.data.frame(cbind(as.numeric(other_geo$longitude), as.numeric(other_geo$latitude)))
names(geo_points) <- c("longitude", "latitude")
geo_points        <- geo_points %>% filter(! is.na(longitude))
coordinates(geo_points) <- c("longitude", "latitude")

# assign project to that of the file from which they were taken
# Google Maps is in a projected coordinate system that is based on the wgs84 datum
proj4string(geo_points) <- CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84
                                      +datum=WGS84 +no_defs +towgs84=0,0,0")

# transform the geocoding to match that in the county file
geo_points <- spTransform(geo_points, CRS(proj4string(wa_counties)))

# find which county (polygon) each point is in 
interstate_point_county <- over(geo_points, wa_counties)

# add county code, name & area to database
other_geo$county_fips <- interstate_point_county$COUNTY
other_geo$county_name <- interstate_point_county$NAME


# ------------------------------------------------------------------------------------------------
# join state, interstate & country roads
# save to file
# ------------------------------------------------------------------------------------------------
WA <- rbind(state_geo, other_geo)
WA <- rbind(WA, missing_roads)

wa <- WA %>%
  mutate(county_name = as.character(county_name)) %>%
  group_by(milepost_id) %>%
  summarise(longitude = mean(longitude),
            latitude  = mean(latitude),
            milepost = first(milepost), 
            road_number = first(road_number), 
            highway_type = first(highway_type),
            county_fips = ifelse(length(unique(county_fips))==1, unique(county_fips), 'x'),
            county_name = ifelse(length(unique(county_name))==1, unique(county_name), 'x')) %>%
  filter(! is.na(milepost_id)) %>%
  filter(! is.na(county_name)) %>%
  as.data.frame()

# Add milepost_ids for country road stops with no milepost marker (since these are short roads which are all in the same county anyway)
country_d = filter(wa, highway_type == 'C') %>%
  mutate(milepost = NA)
non_duplicates = !duplicated(country_d[,c('highway_type','road_number','milepost')])
country_d = country_d[non_duplicates,]
country_d$milepost_id = paste(country_d$highway_type, 
                                  country_d$road_number, 
                                  country_d$milepost, sep = '-')

# Save to file.
wa = rbind(country_d, wa)

write.csv(wa, file='../tmp/wa_location.csv')
