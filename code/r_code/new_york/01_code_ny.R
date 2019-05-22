
elects <- fread("./raw_data/elects_nys.csv")

# if(db_access){
#   ## read in new york voter file
#   ny <- dbGetQuery(db, "select nys_id, last_name, gender, dob, county_code, political_party,
#                         res_house_number, res_pre_street, res_street_name, res_post_street_dir, res_city,
#                         zip5, history from nys_roll_0319
#                        where voter_status == 'ACTIVE' and registration_date <= 20181012") %>% 
#     mutate_at(vars(res_house_number, res_pre_street, res_street_name, res_post_street_dir), funs(ifelse(is.na(.), "", .))) %>%
#     mutate(street = paste(res_house_number, res_pre_street, res_street_name, res_post_street_dir),
#            street = gsub("\\s+", " ", street),
#            city = res_city,
#            zip = zip5,
#            state = "NY",
#            dem = political_party == "DEM",
#            rep = political_party == "REP",
#            yob = floor(dob / 10000),
#            gender = gender == "F") %>% 
#     select(-dob, -starts_with("res_"), -zip5, -political_party)
#   
#   history <- cSplit(dplyr::select(ny, nys_id, history), "history", sep = ";", direction = "long", type.convert = F)
#   history <- left_join(history, elects, by = "history")
#   
#   primary <- filter(history, year == 2018, grepl("primary", election_type))
#   general <- filter(history, year == 2018, grepl("general", election_type))
#   
#   ny$voted_primary <- ny$nys_id %in% primary$nys_id
#   ny$voted_general <- ny$nys_id %in% general$nys_id
#   
#   ny <- dplyr::select(ny, -history)
#   
#   rm(general, primary, history)
#   ## set up and geocode voter file
#   
#   ny <- geocode(ny) %>% 
#     select(-street, -city, -zip, -state)
#   
#   
#   saveRDS(ny, "./temp/ny_geocoded.RDS")
# }


ny <- readRDS("./temp/ny_geocoded.RDS")
## spatial join to find census tracts and congressional districts
tracts <- readOGR("H:/Public/Democracy/Voting Rights & Elections/data/uncontested/raw_data/tl_2018_36_tract", "tl_2018_36_tract")

pings  <- SpatialPoints(ny[c('longitude','latitude')], proj4string = tracts@proj4string)
ny$tract <- over(pings, tracts)$GEOID

cds <- readOGR("H:/Public/Democracy/Voting Rights & Elections/data/uncontested/raw_data/tl_2018_us_cd116", "tl_2018_us_cd116")
cds <- cds[substring(cds@data$GEOID, 1, 2) == "36", ]

pings  <- SpatialPoints(ny[c('longitude','latitude')], proj4string = cds@proj4string)
ny$cd <- over(pings, cds)$GEOID
rm(tracts, cds, pings)

## use wru to come up with race estimates
ny_census <- get_census_data(key = api_key, state = "NY", age = F, sex = F, census.geo = "tract")
saveRDS(ny_census, "./temp/wru_census_ny.RDS")

ny_census <- readRDS("./temp/wru_census_ny.RDS")

ny <- ny %>%
  rename(surname = last_name,
         tract_full = tract) %>%
  mutate(state_code = substring(tract_full, 1, 2),
         county = substring(tract_full, 3, 5),
         tract = substring(tract_full, 6, 11),
         state = "NY")

ny <- predict_race(ny, census.geo = "tract", census.key = api_key, retry = 999, census.data = ny_census)



### pull down census data from tidycensus

ny_census_data <- get_basic_census_stats(geo = "tract", state = "NY", year = 2017)

ny <- left_join(ny, ny_census_data, by = c("tract_full" = "GEOID"))

### pull in uncontested races
uc <- fread("./raw_data/uncontested_seats_2018.csv") %>% 
  mutate(state = substring(seat, 1, 2),
         seat = substring(seat, 3, 4))
codes <- fips_codes %>% 
  group_by(state) %>% 
  filter(row_number() == 1)
uc <- left_join(uc, codes, by = "state") %>% 
  mutate(seat = paste0(state_code, seat)) %>% 
  select(seat, type)
rm(codes)

ny <- left_join(ny, uc, by = c("cd" = "seat"))
ny$uncontested <- !is.na(ny$type)

## clean up, only keep complete cases for matching procedure
ny <- ny %>% 
  mutate_at(vars(voted_primary, voted_general, rep, dem), funs(ifelse(is.na(.), 0, .))) %>% 
  filter(!is.na(nys_id)) %>% 
  dplyr::select(-type)

ny <- ny[complete.cases(ny), ]

saveRDS(ny, "./temp/new_york_race_census.RDS")
