
## read in new york voter file
wi <- dbGetQuery(db, "select Voter_Reg_Number, LastName, FirstName, County,
                      Address1, Address2, November2018, August2018, ApplicationDate, Voter_Status from wi_roll_0319
                     where Voter_Status == 'Active'") %>%
  filter(as.Date(gsub(" [A-z ]*", "" , ApplicationDate), "%m/%d/%Y") <= "2018-10-17") %>% 
  mutate(street = Address1,
         city = gsub(" WI.*$", "", Address2),
         zip = gsub(".*WI", "", Address2),
         state = "WI") %>%
  select(-Address2, -Address1)


saveRDS(wi, "./temp/raw_wi.rds")


wi <- readRDS("./temp/raw_wi.rds")

wi <- geocode(wi)












history <- cSplit(dplyr::select(ny, nys_id, history), "history", sep = ";", direction = "long", type.convert = F)
history <- left_join(history, elects, by = "history")

primary <- filter(history, year == 2018, grepl("primary", election_type))
general <- filter(history, year == 2018, grepl("general", election_type))

ny$voted_primary <- ny$nys_id %in% primary$nys_id
ny$voted_general <- ny$nys_id %in% general$nys_id

ny <- dplyr::select(ny, -history)

rm(general, primary, history)
## set up and geocode voter file

ny <- geocode(ny) %>%
  select(-street, -city, -zip, -state)


saveRDS(ny, "./temp/ny_geocoded.RDS")

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
