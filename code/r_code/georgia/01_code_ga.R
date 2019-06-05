

if(db_access){
  ## get ga history
  hist = read_fwf(
    file = "D:/rolls/georgia/history/2018.TXT",
    fwf_widths(c(3, 8, 8, 3, 2, 1, 1, 1)),
    col_types = "iidccccc"
  )
  
  colnames(hist) <- c("county", "voter_id", "election_date", "election_type", "party", "absentee", "provisional", "supplemental")
  
  p_votes <- select(filter(hist, election_date == 20180522), county, voter_id, party) %>% 
    mutate(dem = party == "D",
           rep = party == "R") %>% 
    group_by(county, voter_id) %>% 
    summarize(dem = max(dem, na.rm = T),
              rep = max(rep, na.rm = T)) %>% 
    mutate(dem = ifelse(dem == -Inf, 0, dem),
           rep = ifelse(rep == -Inf, 0, rep),
           voted_primary = T)
  
  g_votes <- select(filter(hist, election_date == 20181106), county, voter_id)
  g_votes$voted_general <- T
  
  g_votes <- g_votes %>% 
    group_by(county, voter_id) %>% 
    filter(row_number() == 1)
  
  ## read in florida voter file
  ga <- dbGetQuery(db, "select COUNTY_CODE, REGISTRATION_NUMBER, GENDER, BIRTHDATE, RACE_DESC,
                        RESIDENCE_HOUSE_NUMBER, RESIDENCE_STREET_NAME, RESIDENCE_STREET_SUFFIX,
                        RESIDENCE_CITY, RESIDENCE_ZIPCODE, LAST_NAME, CONGRESSIONAL_DISTRICT
                           from ga_roll_0319
                       where VOTER_STATUS == 'A' and REGISTRATION_DATE <= 20181009") %>% 
    mutate_at(vars(RESIDENCE_HOUSE_NUMBER, RESIDENCE_STREET_NAME, RESIDENCE_STREET_SUFFIX), funs(ifelse(is.na(.), "", .))) %>%
    mutate(street = paste(RESIDENCE_HOUSE_NUMBER, RESIDENCE_STREET_NAME, RESIDENCE_STREET_SUFFIX),
           street = gsub("\\s+", " ", street),
           state = "GA",
           yob = BIRTHDATE,
           gender = GENDER == "F",
           white = RACE_DESC == "White not of Hispanic Origin",
           black = RACE_DESC == "Black not of Hispanic Origin",
           latino = RACE_DESC == "Hispanic") %>% 
    rename(city = RESIDENCE_CITY,
           zip = RESIDENCE_ZIPCODE,
           voter_id = REGISTRATION_NUMBER,
           surname = LAST_NAME,
           county = COUNTY_CODE,
           cd = CONGRESSIONAL_DISTRICT) %>% 
    select(-GENDER, -BIRTHDATE, -RACE_DESC, -RESIDENCE_HOUSE_NUMBER, -RESIDENCE_STREET_NAME, -RESIDENCE_STREET_SUFFIX)
  
  ga <- left_join(ga, p_votes, by = c("county", "voter_id")) %>% 
    mutate(dem = ifelse(is.na(dem), 0, dem),
           rep = ifelse(is.na(rep), 0, rep),
           voted_primary = ifelse(is.na(voted_primary), F, voted_primary))

  ga <- left_join(ga, g_votes, by = c("county", "voter_id")) %>% 
    mutate(voted_general = ifelse(is.na(voted_general), F, voted_general))
  ## set up and geocode voter file
  
  ga <- geocode(ga) %>% 
    select(-street, -city, -zip, -state)
  
  saveRDS(ga, "./temp/ga_geocoded.RDS")
  rm(hist, p_votes, g_votes)
}

ga <- readRDS("./temp/ga_geocoded.RDS")
## spatial join to find census tracts and congressional districts
tracts <- readOGR("./raw_data/tl_2018_13_tract", "tl_2018_13_tract")

pings  <- SpatialPoints(ga[c('longitude','latitude')], proj4string = tracts@proj4string)
ga$tract_full <- over(pings, tracts)$GEOID


### pull down census data from tidycensus

ga_census_data <- get_basic_census_stats(geo = "tract", state = "GA", year = 2017)

ga <- left_join(ga, ga_census_data, by = c("tract_full" = "GEOID"))

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

ga$cd <- as.character(paste0("13", str_pad(ga$cd, width = 2, side = "left", pad = "0")))

ga <- left_join(ga, uc, by = c("cd" = "seat"))
ga$uncontested <- !is.na(ga$type)

## clean up, only keep complete cases for matching procedure
ga <- ga %>% 
  filter(!is.na(voter_id),
         cd != "1399999") %>% 
  dplyr::select(-type)

ga <- ga[complete.cases(ga), ]

saveRDS(ga, "./temp/georgia_race_census.RDS")
