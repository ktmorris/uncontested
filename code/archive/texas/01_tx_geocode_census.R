

if(db_access){
  ## get texas history
  p_vote <- dbGetQuery(db, "select VUID from tx_roll_history_0419 where ELECTION_DATE == '20180306'")
  g_vote <- dbGetQuery(db, "select VUID from tx_roll_history_0419 where ELECTION_DATE == '20181106'")
  dem <- dbGetQuery(db, "select VUID from tx_roll_history_0419 where ELECTION_PARTY == 'DEM'")
  rep <- dbGetQuery(db, "select VUID from tx_roll_history_0419 where ELECTION_PARTY == 'REP'")
  
  ## read in texas voter file
  tx <- dbGetQuery(db, "select VUID, LAST_NAME, GENDER, DOB, PERM_HOUSE_NUMBER, PERM_DESIGNATOR, PERM_STREET_NAME, PERM_STREET_TYPE,
                        PERM_CITY, PERM_ZIPCODE, PERM_DIRECTIONAL_PREFIX, PERM_DIRECTIONAL_SUFFIX, COUNTY_CODE
                        from tx_roll_0419 where STATUS_CODE == 'V' AND EDR__EFFECTIVE_DATE_OF_REGISTRATION_ <= '20181009'")

  ## set up and geocode voter file
  tx <- tx %>% 
    mutate_at(vars(PERM_HOUSE_NUMBER, PERM_DESIGNATOR, PERM_DIRECTIONAL_PREFIX, PERM_STREET_NAME, PERM_STREET_TYPE, PERM_DIRECTIONAL_SUFFIX), funs(ifelse(is.na(.), "", .))) %>% 
    mutate(street = paste(PERM_HOUSE_NUMBER, PERM_DESIGNATOR, PERM_DIRECTIONAL_PREFIX, PERM_STREET_NAME, PERM_STREET_TYPE, PERM_DIRECTIONAL_SUFFIX),
           street = gsub("\\s+", " ", street),
           city = PERM_CITY,
           zip = PERM_ZIPCODE,
           state = "TX") %>% 
    select(-starts_with("PERM")) %>% 
    filter(!is.na(VUID))
  
  tx$voted_primary <- tx$VUID %in% p_vote$VUID
  tx$voted_general <- tx$VUID %in% g_vote$VUID
  tx$dem <- tx$VUID %in% dem$VUID
  tx$rep <- tx$VUID %in% rep$VUID
  
  rm(p_vote, g_vote, dem, rep)
  
  tx <- geocode(tx) %>% 
    select(-street, -city, -zip, -state)
  

  saveRDS(tx, "./temp/tx_geocoded.RDS")
}

tx <- readRDS("./temp/tx_geocoded.RDS")
## spatial join to find census tracts and congressional districts
tracts <- readOGR("H:/Public/Democracy/Voting Rights & Elections/data/uncontested/raw_data/tl_2018_48_tract", "tl_2018_48_tract")

pings  <- SpatialPoints(tx[c('longitude','latitude')], proj4string = tracts@proj4string)
tx$tract <- over(pings, tracts)$GEOID

cds <- readOGR("H:/Public/Democracy/Voting Rights & Elections/data/uncontested/raw_data/tl_2018_us_cd116", "tl_2018_us_cd116")
cds <- cds[substring(cds@data$GEOID, 1, 2) == "48", ]

pings  <- SpatialPoints(tx[c('longitude','latitude')], proj4string = cds@proj4string)
tx$cd <- over(pings, cds)$GEOID
rm(tracts, cds, pings)

## use wru to come up with race estimates
# tx_census <- get_census_data(key = api_key, state = "TX", age = F, sex = F, census.geo = "tract")
# saveRDS(tx_census, "./temp/wru_census_tx.RDS")

tx_census <- readRDS("./temp/wru_census_tx.RDS")

tx <- tx %>%
  rename(surname = LAST_NAME,
         tract_full = tract) %>%
  mutate(state_code = substring(tract_full, 1, 2),
         county = substring(tract_full, 3, 5),
         tract = substring(tract_full, 6, 11),
         state = "TX",
         yob = floor(as.integer(tx$DOB) / 10000))

tx <- predict_race(tx, census.geo = "tract", census.key = api_key, retry = 999, census.data = tx_census)

### pull down census data from tidycensus

tx_census_data <- get_basic_census_stats(geo = "tract", state = "TX", year = 2017)

tx <- left_join(tx, tx_census_data, by = c("tract_full" = "GEOID"))

### pull in uncontested races
## data from wapo https://www.washingtonpost.com/graphics/2018/politics/midterms-uncontested-candidates/
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

tx <- left_join(tx, uc, by = c("cd" = "seat"))
tx$uncontested <- !is.na(tx$type)

## clean up, only keep complete cases for matching procedure
tx <- tx %>% 
  mutate(GENDER = ifelse(GENDER == "F", 1, ifelse(GENDER == "M", 0, 0.5)),
         GENDER = ifelse(is.na(GENDER), 0.5, GENDER)) %>% 
  filter(!is.na(VUID)) %>% 
  dplyr::select(-type)

tx <- tx[complete.cases(tx), ]

saveRDS(tx, "./temp/texas_race_census.RDS")
