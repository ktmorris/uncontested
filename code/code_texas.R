

if(db_access){
  ## get texas history
  history <- dbGetQuery(db, "select * from tx_roll_history_0419")
  
  history <- history %>% 
    mutate(voted_primary = ELECTION_DATE == "20180306",
           voted_general = ELECTION_DATE == "20181106",
           dem = ELECTION_PARTY == "DEM" & !is.na(ELECTION_PARTY),
           rep = ELECTION_PARTY == "REP" & !is.na(ELECTION_PARTY)) %>% 
    group_by(VUID) %>% 
    summarize(voted_primary = sum(voted_primary, na.rm = T),
              voted_general = sum(voted_general, na.rm = T),
              dem = sum(dem, na.rm = T),
              rep = sum(rep, na.rm = T))
  
  ## read in texas voter file
  tx <- dbGetQuery(db, "select VUID, LAST_NAME, GENDER, DOB, PERM_HOUSE_NUMBER, PERM_DESIGNATOR, PERM_STREET_NAME, PERM_STREET_TYPE,
                        PERM_CITY, PERM_ZIPCODE, PERM_DIRECTIONAL_PREFIX, PERM_DIRECTIONAL_SUFFIX from tx_roll_0419 where STATUS_CODE == 'V'")

  ## set up and geocode voter file
  tx <- tx %>% 
    mutate(street = paste("-", PERM_HOUSE_NUMBER, PERM_DESIGNATOR, PERM_DIRECTIONAL_PREFIX, PERM_STREET_NAME, PERM_STREET_TYPE, PERM_DIRECTIONAL_SUFFIX, "-", sep = "-"),
           street = gsub("\\s+", " ", str_trim(gsub("-", " ", gsub("-NA-", " ", street)))),
           street = gsub(" NA ", " ", street),
           city = PERM_CITY,
           zip = PERM_ZIPCODE,
           state = "TX") %>% 
    select(-starts_with("PERM"))
  
  tx <- geocode(tx) %>% 
    select(-street, -city, -zip, -state)
  
  tx <- left_join(tx, history, by = "VUID")
  rm(history)
  
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

saveRDS(tx, "texas_race_census.RDS")

