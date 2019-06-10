
ca <- dbGetQuery(db, "select * from ca_roll_0619 limit 1")
if(db_access){
  ## get voter history
  # history <- fread("D:/rolls/california/060719/836-37163-59-pvrdr-vph-20190530-0831.TXT", select = c(1, 2, 6))
  # 
  # history <- history[history$ElectionDate %in% c("2018-06-05", "2018-11-06"), ]
  # 
  # history$v_primary <- history$ElectionDate == "2018-06-05"
  # history$v_general <- history$ElectionDate == "2018-11-06"
  # 
  # history <- history %>% 
  #   group_by(CountyCode, RegistrantID) %>% 
  #   summarize(v_primary = max(v_primary, na.rm = T),
  #             v_general = max(v_general, na.rm = T))
  # saveRDS(history, "./temp/ca_hist.rds")
  history <- readRDS("./temp/ca_hist.rds")
  
  ## read in california voter file
  ca <- dbGetQuery(db, "select RegistrantID, LastName, Gender, DOB, CountyCode, PartyCode,
                        AddressNumber, StreetDirPrefix, StreetName, StreetType, StreetDirSuffix,
                        City, Zip
                        from ca_roll_0619
                        where Status == 'A' and
                        (RegistrationDate <= '2018-10-22' or VoterStatusReasonCodeDesc == 'Updated Registration')") %>% 
    mutate_at(vars(AddressNumber, StreetDirPrefix, StreetName, StreetType, StreetDirSuffix), funs(ifelse(is.na(.), "", .))) %>%
    mutate(state = "CA",
           street = trimws(paste(AddressNumber, StreetDirPrefix, StreetName, StreetType, StreetDirSuffix)),
           dem = PartyCode == "DEM",
           rep = PartyCode == "REP",
           yob = as.integer(substring(DOB, 1, 4)),
           gender = ifelse(Gender == "", 0.5, Gender == "F")) %>% 
    rename(city = City,
           zip = Zip,
           voter_id = RegistrantID,
           surname = LastName,
           county = CountyCode) %>% 
    select(-Gender, -DOB, -PartyCode, -AddressNumber, -StreetDirPrefix, -StreetName, -StreetType, -StreetDirSuffix)
  
  ## set up and geocode voter file
  
  ca <- geocode(ca) %>% 
    ca <- ca %>% 
    select(-street, -city, -zip, -state)
  
  ca <- left_join(ca, history, by = c("county" = "CountyCode", "voter_id" = "RegistrantID")) %>% 
    mutate(v_primary = ifelse(is.na(v_primary), 0, v_primary),
           v_general = ifelse(is.na(v_general), 0, v_general)) %>% 
    rename(voted_primary = v_primary,
           voted_general = v_general)
  
  saveRDS(ca, "./temp/ca_geocoded.RDS")


ca <- readRDS("./temp/ca_geocoded.RDS")
## spatial join to find census tracts and congressional districts
tracts <- readOGR("./raw_data/tl_2018_06_tract", "tl_2018_06_tract")

pings  <- SpatialPoints(ca[c('longitude','latitude')], proj4string = tracts@proj4string)
ca$tract_full <- over(pings, tracts)$GEOID

cds <- readOGR("./raw_data/tl_2018_us_cd116", "tl_2018_us_cd116")
cds <- cds[substring(cds@data$GEOID, 1, 2) == "06", ]

pings  <- SpatialPoints(ca[c('longitude','latitude')], proj4string = cds@proj4string)
ca$cd <- over(pings, cds)$GEOID
rm(tracts, cds, pings)
## use wru to come up with race estimates
ca_census <- get_census_data(key = api_key, state = "CA", age = F, sex = F, census.geo = "tract")
saveRDS(ca_census, "./temp/wru_census_ca.RDS")

ca_census <- readRDS("./temp/wru_census_ca.RDS")

ca<- ca %>%
  mutate(state_code = substring(tract_full, 1, 2),
         county = substring(tract_full, 3, 5),
         tract = substring(tract_full, 6, 11),
         state = "CA")

ca <- predict_race(ca, census.geo = "tract", census.key = api_key, retry = 999, census.data = ca_census)

### pull down census data from tidycensus

ca_census_data <- get_basic_census_stats(geo = "tract", state = "CA", year = 2017)

ca <- left_join(ca, ca_census_data, by = c("tract_full" = "GEOID"))

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

ca <- left_join(ca, uc, by = c("cd" = "seat"))
ca$uncontested <- !is.na(ca$type)

## clean up, only keep complete cases for matching procedure
ca <- ca %>% 
  mutate_at(vars(voted_primary, voted_general, rep, dem), funs(ifelse(is.na(.), 0, .))) %>% 
  filter(!is.na(voter_id)) %>% 
  dplyr::select(-type)

ca <- ca[complete.cases(ca), ]

saveRDS(ca, "./temp/california_race_census.RDS")
