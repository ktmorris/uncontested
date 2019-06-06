

if(db_access){
  ## get ga history
  hist = read_fwf(
    file = "D:/rolls/georgia/history/2018_full/2018.TXT",
    fwf_widths(c(3, 8, 8, 3, 2, 1, 1, 1)),
    col_types = "iidccccc"
  )
  
  colnames(hist) <- c("county", "voter_id", "election_date", "election_type", "party", "absentee", "provisional", "supplemental")
  
  votes_2018 <- select(hist, county, voter_id, election_date) %>% 
    mutate(voted_primary = election_date == 20180522,
           voted_general = election_date == 20181106) %>% 
    group_by(county, voter_id) %>% 
    summarize(voted_primary = max(voted_primary, na.rm = T),
              voted_general = max(voted_general, na.rm = T))
  
  ### do party history
  
  p18 <- hist[hist$election_date == 20180522 & !is.na(hist$party), c("county", "voter_id", "party")]
  p18$year <- 2018
  
  p16 <- read_fwf(
    file = "D:/rolls/georgia/history/2016_full/2016.TXT",
    fwf_widths(c(3, 8, 8, 3, 2, 1, 1, 1),
               col_names = c("county", "voter_id", "election_date", "election_type", "party", "absentee", "provisional", "supplemental")),
    col_types = "iidccccc"
  )
  
  p16 <- p16[p16$election_date %in% c(20160301, 20160524) & !is.na(p16$party), c("county", "voter_id", "election_date", "party")] %>% 
    group_by(county, voter_id) %>% 
    filter(election_date == max(election_date)) %>% 
    select(-election_date)
  p16$year <- 2016
  
  p14 <- read_fwf(
    file = "D:/rolls/georgia/history/2014_primary/2014_primary.TXT",
    fwf_widths(c(3, 8, 8, 3, 2, 1, 1, 1),
               col_names = c("county", "voter_id", "election_date", "election_type", "party", "absentee", "provisional", "supplemental")),
    col_types = "iidccccc"
  )
  
  p14 <- p14[!is.na(p14$party), c("county", "voter_id", "party")]
  p14$year <- 2014
  
  p12 <- read_fwf(
    file = "D:/rolls/georgia/history/2012_full/Voter History 2012.TXT",
    fwf_widths(c(3, 8, 8, 3, 1, 1),
               col_names = c("county", "voter_id", "election_date", "election_type", "party", "absentee")),
    col_types = "iidccc"
  )
  
  p12 <- p12[p12$election_date %in% c(3062012, 7312012) & !is.na(p12$party), c("county", "voter_id", "election_date", "party")] %>% 
    group_by(county, voter_id) %>% 
    filter(election_date == max(election_date)) %>% 
    select(-election_date)
  p12$year <- 2012
  
  p10 <- read_fwf(
    file = "D:/rolls/georgia/history/2010_full/Voter History 2010.TXT",
    fwf_widths(c(3, 8, 8, 3, 1, 1),
               col_names = c("county", "voter_id", "election_date", "election_type", "party", "absentee")),
    col_types = "iidccc"
  )
  
  p10 <- p10[p10$election_date == 7202010 & !is.na(p10$party), c("county", "voter_id", "party")]
  p10$year <- 2010
  
  p08 <- read_fwf(
    file = "D:/rolls/georgia/history/2008_full/Voter History 2008.TXT",
    fwf_widths(c(3, 8, 8, 3, 1, 1),
               col_names = c("county", "voter_id", "election_date", "election_type", "party", "absentee")),
    col_types = "iidccc"
  )
  
  p08 <- p08[p08$election_date %in% c(2052008, 7152008) & !is.na(p08$party), c("county", "voter_id", "election_date", "party")] %>% 
    group_by(county, voter_id) %>% 
    filter(election_date == max(election_date)) %>% 
    select(-election_date)
  p08$year <- 2008
  
  party <- rbindlist(list(p08, p10, p12, p14, p16, p18)) %>% 
    group_by(county, voter_id) %>% 
    filter(year == max(year)) %>% 
    mutate(dem = party == "D",
           rep = party == "R") %>% 
    select(-year, -party)
  rm(p08, p10, p12, p14, p16, p18)
  
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

  ga <- left_join(ga, votes_2018, by = c("county", "voter_id")) %>% 
    mutate(voted_general = ifelse(is.na(voted_general), 0, voted_general),
           voted_primary = ifelse(is.na(voted_primary), 0, voted_primary))
  
  ga <- left_join(ga, party, by = c("county", "voter_id")) %>% 
    mutate(dem = ifelse(is.na(dem), F, dem),
           rep = ifelse(is.na(rep), F, rep))
  ## set up and geocode voter file
  
  ga <- geocode(ga) %>% 
    select(-street, -city, -zip, -state)
  
  saveRDS(ga, "./temp/ga_geocoded.RDS")
  rm(hist, votes_2018, party)
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

