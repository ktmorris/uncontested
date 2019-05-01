

if(db_access){
  ## get florida history

  
  p_votes <- dbGetQuery(db, "select * from fl_history_0319 where election_date == '08/28/2018' 
                             and participation in ('A', 'E', 'Y')")
  
  g_votes <- dbGetQuery(db, "select * from fl_history_0319 where election_date == '11/06/2018' 
                             and participation in ('A', 'E', 'Y')")
  
  ## read in florida voter file
  fl <- dbGetQuery(db, "select [Voter_ID], [Name_Last], [Gender], [Birth_Date], [County_Code], Race, [Party_Affiliation],
                        [Residence_Address_Line_1], [Residence_City], [Residence_Zipcode], [Registration_Date]
                           from fl_roll_0319
                       where [Voter_Status] == 'ACT'") %>% 
    mutate(state = "FL",
           Registration_Date = as.Date(as.character(Registration_Date), "%m/%d/%Y"),
           dem = Party_Affiliation == "DEM",
           rep = Party_Affiliation == "REP",
           yob = as.integer(substring(Birth_Date, nchar(Birth_Date) - 3)),
           gender = Gender == "F",
           white = Race == 5,
           black = Race == 3,
           latino = Race == 4) %>% 
    filter(Registration_Date <= "2018-10-09") %>% 
    rename(street = Residence_Address_Line_1,
           city = Residence_City,
           zip = Residence_Zipcode,
           voter_id = Voter_ID,
           surname = Name_Last,
           county = County_Code) %>% 
    select(-Gender, -Birth_Date, -Race, -Party_Affiliation, -Registration_Date)
  
  ## set up and geocode voter file
  
  fl <- geocode(fl) %>% 
    select(-street, -city, -zip, -state)
  
  fl$voted_primary <- fl$voter_id %in% p_votes$voter_id
  fl$voted_general <- fl$voter_id %in% g_votes$voter_id
  
  saveRDS(fl, "./temp/fl_geocoded.RDS")
}

fl <- readRDS("./temp/fl_geocoded.RDS")
## spatial join to find census tracts and congressional districts
tracts <- readOGR("H:/Public/Democracy/Voting Rights & Elections/data/uncontested/raw_data/tl_2018_12_tract", "tl_2018_12_tract")

pings  <- SpatialPoints(fl[c('longitude','latitude')], proj4string = tracts@proj4string)
fl$tract_full <- over(pings, tracts)$GEOID

cds <- readOGR("H:/Public/Democracy/Voting Rights & Elections/data/uncontested/raw_data/tl_2018_us_cd116", "tl_2018_us_cd116")
cds <- cds[substring(cds@data$GEOID, 1, 2) == "12", ]

pings  <- SpatialPoints(fl[c('longitude','latitude')], proj4string = cds@proj4string)
fl$cd <- over(pings, cds)$GEOID
rm(tracts, cds, pings)


### pull down census data from tidycensus

fl_census_data <- get_basic_census_stats(geo = "tract", state = "FL", year = 2017)

fl <- left_join(fl, fl_census_data, by = c("tract_full" = "GEOID"))

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

fl <- left_join(fl, uc, by = c("cd" = "seat"))
fl$uncontested <- !is.na(fl$type)

## clean up, only keep complete cases for matching procedure
fl <- fl %>% 
  mutate_at(vars(voted_primary, voted_general, rep, dem), funs(ifelse(is.na(.), 0, .))) %>% 
  filter(!is.na(voter_id)) %>% 
  dplyr::select(-type)

fl <- fl[complete.cases(fl), ]

saveRDS(fl, "./temp/florida_race_census.RDS")
