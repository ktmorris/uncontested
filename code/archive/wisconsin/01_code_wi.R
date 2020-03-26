wi <- read.csv.sql("D:/national/04-11-2019-delivery/voter-files-by-state/VM2--WI--2019-02-01/VM2--WI--2019-02-01-DEMOGRAPHIC.tab",
                   sep = "\t",
                   sql = "select LALVOTERID, Voters_Active, 
                   Voters_StateVoterID, Voters_CountyVoterID, 
                   Voters_LastName, 
                   Residence_Addresses_CensusTract, 
                   Residence_Addresses_CensusBlockGroup, 
                   Residence_Addresses_CensusBlock, 
                   Residence_Addresses_Latitude, 
                   Residence_Addresses_Longitude, 
                   Voters_Gender, Voters_Age, Voters_BirthDate, 
                   DateConfidence_Description, Parties_Description, 
                   EthnicGroups_EthnicGroup1Desc, Voters_CalculatedRegDate, 
                   Voters_OfficialRegDate, US_Congressional_District, 
                   State_Senate_District, State_House_District, 
                   County, Voters_FIPS
                   from file where Residence_Addresses_CensusTract != '0'")

wi_history <- read.csv.sql("D:/national/04-11-2019-delivery/voter-files-by-state/VM2--WI--2019-02-01/VM2--WI--2019-02-01-VOTEHISTORY.tab",
                           sep = "\t",
                           sql = "select LALVOTERID from file
                                  where BallotType_General_2018_11_06 in ('Absentee', 'Poll Vote')")

wi$voted_general <- wi$LALVOTERID %in% wi_history$LALVOTERID

rm(wi_history)

####
wi <- rename(wi, cd = US_Congressional_District, assembly = State_House_District)
## use wru to come up with race estimates
# wi_census <- get_census_data(key = api_key, state = "WI", census.geo = "tract")
# saveRDS(wi_census, "./temp/wru_census_wi.RDS")

wi_census <- readRDS("./temp/wru_census_wi.RDS")

wi <- wi %>%
  mutate(tract_full = paste0("55", str_pad(as.character(Voters_FIPS), side = "left",
                                           width = 3, pad = "0"),
                             str_pad(as.character(Residence_Addresses_CensusTract), side = "left",
                                     width = 6, pad = "0")),
         state_code = substring(tract_full, 1, 2),
         county = substring(tract_full, 3, 5),
         tract = substring(tract_full, 6, 11),
         state = "WI") %>% 
  rename(surname = Voters_LastName) %>% 
  mutate(surname = str_replace_all(surname, "[^[:alnum:]]", ""))

wi <- predict_race(wi, census.geo = "tract", census.key = api_key, retry = 999, census.data = wi_census)


### pull down census data from tidycensus

wi_census_data <- get_basic_census_stats(geo = "tract", state = "WI", year = 2017)

wi <- left_join(wi, wi_census_data, by = c("tract_full" = "GEOID"))

### pull in uncontested races
uc <- fread("./raw_data/wi_assembly_uncontested.csv") %>% 
  select(assembly = district, uncontested)


wi <- left_join(wi, uc, by = "assembly")

## clean up, only keep complete cases for matching procedure
wi <- wi %>% 
  mutate_at(vars(voted_general), funs(ifelse(is.na(.), 0, .))) %>% 
  mutate(gender = Voters_Gender == "F",
         dem = Parties_Description == "Democratic",
         rep = Parties_Description == "Republican",
         yob = as.integer(substring(Voters_BirthDate, nchar(Voters_BirthDate) - 3))) %>% 
  select(uncontested, LALVOTERID, voted_general, gender, dem, rep, median_age,
         pred.whi, pred.bla, pred.his, median_income, some_college, assembly) %>% 
  filter(!is.na(LALVOTERID))

wi <- wi[complete.cases(wi), ]

saveRDS(wi, "./temp/wisconsin_race_census.RDS")
