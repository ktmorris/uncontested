# 
# ## read in wisconsin voter file
# wi <- dbGetQuery(db, "select Voter_Reg_Number, LastName, FirstName, County, Congressional, State_Assembly,
#                       Address1, Address2, November2018, August2018, ApplicationDate, Voter_Status from wi_roll_0319
#                      where Voter_Status == 'Active'") %>%
#   filter(as.Date(gsub(" [A-z ]*", "" , ApplicationDate), "%m/%d/%Y") <= "2018-10-17") %>% 
#   mutate(street = Address1,
#          city = gsub(" WI.*$", "", Address2),
#          zip = gsub(".*WI", "", Address2),
#          state = "WI") %>%
#   select(-Address2, -Address1)
# 
# 
# wi <- geocode(wi)
# 
# 
# genders <- gender::gender(unique(wi$FirstName))
# 
# wi <- left_join(wi, dplyr::select(genders, FirstName = name, gender = proportion_female), by = "FirstName")
# rm(genders)
# 
# tracts <- readOGR("./raw_data/tl_2018_55_tract", "tl_2018_55_tract")
# pings  <- SpatialPoints(wi[c('longitude','latitude')], proj4string = tracts@proj4string)
# wi$tract <- over(pings, tracts)$GEOID
# 
# saveRDS(wi, "./temp/wi_geocoded.rds")

wi <- readRDS("./temp/wi_geocoded.rds")


wi$voted_primary <- wi$August2018 %in% c("Absentee", "At Polls")
wi$voted_general <- wi$November2018 %in% c("Absentee", "At Polls")

wi <- dplyr::select(wi, -August2018, -November2018)

####
wi$cd <- as.integer(gsub("Congressional - District ", "", wi$Congressional))
wi$assembly <- as.integer(gsub("Assembly - District ", "", wi$State_Assembly))
wi <- dplyr::select(wi, -Congressional, -State_Assembly)
## use wru to come up with race estimates
# wi_census <- get_census_data(key = api_key, state = "WI", census.geo = "tract")
# saveRDS(wi_census, "./temp/wru_census_wi.RDS")

wi_census <- readRDS("./temp/wru_census_wi.RDS")

wi <- wi %>%
  rename(surname = LastName,
         tract_full = tract) %>%
  mutate(state_code = substring(tract_full, 1, 2),
         county = substring(tract_full, 3, 5),
         tract = substring(tract_full, 6, 11),
         state = "WI")

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
  mutate_at(vars(voted_primary, voted_general), funs(ifelse(is.na(.), 0, .))) %>% 
  mutate(gender = ifelse(is.na(gender), 0.5, gender)) %>% 
  filter(!is.na(Voter_Reg_Number))

wi <- wi[complete.cases(wi), ]

saveRDS(wi, "./temp/wisconsin_race_census.RDS")
