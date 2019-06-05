# 
# ## read in wisconsin voter file
# wi <- dbGetQuery(db, "select Voter_Reg_Number, LastName, FirstName, County, Congressional, State_Assembly, Jurisdiction, Ward,
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

#### read in 2016 election results for party controls

r16 <- fread("./raw_data/wi_results/cleaned_file_2016_results.csv")
colnames(r16) <- gsub("[.]", "_", make.unique(make.names(colnames(r16))))
r16$share_r <- r16$Donald_J__Trump___Michael_R__Pence / r16$Total_Votes
r16 <- select(r16, County_Name, Municipality_Name, Reporting_Unit, share_r)
# start with non-consequtive precincts - they have commas

nonc <- r16 %>% 
  filter(grepl(",", Reporting_Unit))
nonc <- cSplit(nonc, "Reporting_Unit", sep = ",", type.convert = F, drop = F, direction = "long")

nonc$ward <- gsub(".*Wards ", "", nonc$Reporting_Unit)
nonc$ward <- gsub(".*Ward ", "", nonc$ward)
nonc <- nonc[!is.na(nonc$ward), ]

nonc_cool <- nonc[!grepl("-", nonc$ward)] %>% 
  select(-Reporting_Unit, -Reporting_Unit)

nonc_more <- nonc %>% 
  filter(grepl("-", ward)) %>% 
  mutate(ward = gsub("A", "", ward))
nonc_more <- cSplit(nonc_more, "ward", sep = "-", type.convert = F)
for(i in 1:nrow(nonc_more)){
  nonc_more$ward[i] <- paste(c(nonc_more$ward_1[i]:nonc_more$ward_2[i]), collapse = ",")
}
nonc_more <- select(nonc_more, -ward_1, -ward_2, -Reporting_Unit)
nonc_more <- cSplit(nonc_more, "ward", sep = ",", direction = "long", type.convert = F)

nonc <- bind_rows(nonc_cool, nonc_more)
# now consequtive precincts

others <- r16 %>% 
  mutate(Reporting_Unit = gsub("Wards", "Ward", Reporting_Unit)) %>% 
  filter(grepl("-", gsub(".*Ward ", "", Reporting_Unit)),
         !grepl(",", Reporting_Unit))

others$ward <- gsub(".*Wards ", "", others$Reporting_Unit)
others$ward <- gsub(".*Ward ", "", others$ward)
others$ward <- gsub("B", "", others$ward)
others$ward <- gsub("A", "", others$ward)
others <- cSplit(others, "ward", sep = "-", type.convert = F)

for(i in 1:nrow(others)){
  others$ward[i] <- paste(c(others$ward_1[i]:others$ward_2[i]), collapse = ",")
}
others <- cSplit(others, "ward", sep = ",", direction = "long", type.convert = F)
others$ward <- ifelse(others$Reporting_Unit == "TOWN OF LA GRANGE Ward 1B-3B", paste0(others$ward, "00"), others$ward)
others <- select(others, -Reporting_Unit, - ward_1, -ward_2)

# fine ones
fine <- r16 %>% 
  mutate(Reporting_Unit = gsub("Wards", "Ward", Reporting_Unit)) %>% 
  filter(!grepl("-", gsub(".*Ward ", "", Reporting_Unit)),
         !grepl(",", Reporting_Unit)) %>% 
  mutate(ward = gsub(".*Ward ", "", Reporting_Unit)) %>% 
  select(-Reporting_Unit)
##
r16 <- bind_rows(fine, others, nonc_more, nonc_cool) %>% 
  filter(!is.nan(share_r)) %>% 
  mutate(Municipality_Name = gsub("TOWN OF |CITY OF |VILLAGE OF ", "", Municipality_Name))
####

wi <- wi %>% 
  mutate(Municipality_Name = gsub("TOWN OF |CITY OF |VILLAGE OF ", "", trimws(gsub("-.*", "", Jurisdiction))),
         County_Name = toupper(gsub(" County", "", County)),
         ward = gsub(".*Ward ", "", Ward),
         ward = ifelse(Municipality_Name == "TOWN OF LA GRANGE", gsub("A|B", "", ward), ward))

wi2 <- left_join(wi, r16, by = c("County_Name", "Municipality_Name", "ward"))

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
