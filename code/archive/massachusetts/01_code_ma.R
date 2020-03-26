

j <- list.files("D:/rolls/massachusetts/20190815", full.names = T, pattern = "*.csv", recursive = T)


full <- rbindlist(lapply(j, function(x){
  k <- fread(x)
  colnames(k) <- gsub("[.]", "_", make.unique(make.names(colnames(k))))
  k <- k %>% 
    select(LALVOTERID, Voters_StateVoterID, Voters_Active,
           Residence_Addresses_CensusTract, Residence_Addresses_CensusBlockGroup,
           Voters_FIPS, Voters_BirthDate, Voters_OfficialRegDate, Voters_CalculatedRegDate,
           X2011_NEW_Congressional_District, Primary_2018_09_04, General_2018_11_06,
           General_2016_11_08, Voters_Gender, Parties_Description, EstimatedIncomeAmount,
           Voters_LastName)
  
  
}))



census_obj <- get_census_data(key = api_key, states = "MA", census.geo = "tract")

full <- predict_race(full %>% 
                       rename(surname = Voters_LastName,
                              county = Voters_FIPS,
                              tract = Residence_Addresses_CensusTract) %>% 
                       mutate(state = "MA",
                              tract = str_pad(as.character(tract), width = 6, side = "left", pad = "0"),
                              county = str_pad(as.character(county), width = 3, side = "left", pad = "0")),
                     census.geo = "tract",
                     census.key = api_key, census.data = census_obj)


tract_data <- get_basic_census_stats(geo = "tract", state = "MA", year = 2017)

full <- left_join(full %>% 
                    mutate(GEOID = paste0("25", county, tract)),
                  tract_data)

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

full <- left_join(full %>% 
                    mutate(cd = paste0("25",
                                       str_pad(as.character(X2011_NEW_Congressional_District),
                                               side = "left", width = 2, pad = "0"))),
                  uc, by = c("cd" = "seat"))
full$uncontested <- !is.na(full$type)

## clean up, only keep complete cases for matching procedure
full <- full %>% 
  mutate(voted_primary = Primary_2018_09_04 == "Y",
         voted_general = General_2018_11_06 == "Y",
         rep = Parties_Description == "Republican",
         dem = Parties_Description == "Democratic") %>% 
  mutate_at(vars(voted_primary, voted_general, rep, dem), funs(ifelse(is.na(.), 0, .))) %>% 
  filter(!is.na(Voters_StateVoterID)) %>% 
  dplyr::select(-type, -Primary_2018_09_04, -General_2018_11_06,
                -General_2016_11_08, -Parties_Description)



full <- full %>% 
  mutate(gender = Voters_Gender == "F",
         yob = as.integer(substring(Voters_BirthDate, nchar(Voters_BirthDate) - 3)))

full <- full[complete.cases(full), ]

saveRDS(full, "./temp/mass_race_census.RDS")
