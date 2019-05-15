source("./code/misc/AutoCluster4.R")
cl <- NCPUS(detectCores() - 1)

### collapse to census tract level

tx <- readRDS("./temp/texas_race_census.RDS")


tracts <- tx %>% 
  group_by(tract_full) %>% 
  summarize(share_female = mean(GENDER, na.rm = T),
            voted_primary = sum(voted_primary),
            voted_general = sum(voted_general),
            dem = mean(dem),
            rep = mean(rep),
            yob = mean(yob),
            white = mean(pred.whi),
            black = mean(pred.bla),
            latino = mean(pred.his),
            asian = mean(pred.asi),
            race_other = mean(pred.oth),
            uncontested = mean(uncontested)) %>% 
  filter(uncontested %in% c(0, 1)) ## drop census tracts that cross lines


census_data <- get_basic_census_stats(geo = "tract", state = "TX", year = 2017) %>% 
  dplyr::select(-latino, -latino_black, -nh_white, -nh_black)

cvap <- fread("./raw_data/CVAP_2013-2017_ACS_csv_files/Tract.csv") %>% 
  filter(lntitle == "Total") %>% 
  mutate(GEOID = sub('.*\\US', '', geoid)) %>% 
  select(GEOID, cvap = CVAP_EST)

tracts <- inner_join(tracts, census_data, by = c("tract_full" = "GEOID")) 

tracts <- left_join(tracts, cvap, by = c("tract_full" = "GEOID")) %>% 
  mutate(primary_to = voted_primary / cvap,
         general_to = voted_general / cvap) %>% 
  dplyr::select(tract_full, uncontested, share_female, dem, rep, yob, white, black, latino, asian,
                race_other, median_income, some_college, unem, share_non_citizen, primary_to, general_to)

saveRDS(tracts, "./temp/tx_tracts_pre_genmatch.rds")

tracts <- readRDS("./temp/tx_tracts_pre_genmatch.rds")

match_data <- tracts %>% 
  dplyr::select(-uncontested, -tract_full, -general_to)
  

genout <- GenMatch(Tr = tracts$uncontested, X = match_data,
                   replace = T, pop.size = 1000, cluster = cl)