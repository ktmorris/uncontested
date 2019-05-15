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
  

# genout <- GenMatch(Tr = tracts$uncontested, X = match_data,
#                    replace = T, pop.size = 1000, cluster = cl)
# 
# 
# saveRDS(genout, "./temp/tx_tract_genout.rds")

genout <- readRDS("./temp/tx_tract_genout.rds")

treat <- tracts$uncontested

mout <- Match(Tr = treat, X = match_data, estimand = "ATT", Weight.matrix = genout, version = "fast", M = 10)
summary(mout)

save(mout, file = "./temp/mout_tx_tract.RData")

load("./temp/mout_tx_tract.RData")


matches <- data.frame("treated" = mout[["index.treated"]],
                      "control" = mout[["index.control"]],
                      "weight" = mout[["weights"]])

tracts <- tracts %>% 
  mutate(id = row_number())

treat_row <- tracts %>% 
  filter(uncontested == 1) %>% 
  select(id, GEOID = tract_full)

untreat_row <- tracts %>% 
  filter(uncontested == 0) %>% 
  select(id, control_GEOID = tract_full)

matches <- left_join(matches, treat_row, by = c("treated" = "id"))
matches <- left_join(matches, untreat_row, by = c("control" = "id"))

matches <- dplyr::select(matches, -treated, -control)

matches_v <- bind_rows(dplyr::select(matches, GEOID, weight), dplyr::select(matches, GEOID = control_GEOID, weight))

match_w <- matches_v %>% 
  group_by(GEOID) %>% 
  summarize(weight = sum(weight))

reg <- inner_join(tracts, match_w, by = c("tract_full" = "GEOID"))

reg_output <- lm(general_to ~ uncontested, data = reg, weights = weight)

saveRDS(reg_output, "./temp/match_reg_texas_tract.rds")
saveRDS(reg, "./temp/texas_tract_reg_data.rds")

###########
load("./temp/mout_tx_tract.RData")
order <- fread("./raw_data/tx_tract_varorder.csv")

balance <- MatchBalance(uncontested ~ share_female + dem + rep + yob + white + black + latino + asian +
                        race_other + median_income + some_college + unem + share_non_citizen + primary_to, match.out = mout,
                        data = tracts)
TrMean <- c()
PreMean <- c()
PreQQmed <- c()
PreQQmean <- c()
PreQQmax <- c()
PostMean <- c()
PostQQmed <- c()
PostQQmean <- c()
PostQQmax <- c()

for(j in c(1:length(balance$BeforeMatching))){
  TrMean <- unlist(c(TrMean, balance$BeforeMatching[[j]][3][1]))
  PreMean <- unlist(c(PreMean, balance$BeforeMatching[[j]][4][1]))
  PreQQmed <- unlist(c(PreQQmed, balance$BeforeMatching[[j]]$qqsummary[2]))
  PreQQmean <- unlist(c(PreQQmean, balance$BeforeMatching[[j]]$qqsummary[1]))
  PreQQmax <- unlist(c(PreQQmax, balance$BeforeMatching[[j]]$qqsummary[3]))
  
  PostMean <- unlist(c(PostMean, balance$AfterMatching[[j]][4][1]))
  PostQQmed <- unlist(c(PostQQmed, balance$AfterMatching[[j]]$qqsummary[2]))
  PostQQmean <- unlist(c(PostQQmean, balance$AfterMatching[[j]]$qqsummary[1]))
  PostQQmax <- unlist(c(PostQQmax, balance$AfterMatching[[j]]$qqsummary[3]))
}

varnames <- c("share_female", "dem", "rep", "yob", "white", "black", "latino", "asian",
              "race_other", "median_income", "some_college", "unem", "share_non_citizen", "primary_to")


df <- data.frame("TrMean" = TrMean,
                 "TrMean2" = TrMean,
                 "PreMean" = PreMean,
                 "PreQQmed" = PreQQmed,
                 "PreQQmean" = PreQQmean,
                 "PreQQmax" = PreQQmax,
                 "PostMean" = PostMean,
                 "PostQQmed" = PostQQmed,
                 "PostQQmean" = PostQQmean,
                 "PostQQmax" = PostQQmax,
                 "names" = varnames) %>%
  mutate(change_mean = 1 - (abs(TrMean - PostMean) / abs(TrMean - PreMean)),
         change_eqqmed = 1 - abs(PostQQmed / PreQQmed),
         change_eqqmean = 1 - abs(PostQQmean / PreQQmean),
         change_eqqmax = 1 - abs(PostQQmax / PreQQmax)) %>%
  mutate_at(vars(TrMean, PreMean, TrMean2, PostMean), funs(comma(round(., 2), accuracy = .01))) %>%
  mutate_at(vars(change_mean, change_eqqmed, change_eqqmean, change_eqqmax), funs(round(. * 100, 2)))

df <- full_join(df, order, by = c("names" = "variable")) %>%
  arrange(order) %>%
  select(name, TrMean, PreMean, TrMean2, PostMean, change_mean, change_eqqmed, change_eqqmean, change_eqqmax) %>%
  filter(!is.na(TrMean))

colnames(df) <- c("", "Treated", "Control", "Treated", "Control", "Mean Diff", "eQQ Med", "eQQ Mean", "eQQ Max")

saveRDS(df, "./temp/match_table_tx_tract.rds")

kable(df, escape = FALSE, align = c('l', rep('c', 99))) %>%
  add_header_above(c(" " = 1, "Means: Unmatched Data" = 2, "Means: Matched Data" = 2, "Percent Improvement" = 4), align = "c") %>%
  kable_styling(font_size = 12, full_width = F) %>%
  save_kable(file = "./output/matches_tx_tract.html", self_contained = T)

