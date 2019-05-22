library(tidyverse)
library(Matching)
library(data.table)
library(snow)
library(parallel)
library(scales)
library(kableExtra)


setwd("H:/Public/Democracy/Voting Rights & Elections/data/uncontested")

  

load(file = "./temp/tx_genmatch_1.Rdata")

tx <- readRDS("./temp/texas_race_census.RDS")
  

X <- tx %>% 
	  dplyr::select(GENDER, voted_primary, dem, rep, yob, pred.whi, pred.bla, pred.his, median_income, some_college)



# mout <- Match(Tr = tx$uncontested, X = X, estimand = "ATT", Weight.matrix = genout, version = "fast", M = 100)
# summary(mout)
# 
# save(mout, file = "./temp/mout_tx_1.RData")

load("./temp/mout_tx_1.RData")

matches1 <- data.frame("id" = mout[["index.control"]],
                       "weight" = mout[["weights"]]) %>% 
  group_by(id) %>% 
  summarize(weight = sum(weight)) %>% 
  mutate(treat = F)

matches2 <- data.frame("id" = mout[["index.treated"]],
                       "weight" = mout[["weights"]]) %>% 
  group_by(id) %>% 
  summarize(weight = sum(weight)) %>% 
  mutate(treat = T)

matches <- bind_rows(matches1, matches2)

tx <- tx %>% 
  mutate(id = row_number())

matches <- left_join(matches, dplyr::select(tx, id, VUID, voted_general), by = "id")

matches$voted_general <- matches$voted_general >= 1


reg_output <- glm(voted_general ~ treat, data = matches, weights = weight*100, family = "binomial")
saveRDS(reg_output, "./temp/match_reg_tx_1.rds")

