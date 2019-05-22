library(Matching)
library(data.table)
library(snow)
library(parallel)
library(scales)
library(kableExtra)
library(tidyverse)

setwd("H:/Public/Democracy/Voting Rights & Elections/data/uncontested")


load(file = "./temp/ny_genmatch_1.Rdata")

ny <- readRDS("./temp/new_york_race_census.RDS")


X <- ny %>% 
  dplyr::select(gender, voted_primary, dem, rep, yob, pred.whi, pred.bla, pred.his, median_income, some_college)



# mout <- Match(Tr = ny$uncontested, X = X, estimand = "ATT", Weight.matrix = genout, version = "fast", M = 100)
# summary(mout)
# 
# save(mout, file = paste0("./temp/mout_ny_", i, ".RData"))


load("./temp/mout_ny_1.RData")

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

ny <- ny %>% 
  mutate(id = row_number())

matches <- left_join(matches, dplyr::select(ny, id, nys_id, voted_general), by = "id")

matches$voted_general <- matches$voted_general >= 1

reg_output <- glm(voted_general ~ treat, data = matches, weights = weight*100, family = "binomial")
saveRDS(reg_output, "./temp/match_reg_ny_1.rds")
