
setwd("H:/Public/Democracy/Voting Rights & Elections/data/uncontested")


library(Matching)
library(scales)
library(kableExtra)
library(tidyverse)
library(data.table)

fl <- readRDS("./temp/florida_race_census.RDS")

X <- fl %>%
  dplyr::select(dem, rep, yob, gender, white, black, latino.x, voted_primary, median_income, some_college)


load("./temp/fl_genmatch_1.Rdata")
# 
# mout <- Matchby(Tr = fl$uncontested, X = X, by = c(X$dem, X$rep, X$voted_primary, X$gender), estimand = "ATT", Weight.matrix = genout, M = 100)
# 
# save(mout, file = "./temp/mout_fl_1.RData")

load("./temp/mout_fl_1.RData")

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

fl <- fl %>% 
  mutate(id = row_number())

matches <- left_join(matches, dplyr::select(fl, id, voter_id, voted_general, uncontested), by = "id")

matches$voted_general <- matches$voted_general >= 1


reg_output <- glm(voted_general ~ uncontested, data = matches, weights = weight, family = "binomial")
saveRDS(reg_output, "./temp/match_reg_fl_1.rds")
