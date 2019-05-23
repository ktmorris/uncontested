library(Matching)
library(data.table)
library(snow)
library(parallel)
library(scales)
library(kableExtra)
library(tidyverse)

while(!(file.exists("./temp/wi_genmatch_1.Rdata"))){
  print("X")
  Sys.sleep(5*60)
}

Sys.sleep(5*60)

load(file = "./temp/wi_genmatch_1.Rdata")

wi <- readRDS("./temp/wisconsin_race_census.RDS")


X <- wi %>% 
  dplyr::select(gender, voted_primary, pred.whi, pred.bla, pred.his, median_income, some_college, median_age)


mout <- Matchby(Tr = ny$uncontested, X = X, by = c(X$voted_primary), estimand = "ATT", Weight.matrix = genout, M = 100)

save(mout, file = "./temp/mout_wi_1.RData")


load("./temp/mout_wi_1.RData")

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

wi <- wi %>% 
  mutate(id = row_number())

matches <- left_join(matches, dplyr::select(wi, id, nys_id, voted_general), by = "id")

matches$voted_general <- matches$voted_general >= 1

reg_output <- glm(voted_general ~ treat, data = matches, weights = weight*100, family = "binomial")
saveRDS(reg_output, "./temp/match_reg_wi_1.rds")
