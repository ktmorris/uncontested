
setwd("H:/Public/Democracy/Voting Rights & Elections/data/uncontested")


library(Matching)
library(scales)
library(kableExtra)
library(tidyverse)
library(data.table)

ga <- readRDS("./temp/georgia_race_census.RDS")

X <- ga %>%
  dplyr::select(dem, rep, yob, gender, white, black, latino.x, voted_primary, median_income, some_college)


load("./temp/ga_genmatch_1.Rdata")

mout <- Matchby(Tr = ga$uncontested, X = X, by = c(X$dem, X$rep, X$voted_primary, X$gender), estimand = "ATT", Weight.matrix = genout, M = 100)

save(mout, file = "./temp/mout_ga_1.RData")

load("./temp/mout_ga_1.RData")

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

ga <- ga %>% 
  mutate(id = row_number())

matches <- left_join(matches, dplyr::select(ga, id, voter_id, voted_general, uncontested, county), by = "id")

matches$voted_general <- matches$voted_general >= 1


reg_output <- glm(voted_general ~ treat, data = matches, weights = weight, family = "binomial")
saveRDS(reg_output, "./temp/match_reg_ga_1.rds")
