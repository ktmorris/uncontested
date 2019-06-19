
setwd("H:/Public/Democracy/Voting Rights & Elections/data/uncontested")


library(Matching)
library(scales)
library(kableExtra)
library(tidyverse)
library(data.table)

ga <- readRDS("./temp/georgia_race_census.RDS") %>% 
  ungroup() %>% 
  mutate(id = row_number())

X <- ga %>%
  dplyr::select(dem, rep, yob, gender, white, black, latino.x, voted_primary, median_income, some_college)


load("./temp/ga_genmatch_1.Rdata")
# 
# mout <- Matchby(Tr = ga$uncontested, X = X, by = c(X$dem, X$rep, X$voted_primary, X$gender), estimand = "ATT", Weight.matrix = genout, M = 100)
# 
# save(mout, file = "./temp/mout_ga_1.RData")

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

matches <- left_join(matches, ga, by = "id")

matches$voted_general <- matches$voted_general >= 1


reg_output <- glm(voted_general ~ treat, data = matches, weights = weight, family = "binomial")
saveRDS(reg_output, "./temp/match_reg_ga_1.rds")

#### look just in ga 8th (where republican ran)

match_df <- data.frame("id_control" = mout[["index.control"]],
                       "weight_control" = mout[["weights"]],
                       "id_treat" = mout[["index.treated"]],
                       "weight_treat" = mout[["weights"]])

match_df <- left_join(match_df,
                      ga %>% 
                        select(cd, id),
                      by = c("id_treat" = "id")) %>% 
  filter(cd == "1308")

matches1 <- match_df %>% 
  group_by(id = id_control) %>% 
  summarize(weight = sum(weight_control)) %>% 
  mutate(treat = F)

matches2 <- match_df %>% 
  group_by(id = id_treat) %>% 
  summarize(weight = sum(weight_treat)) %>% 
  mutate(treat = T)

matches <- bind_rows(matches1, matches2)

matches <- left_join(matches, ga, by = "id")

matches$voted_general <- matches$voted_general >= 1


reg_output_ga08 <- glm(voted_general ~ treat, data = matches, weights = weight, family = "binomial")
saveRDS(reg_output_ga08, "./temp/match_reg_ga_08.rds")

#### look just in ga 5th (where democrat ran)

match_df <- data.frame("id_control" = mout[["index.control"]],
                       "weight_control" = mout[["weights"]],
                       "id_treat" = mout[["index.treated"]],
                       "weight_treat" = mout[["weights"]])

match_df <- left_join(match_df,
                      ga %>% 
                        select(cd, id),
                      by = c("id_treat" = "id")) %>% 
  filter(cd == "1305")

matches1 <- match_df %>% 
  group_by(id = id_control) %>% 
  summarize(weight = sum(weight_control)) %>% 
  mutate(treat = F)

matches2 <- match_df %>% 
  group_by(id = id_treat) %>% 
  summarize(weight = sum(weight_treat)) %>% 
  mutate(treat = T)

matches <- bind_rows(matches1, matches2)

matches <- left_join(matches, ga, by = "id")

matches$voted_general <- matches$voted_general >= 1


reg_output_ga05 <- glm(voted_general ~ treat, data = matches, weights = weight, family = "binomial")
saveRDS(reg_output_ga05, "./temp/match_reg_ga_05.rds")
