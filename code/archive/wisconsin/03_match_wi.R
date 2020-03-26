
library(Matching)
library(data.table)
library(snow)
library(parallel)
library(scales)
library(kableExtra)
library(tidyverse)

load(file = "./temp/wi_genmatch_1.Rdata")

wi <- readRDS("./temp/wisconsin_race_census.RDS")

ids <- wi %>% 
  mutate(id = row_number()) %>% 
  select(id, LALVOTERID)

i <- 1
# X <- wi %>% 
#   dplyr::select(gender, dem, rep, median_age,
#                 pred.whi, pred.bla, pred.his, median_income, some_college)
# 
# 
# mout <- Matchby(Tr = wi$uncontested, X = X, by = c(X$gender, X$dem, X$rep), estimand = "ATT", Weight.matrix = genout, M = 100)
# 
# save(mout, file = "./temp/mout_wi_1.RData")
# 
# 
# load("./temp/mout_wi_1.RData")
# 
# m1 <- data.frame("treat" = mout[["index.treated"]],
#                  "control" = mout[["index.control"]])
# 
# m1 <- left_join(m1, ids, by = c("treat" = "id"))
# 
# saveRDS(m1, "./temp/wi_reg_data.rds")

#####

m1 <- readRDS("./temp/wi_reg_data.rds")

m2 <- filter(m1, LALVOTERID %in%
               filter(wi)$LALVOTERID) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>% 
  group_by(id) %>% 
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(wi, matches, by = "LALVOTERID")

reg1 <-summary(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight))

c1 <- exp(confint(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight)))

reg2 <-summary(glm(voted_general ~ uncontested + 
                      gender + dem + rep + median_age + pred.whi + pred.bla + pred.his + 
                      median_income + some_college, data = reg,
                    family = "binomial", weights = weight))

c2 <- exp(confint(glm(voted_general ~ uncontested + 
                        gender + dem + rep + median_age + pred.whi + pred.bla + pred.his + 
                        median_income + some_college, data = reg,
                      family = "binomial", weights = weight)))

save(reg1, reg2, c1, c2, file = paste0("./temp/wi_match_reg.rdata"))

###### white
m1 <- readRDS("./temp/wi_reg_data.rds")

m2 <- filter(m1, LALVOTERID %in%
               filter(wi, pred.whi > 0.9)$LALVOTERID) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>% 
  group_by(id) %>% 
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(wi, matches, by = "LALVOTERID")

reg1 <-summary(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight))

c1 <- exp(confint(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight)))


reg2 <-summary(glm(voted_general ~ uncontested + 
                      gender + dem + rep + median_age + pred.whi + pred.bla + pred.his + 
                      median_income + some_college, data = reg,
                    family = "binomial", weights = weight))

c2 <- exp(confint(glm(voted_general ~ uncontested + 
                        gender + dem + rep + median_age + pred.whi + pred.bla + pred.his + 
                        median_income + some_college, data = reg,
                      family = "binomial", weights = weight)))

save(reg1, reg2, c1, c2, file = paste0("./temp/wi_white_match_reg.rdata"))

###### black
m1 <- readRDS("./temp/wi_reg_data.rds")

m2 <- filter(m1, LALVOTERID %in%
               filter(wi, pred.bla > 0.9)$LALVOTERID) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>% 
  group_by(id) %>% 
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(wi, matches, by = "LALVOTERID")

reg1 <-summary(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight))

c1 <- exp(confint(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight)))


reg2 <-summary(glm(voted_general ~ uncontested + 
                      gender + dem + rep + median_age + pred.whi + pred.bla + pred.his + 
                      median_income + some_college, data = reg,
                    family = "binomial", weights = weight))

c2 <- exp(confint(glm(voted_general ~ uncontested + 
                        gender + dem + rep + median_age + pred.whi + pred.bla + pred.his + 
                        median_income + some_college, data = reg,
                      family = "binomial", weights = weight)))

save(reg1, reg2, c1, c2, file = paste0("./temp/wi_black_match_reg.rdata"))

###### latino
m1 <- readRDS("./temp/wi_reg_data.rds")

m2 <- filter(m1, LALVOTERID %in%
               filter(wi, pred.his > 0.9)$LALVOTERID) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>% 
  group_by(id) %>% 
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(wi, matches, by = "LALVOTERID")

reg1 <-summary(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight))

c1 <- exp(confint(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight)))


reg2 <-summary(glm(voted_general ~ uncontested + 
                      gender + dem + rep + median_age + pred.whi + pred.bla + pred.his + 
                      median_income + some_college, data = reg,
                    family = "binomial", weights = weight))

c2 <- exp(confint(glm(voted_general ~ uncontested + 
                        gender + dem + rep + median_age + pred.whi + pred.bla + pred.his + 
                        median_income + some_college, data = reg,
                      family = "binomial", weights = weight)))

save(reg1, reg2, c1, c2, file = paste0("./temp/wi_latinx_match_reg.rdata"))

###### dem
m1 <- readRDS("./temp/wi_reg_data.rds")

m2 <- filter(m1, LALVOTERID %in%
               filter(wi,
                      (dem == T & !(assembly %in% c(1, 37, 59, 82, 97, 98, 99))) |
                        (rep == T & assembly %in% c(1, 37, 59, 82, 97, 98, 99)))$LALVOTERID) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>% 
  group_by(id) %>% 
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(wi, matches, by = "LALVOTERID")

reg1 <-summary(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight))

c1 <- exp(confint(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight)))


reg2 <-summary(glm(voted_general ~ uncontested + 
                      gender + median_age + pred.whi + pred.bla + pred.his + 
                      median_income + some_college, data = reg,
                    family = "binomial", weights = weight))

c2 <- exp(confint(glm(voted_general ~ uncontested + 
                        gender + median_age + pred.whi + pred.bla + pred.his + 
                        median_income + some_college, data = reg,
                      family = "binomial", weights = weight)))

save(reg1, reg2, c1, c2, file = paste0("./temp/wi_dem_match_reg.rdata"))

###### rep
m1 <- readRDS("./temp/wi_reg_data.rds")

m2 <- filter(m1, LALVOTERID %in%
               filter(wi,
                      (rep == T & !(assembly %in% c(1, 37, 59, 82, 97, 98, 99))) |
                        (dem == T & assembly %in% c(1, 37, 59, 82, 97, 98, 99)))$LALVOTERID) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>% 
  group_by(id) %>% 
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(wi, matches, by = "LALVOTERID")

reg1 <-summary(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight))

c1 <- exp(confint(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight)))


reg2 <-summary(glm(voted_general ~ uncontested + 
                      gender + median_age + pred.whi + pred.bla + pred.his + 
                      median_income + some_college, data = reg,
                    family = "binomial", weights = weight))

c2 <- exp(confint(glm(voted_general ~ uncontested + 
                        gender + median_age + pred.whi + pred.bla + pred.his + 
                        median_income + some_college, data = reg,
                      family = "binomial", weights = weight)))

save(reg1, reg2, c1, c2, file = paste0("./temp/wi_rep_match_reg.rdata"))

###### men
m1 <- readRDS("./temp/wi_reg_data.rds")

m2 <- filter(m1, LALVOTERID %in%
               filter(wi, gender == F)$LALVOTERID) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>% 
  group_by(id) %>% 
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(wi, matches, by = "LALVOTERID")

reg1 <-summary(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight))

c1 <- exp(confint(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight)))

reg2 <-summary(glm(voted_general ~ uncontested + 
                      dem + rep + median_age + pred.whi + pred.bla + pred.his + 
                      median_income + some_college, data = reg,
                    family = "binomial", weights = weight))

c2 <- exp(confint(glm(voted_general ~ uncontested + 
                        dem + rep + median_age + pred.whi + pred.bla + pred.his + 
                        median_income + some_college, data = reg,
                      family = "binomial", weights = weight)))

save(reg1, reg2, c1, c2, file = paste0("./temp/wi_men_match_reg.rdata"))

###### men
m1 <- readRDS("./temp/wi_reg_data.rds")

m2 <- filter(m1, LALVOTERID %in%
               filter(wi, gender == T)$LALVOTERID) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>% 
  group_by(id) %>% 
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(wi, matches, by = "LALVOTERID")

reg1 <-summary(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight))

c1 <- exp(confint(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight)))

reg2 <-summary(glm(voted_general ~ uncontested + 
                      dem + rep + median_age + pred.whi + pred.bla + pred.his + 
                      median_income + some_college, data = reg,
                    family = "binomial", weights = weight))

c2 <- exp(confint(glm(voted_general ~ uncontested + 
                        dem + rep + median_age + pred.whi + pred.bla + pred.his + 
                        median_income + some_college, data = reg,
                      family = "binomial", weights = weight)))

save(reg1, reg2, c1, c2, file = paste0("./temp/wi_women_match_reg.rdata"))