library(Matching)
library(scales)
library(kableExtra)
library(tidyverse)
library(data.table)

ny <- readRDS("./temp/new_york_race_census.RDS")

ids <- ny %>% 
  mutate(id = row_number()) %>% 
  select(id, nys_id)

# i <- 1
# 
# load("./temp/ny_genmatch_1.Rdata")
# 
# X <- ny %>%
#   dplyr::select(gender, dem, rep, yob, pred.whi, pred.bla, pred.his, median_income, some_college)
# 
# mout <- Matchby(Tr = ny$uncontested, X = X, by = c(X$dem, X$rep, X$gender), estimand = "ATT", Weight.matrix = genout, M = 25)
# 
# save(mout, file = paste0("./temp/mout_ny_", i, ".RData"))
# 
# load("./temp/mout_ny_1.RData")
# 
# 
# m1 <- data.frame("treat" = mout[["index.treated"]],
#                  "control" = mout[["index.control"]])
# 
# m1 <- left_join(m1, ids, by = c("treat" = "id"))
# 
# saveRDS(m1, "./temp/ny_reg_data.rds")

m1 <- readRDS("./temp/ny_reg_data.rds")

#######

m2 <- filter(m1, nys_id %in%
               filter(ny)$nys_id) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>%
  group_by(id) %>%
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(ny, matches, by = "nys_id")

reg1 <- summary(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight))

c1 <- exp(confint(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight)))

reg2 <- summary(glm(voted_general ~ uncontested +
                      gender + dem + rep + yob + pred.whi + pred.bla + pred.his +
                      median_income + some_college, data = reg,
                    family = "binomial", weights = weight))

c2 <- exp(confint(glm(voted_general ~ uncontested +
                        gender + dem + rep + yob + pred.whi + pred.bla + pred.his +
                        median_income + some_college, data = reg,
                      family = "binomial", weights = weight)))

save(reg1, reg2, c1, c2, file = paste0("./temp/ny_match_reg.rdata"))

####### white

m2 <- filter(m1, nys_id %in%
               filter(ny, pred.whi > 0.9)$nys_id) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>%
  group_by(id) %>%
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(ny, matches, by = "nys_id")

reg1 <- summary(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight))

c1 <- exp(confint(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight)))


reg2 <- summary(glm(voted_general ~ uncontested +
                      gender + dem + rep + yob + pred.whi + pred.bla + pred.his +
                      median_income + some_college, data = reg,
                    family = "binomial", weights = weight))

c2 <- exp(confint(glm(voted_general ~ uncontested +
                        gender + dem + rep + yob + pred.whi + pred.bla + pred.his +
                        median_income + some_college, data = reg,
                      family = "binomial", weights = weight)))

save(reg1, reg2, c1, c2, file = paste0("./temp/ny_white_match_reg.rdata"))

####### black

m2 <- filter(m1, nys_id %in%
               filter(ny, pred.bla > 0.9)$nys_id) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>%
  group_by(id) %>%
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(ny, matches, by = "nys_id")

reg1 <- summary(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight))

c1 <- exp(confint(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight)))


reg2 <- summary(glm(voted_general ~ uncontested +
                      gender + dem + rep + yob + pred.whi + pred.bla + pred.his +
                      median_income + some_college, data = reg,
                    family = "binomial", weights = weight))

c2 <- exp(confint(glm(voted_general ~ uncontested +
                        gender + dem + rep + yob + pred.whi + pred.bla + pred.his +
                        median_income + some_college, data = reg,
                      family = "binomial", weights = weight)))

save(reg1, reg2, c1, c2, file = paste0("./temp/ny_black_match_reg.rdata"))

####### latinx

m2 <- filter(m1, nys_id %in%
               filter(ny, pred.his > 0.9)$nys_id) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>%
  group_by(id) %>%
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(ny, matches, by = "nys_id")

reg1 <- summary(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight))

c1 <- exp(confint(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight)))


reg2 <- summary(glm(voted_general ~ uncontested +
                      gender + dem + rep + yob + pred.whi + pred.bla + pred.his +
                      median_income + some_college, data = reg,
                    family = "binomial", weights = weight))

c2 <- exp(confint(glm(voted_general ~ uncontested +
                        gender + dem + rep + yob + pred.whi + pred.bla + pred.his +
                        median_income + some_college, data = reg,
                      family = "binomial", weights = weight)))

save(reg1, reg2, c1, c2, file = paste0("./temp/ny_latinx_match_reg.rdata"))

####### dem

m2 <- filter(m1, nys_id %in%
               filter(ny, dem == T)$nys_id) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>%
  group_by(id) %>%
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(ny, matches, by = "nys_id")

reg1 <- summary(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight))

c1 <- exp(confint(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight)))


reg2 <- summary(glm(voted_general ~ uncontested +
                      gender + yob + pred.whi + pred.bla + pred.his +
                      median_income + some_college, data = reg,
                    family = "binomial", weights = weight))

c2 <- exp(confint(glm(voted_general ~ uncontested +
                        gender + yob + pred.whi + pred.bla + pred.his +
                        median_income + some_college, data = reg,
                      family = "binomial", weights = weight)))

save(reg1, reg2, c1, c2, file = paste0("./temp/ny_dem_match_reg.rdata"))

####### rep

m2 <- filter(m1, nys_id %in%
               filter(ny, rep == T)$nys_id) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>%
  group_by(id) %>%
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(ny, matches, by = "nys_id")

reg1 <- summary(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight))

c1 <- exp(confint(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight)))


reg2 <- summary(glm(voted_general ~ uncontested +
                      gender + yob + pred.whi + pred.bla + pred.his +
                      median_income + some_college, data = reg,
                    family = "binomial", weights = weight))

c2 <- exp(confint(glm(voted_general ~ uncontested +
                        gender + yob + pred.whi + pred.bla + pred.his +
                        median_income + some_college, data = reg,
                      family = "binomial", weights = weight)))

save(reg1, reg2, c1, c2, file = paste0("./temp/ny_rep_match_reg.rdata"))

####### lt40

m2 <- filter(m1, nys_id %in%
               filter(ny, (2018 - yob) < 40)$nys_id) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>%
  group_by(id) %>%
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(ny, matches, by = "nys_id")

reg1 <- summary(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight))

c1 <- exp(confint(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight)))


reg2 <- summary(glm(voted_general ~ uncontested +
                      gender + dem + rep + yob + pred.whi + pred.bla + pred.his +
                      median_income + some_college, data = reg,
                    family = "binomial", weights = weight))

c2 <- exp(confint(glm(voted_general ~ uncontested +
                        gender + dem + rep + yob + pred.whi + pred.bla + pred.his +
                        median_income + some_college, data = reg,
                      family = "binomial", weights = weight)))

save(reg1, reg2, c1, c2, file = paste0("./temp/ny_lt40_match_reg.rdata"))

####### 40-65

m2 <- filter(m1, nys_id %in%
               filter(ny, (2018 - yob) >= 40,
                      (2018 - yob) < 65)$nys_id) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>%
  group_by(id) %>%
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(ny, matches, by = "nys_id")

reg1 <- summary(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight))

c1 <- exp(confint(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight)))


reg2 <- summary(glm(voted_general ~ uncontested +
                      gender + dem + rep + yob + pred.whi + pred.bla + pred.his +
                      median_income + some_college, data = reg,
                    family = "binomial", weights = weight))

c2 <- exp(confint(glm(voted_general ~ uncontested +
                        gender + dem + rep + yob + pred.whi + pred.bla + pred.his +
                        median_income + some_college, data = reg,
                      family = "binomial", weights = weight)))

save(reg1, reg2, c1, c2, file = paste0("./temp/ny_40_65_match_reg.rdata"))

####### gt65

m2 <- filter(m1, nys_id %in%
               filter(ny, (2018 - yob) >= 65)$nys_id) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>%
  group_by(id) %>%
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(ny, matches, by = "nys_id")

reg1 <- summary(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight))

c1 <- exp(confint(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight)))


reg2 <- summary(glm(voted_general ~ uncontested +
                      gender + dem + rep + yob + pred.whi + pred.bla + pred.his +
                      median_income + some_college, data = reg,
                    family = "binomial", weights = weight))

c2 <- exp(confint(glm(voted_general ~ uncontested +
                        gender + dem + rep + yob + pred.whi + pred.bla + pred.his +
                        median_income + some_college, data = reg,
                      family = "binomial", weights = weight)))

save(reg1, reg2, c1, c2, file = paste0("./temp/ny_gt65_match_reg.rdata"))

####### men

m2 <- filter(m1, nys_id %in%
               filter(ny, gender == F)$nys_id) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>%
  group_by(id) %>%
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(ny, matches, by = "nys_id")

reg1 <- summary(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight))

c1 <- exp(confint(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight)))


reg2 <- summary(glm(voted_general ~ uncontested +
                      dem + rep + yob + pred.whi + pred.bla + pred.his +
                      median_income + some_college, data = reg,
                    family = "binomial", weights = weight))

c2 <- exp(confint(glm(voted_general ~ uncontested +
                        dem + rep + yob + pred.whi + pred.bla + pred.his +
                        median_income + some_college, data = reg,
                      family = "binomial", weights = weight)))

save(reg1, reg2, c1, c2, file = paste0("./temp/ny_men_match_reg.rdata"))

####### women

m2 <- filter(m1, nys_id %in%
               filter(ny, gender == T)$nys_id) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>%
  group_by(id) %>%
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(ny, matches, by = "nys_id")

reg1 <- summary(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight))

c1 <- exp(confint(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight)))


reg2 <- summary(glm(voted_general ~ uncontested +
                      dem + rep + yob + pred.whi + pred.bla + pred.his +
                      median_income + some_college, data = reg,
                    family = "binomial", weights = weight))

c2 <- exp(confint(glm(voted_general ~ uncontested +
                        dem + rep + yob + pred.whi + pred.bla + pred.his +
                        median_income + some_college, data = reg,
                      family = "binomial", weights = weight)))

save(reg1, reg2, c1, c2, file = paste0("./temp/ny_women_match_reg.rdata"))