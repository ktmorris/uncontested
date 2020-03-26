#### set up project

library(tidyverse)
library(data.table)

ca <- readRDS("./temp/california_race_census.RDS")

ids <- ca %>% 
  mutate(id = row_number()) %>% 
  select(id, voter_id)

# X <- ca %>%
#   dplyr::select(gender, dem, rep, yob, pred.whi, pred.bla, pred.his, median_income, some_college)
# 
# 
# load("./temp/ca_genmatch_1.Rdata")
# 
# mout <- Matchby(Tr = ca$uncontested, X = X, by = c(X$dem, X$rep, X$gender), estimand = "ATT", Weight.matrix = genout, M = 25)
# 
# save(mout, file = "./temp/mout_ca_1.RData")

load("./temp/mout_ca_1.RData")


m1 <- data.frame("treat" = mout[["index.treated"]],
                 "control" = mout[["index.control"]])

m1 <- left_join(m1, ids, by = c("treat" = "id"))

saveRDS(m1, "./temp/ca_reg_data.rds")

m1 <- readRDS("./temp/ca_reg_data.rds")

#######

m2 <- filter(m1, voter_id %in%
               filter(ca)$voter_id) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>%
  group_by(id) %>%
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(ca, matches, by = "voter_id")

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

save(reg1, reg2, c1, c2, file = paste0("./temp/ca_match_reg.rdata"))

####### white

m2 <- filter(m1, voter_id %in%
               filter(ca, pred.whi > 0.9)$voter_id) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>%
  group_by(id) %>%
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(ca, matches, by = "voter_id")

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

save(reg1, reg2, c1, c2, file = paste0("./temp/ca_white_match_reg.rdata"))

####### black

m2 <- filter(m1, voter_id %in%
               filter(ca, pred.bla > 0.9)$voter_id) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>%
  group_by(id) %>%
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(ca, matches, by = "voter_id")

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

save(reg1, reg2, c1, c2, file = paste0("./temp/ca_black_match_reg.rdata"))

####### latinx

m2 <- filter(m1, voter_id %in%
               filter(ca, pred.his > 0.9)$voter_id) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>%
  group_by(id) %>%
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(ca, matches, by = "voter_id")

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

save(reg1, reg2, c1, c2, file = paste0("./temp/ca_latinx_match_reg.rdata"))

####### dem

m2 <- filter(m1, voter_id %in%
               filter(ca, dem == T)$voter_id) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>%
  group_by(id) %>%
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(ca, matches, by = "voter_id")

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

save(reg1, reg2, c1, c2, file = paste0("./temp/ca_dem_match_reg.rdata"))

####### rep

m2 <- filter(m1, voter_id %in%
               filter(ca, rep == T)$voter_id) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>%
  group_by(id) %>%
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(ca, matches, by = "voter_id")

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

save(reg1, reg2, c1, c2, file = paste0("./temp/ca_rep_match_reg.rdata"))

####### lt40

m2 <- filter(m1, voter_id %in%
               filter(ca, (2018 - yob) < 40)$voter_id) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>%
  group_by(id) %>%
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(ca, matches, by = "voter_id")

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

save(reg1, reg2, c1, c2, file = paste0("./temp/ca_lt40_match_reg.rdata"))

####### 40-65

m2 <- filter(m1, voter_id %in%
               filter(ca, (2018 - yob) >= 40,
                      (2018 - yob) < 65)$voter_id) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>%
  group_by(id) %>%
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(ca, matches, by = "voter_id")

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

save(reg1, reg2, c1, c2, file = paste0("./temp/ca_40_65_match_reg.rdata"))

####### gt65

m2 <- filter(m1, voter_id %in%
               filter(ca, (2018 - yob) >= 65)$voter_id) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>%
  group_by(id) %>%
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(ca, matches, by = "voter_id")

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

save(reg1, reg2, c1, c2, file = paste0("./temp/ca_gt65_match_reg.rdata"))

####### men

m2 <- filter(m1, voter_id %in%
               filter(ca, gender == F)$voter_id) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>%
  group_by(id) %>%
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(ca, matches, by = "voter_id")

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

save(reg1, reg2, c1, c2, file = paste0("./temp/ca_men_match_reg.rdata"))

####### women

m2 <- filter(m1, voter_id %in%
               filter(ca, gender == T)$voter_id) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>%
  group_by(id) %>%
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(ca, matches, by = "voter_id")

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

save(reg1, reg2, c1, c2, file = paste0("./temp/ca_women_match_reg.rdata"))