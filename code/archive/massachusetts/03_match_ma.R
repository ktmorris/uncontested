library(miceadds)
library(Matching)
library(scales)
library(kableExtra)
library(tidyverse)
library(data.table)

ma <- readRDS("./temp/mass_race_census.RDS")

ids <- ma %>% 
        mutate(id = row_number()) %>% 
        select(id, Voters_StateVoterID)

# load("./temp/ma_genmatch_1.Rdata")
# 
# X <- ma %>%
#   dplyr::select(gender, dem, rep, yob, pred.whi, pred.bla, pred.his, median_income, some_college)
# 
# mout <- Matchby(Tr = ma$uncontested, X = X, by = c(X$dem, X$rep, X$gender), estimand = "ATT", Weight.matrix = genout, M = 100)
# 
# save(mout, file = "./temp/mout_ma_1.RData")
# 
# load("./temp/mout_ma_1.RData")
# 
# m1 <- data.frame("treat" = mout[["index.treated"]],
#                  "control" = mout[["index.control"]])
# 
# m1 <- left_join(m1, ids, by = c("treat" = "id"))
# 
# saveRDS(m1, "./temp/ma_reg_data.rds")

#####

m1 <- readRDS("./temp/ma_reg_data.rds")

m2 <- filter(m1, Voters_StateVoterID %in%
               filter(ma)$Voters_StateVoterID) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>% 
        group_by(id) %>% 
        summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(ma, matches, by = "Voters_StateVoterID")

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

save(reg1, reg2, c1, c2, file = paste0("./temp/ma_match_reg.rdata"))

##### white

m2 <- filter(m1, Voters_StateVoterID %in%
               filter(ma, pred.whi > 0.9)$Voters_StateVoterID) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>% 
  group_by(id) %>% 
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(ma, matches, by = "Voters_StateVoterID")

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


save(reg1, reg2, c1, c2, file = paste0("./temp/ma_white_match_reg.rdata"))

##### black

m2 <- filter(m1, Voters_StateVoterID %in%
               filter(ma, pred.bla > 0.9)$Voters_StateVoterID) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>% 
  group_by(id) %>% 
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(ma, matches, by = "Voters_StateVoterID")

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


save(reg1, reg2, c1, c2, file = paste0("./temp/ma_black_match_reg.rdata"))

##### latinx

m2 <- filter(m1, Voters_StateVoterID %in%
               filter(ma, pred.his > 0.9)$Voters_StateVoterID) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>% 
  group_by(id) %>% 
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(ma, matches, by = "Voters_StateVoterID")

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


save(reg1, reg2, c1, c2, file = paste0("./temp/ma_latinx_match_reg.rdata"))

##### dem

m2 <- filter(m1, Voters_StateVoterID %in%
               filter(ma, dem == T)$Voters_StateVoterID) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>% 
  group_by(id) %>% 
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(ma, matches, by = "Voters_StateVoterID")

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


save(reg1, reg2, c1, c2, file = paste0("./temp/ma_dem_match_reg.rdata"))

##### rep

m2 <- filter(m1, Voters_StateVoterID %in%
               filter(ma, rep == T)$Voters_StateVoterID) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>% 
  group_by(id) %>% 
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(ma, matches, by = "Voters_StateVoterID")

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


save(reg1, reg2, c1, c2, file = paste0("./temp/ma_rep_match_reg.rdata"))

##### lt40

m2 <- filter(m1, Voters_StateVoterID %in%
               filter(ma, (2018 - yob) < 40)$Voters_StateVoterID) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>% 
  group_by(id) %>% 
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(ma, matches, by = "Voters_StateVoterID")

reg1 <- summary(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight))

c1 <- exp(confint(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight)))

reg2 <- summary(glm(voted_general ~ uncontested + 
                      gender + yob + pred.whi + pred.bla + pred.his + 
                      median_income + some_college, data = reg,
                    family = "binomial", weights = weight))

c2 <- exp(confint(glm(voted_general ~ uncontested + 
                        gender + dem + rep + yob + pred.whi + pred.bla + pred.his + 
                        median_income + some_college, data = reg,
                      family = "binomial", weights = weight)))


save(reg1, reg2, c1, c2, file = paste0("./temp/ma_lt40_match_reg.rdata"))

##### 40-65

m2 <- filter(m1, Voters_StateVoterID %in%
               filter(ma, (2018 - yob) >= 40,
                      (2018 - yob) < 65)$Voters_StateVoterID) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>% 
  group_by(id) %>% 
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(ma, matches, by = "Voters_StateVoterID")

reg1 <- summary(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight))

c1 <- exp(confint(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight)))

reg2 <- summary(glm(voted_general ~ uncontested + 
                      gender + yob + pred.whi + pred.bla + pred.his + 
                      median_income + some_college, data = reg,
                    family = "binomial", weights = weight))

c2 <- exp(confint(glm(voted_general ~ uncontested + 
                        gender + dem + rep + yob + pred.whi + pred.bla + pred.his + 
                        median_income + some_college, data = reg,
                      family = "binomial", weights = weight)))


save(reg1, reg2, c1, c2, file = paste0("./temp/ma_40_65_match_reg.rdata"))

##### o65

m2 <- filter(m1, Voters_StateVoterID %in%
               filter(ma, (2018 - yob) >= 65)$Voters_StateVoterID) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>% 
  group_by(id) %>% 
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(ma, matches, by = "Voters_StateVoterID")

reg1 <- summary(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight))

c1 <- exp(confint(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight)))

reg2 <- summary(glm(voted_general ~ uncontested + 
                      gender + yob + pred.whi + pred.bla + pred.his + 
                      median_income + some_college, data = reg,
                    family = "binomial", weights = weight))

c2 <- exp(confint(glm(voted_general ~ uncontested + 
                        gender + dem + rep + yob + pred.whi + pred.bla + pred.his + 
                        median_income + some_college, data = reg,
                      family = "binomial", weights = weight)))


save(reg1, reg2, c1, c2, file = paste0("./temp/ma_gt65_match_reg.rdata"))

##### men

m2 <- filter(m1, Voters_StateVoterID %in%
               filter(ma, gender == F)$Voters_StateVoterID) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>% 
  group_by(id) %>% 
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(ma, matches, by = "Voters_StateVoterID")

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


save(reg1, reg2, c1, c2, file = paste0("./temp/ma_men_match_reg.rdata"))


##### women

m2 <- filter(m1, Voters_StateVoterID %in%
               filter(ma, gender == T)$Voters_StateVoterID) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>% 
  group_by(id) %>% 
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(ma, matches, by = "Voters_StateVoterID")

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


save(reg1, reg2, c1, c2, file = paste0("./temp/ma_women_match_reg.rdata"))



