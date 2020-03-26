load(file = "./temp/tx_genmatch_1.Rdata")

tx <- readRDS("./temp/texas_race_census.RDS")

ids <- tx %>%
  mutate(id = row_number()) %>%
  dplyr::select(id, VUID)

# X <- tx %>%
#   dplyr::select(GENDER, dem, rep, yob, pred.whi, pred.bla, pred.his, median_income, some_college)
#
#
#
# mout <- Matchby(Tr = tx$uncontested, X = X, by = c(X$GENDER, X$dem, X$rep), estimand = "ATT", Weight.matrix = genout, M = 25)
# summary(mout)
#
# save(mout, file = "./temp/mout_tx_1.RData")
# 
# load("./temp/mout_tx_1.RData")
# 
# m1 <- data.frame("treat" = mout[["index.treated"]],
#                  "control" = mout[["index.control"]])
# 
# m1 <- left_join(m1, ids, by = c("treat" = "id"))
# m1 <- left_join(m1, tx, by = "VUID")
# 
# saveRDS(m1, "./temp/tx_reg_data.rds")

m1 <- readRDS("./temp/tx_reg_data.rds")


####

m2 <- filter(m1) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>%
  group_by(id) %>%
  summarize(weight = sum(weight))


matches <- left_join(matches, ids, by = "id")

reg <- left_join(matches, tx, by = "VUID")

reg1 <- summary(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight))

c1 <- exp(confint(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight)))

reg2 <- summary(glm(voted_general ~ uncontested +
                      GENDER + dem + rep + yob + pred.whi + pred.bla + pred.his +
                      median_income + some_college, data = reg,
                    family = "binomial", weights = weight))

c2 <- exp(confint(glm(voted_general ~ uncontested +
                        GENDER + dem + rep + yob + pred.whi + pred.bla + pred.his +
                        median_income + some_college, data = reg,
                      family = "binomial", weights = weight)))

save(reg1, reg2, c1, c2, file = paste0("./temp/tx_match_reg.rdata"))

##### white

m2 <- filter(m1, pred.whi > 0.9) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>%
  group_by(id) %>%
  summarize(weight = sum(weight))


matches <- left_join(matches, ids, by = "id")

reg <- left_join(matches, tx, by = "VUID")

reg1 <- summary(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight))

c1 <- exp(confint(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight)))

reg2 <- summary(glm(voted_general ~ uncontested +
                      GENDER + dem + rep + yob + pred.whi + pred.bla + pred.his +
                      median_income + some_college, data = reg,
                    family = "binomial", weights = weight))

c2 <- exp(confint(glm(voted_general ~ uncontested +
                        GENDER + dem + rep + yob + pred.whi + pred.bla + pred.his +
                        median_income + some_college, data = reg,
                      family = "binomial", weights = weight)))

save(reg1, reg2, c1, c2, file = paste0("./temp/tx_white_match_reg.rdata"))

##### black

m2 <- filter(m1, pred.bla > 0.9) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>%
  group_by(id) %>%
  summarize(weight = sum(weight))


matches <- left_join(matches, ids, by = "id")

reg <- left_join(matches, tx, by = "VUID")

reg1 <- summary(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight))

c1 <- exp(confint(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight)))

reg2 <- summary(glm(voted_general ~ uncontested +
                      GENDER + dem + rep + yob + pred.whi + pred.bla + pred.his +
                      median_income + some_college, data = reg,
                    family = "binomial", weights = weight))

c2 <- exp(confint(glm(voted_general ~ uncontested +
                        GENDER + dem + rep + yob + pred.whi + pred.bla + pred.his +
                        median_income + some_college, data = reg,
                      family = "binomial", weights = weight)))

save(reg1, reg2, c1, c2, file = paste0("./temp/tx_black_match_reg.rdata"))

##### latinx

m2 <- filter(m1, pred.his > 0.9) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>%
  group_by(id) %>%
  summarize(weight = sum(weight))


matches <- left_join(matches, ids, by = "id")

reg <- left_join(matches, tx, by = "VUID")

reg1 <- summary(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight))

c1 <- exp(confint(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight)))

reg2 <- summary(glm(voted_general ~ uncontested +
                      GENDER + dem + rep + yob + pred.whi + pred.bla + pred.his +
                      median_income + some_college, data = reg,
                    family = "binomial", weights = weight))

c2 <- exp(confint(glm(voted_general ~ uncontested +
                        GENDER + dem + rep + yob + pred.whi + pred.bla + pred.his +
                        median_income + some_college, data = reg,
                      family = "binomial", weights = weight)))

save(reg1, reg2, c1, c2, file = paste0("./temp/tx_latinx_match_reg.rdata"))

##### dem

m2 <- filter(m1, dem == T) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>%
  group_by(id) %>%
  summarize(weight = sum(weight))


matches <- left_join(matches, ids, by = "id")

reg <- left_join(matches, tx, by = "VUID")

reg1 <- summary(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight))

c1 <- exp(confint(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight)))

reg2 <- summary(glm(voted_general ~ uncontested +
                      GENDER + yob + pred.whi + pred.bla + pred.his +
                      median_income + some_college, data = reg,
                    family = "binomial", weights = weight))

c2 <- exp(confint(glm(voted_general ~ uncontested +
                        GENDER + yob + pred.whi + pred.bla + pred.his +
                        median_income + some_college, data = reg,
                      family = "binomial", weights = weight)))

save(reg1, reg2, c1, c2, file = paste0("./temp/tx_dem_match_reg.rdata"))

##### rep

m2 <- filter(m1, rep == T) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>%
  group_by(id) %>%
  summarize(weight = sum(weight))


matches <- left_join(matches, ids, by = "id")

reg <- left_join(matches, tx, by = "VUID")

reg1 <- summary(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight))

c1 <- exp(confint(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight)))

reg2 <- summary(glm(voted_general ~ uncontested +
                      GENDER + yob + pred.whi + pred.bla + pred.his +
                      median_income + some_college, data = reg,
                    family = "binomial", weights = weight))

c2 <- exp(confint(glm(voted_general ~ uncontested +
                        GENDER + yob + pred.whi + pred.bla + pred.his +
                        median_income + some_college, data = reg,
                      family = "binomial", weights = weight)))

save(reg1, reg2, c1, c2, file = paste0("./temp/tx_rep_match_reg.rdata"))

##### lt40

m2 <- filter(m1, (2018 - yob) < 40) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>%
  group_by(id) %>%
  summarize(weight = sum(weight))


matches <- left_join(matches, ids, by = "id")

reg <- left_join(matches, tx, by = "VUID")

reg1 <- summary(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight))

c1 <- exp(confint(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight)))

reg2 <- summary(glm(voted_general ~ uncontested +
                      GENDER + dem + rep + yob + pred.whi + pred.bla + pred.his +
                      median_income + some_college, data = reg,
                    family = "binomial", weights = weight))

c2 <- exp(confint(glm(voted_general ~ uncontested +
                        GENDER + dem + rep + yob + pred.whi + pred.bla + pred.his +
                        median_income + some_college, data = reg,
                      family = "binomial", weights = weight)))

save(reg1, reg2, c1, c2, file = paste0("./temp/tx_lt40_match_reg.rdata"))

##### 40-65

m2 <- filter(m1, (2018 - yob) >= 40,
             (2018 - yob) < 65) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>% 
  group_by(id) %>% 
  summarize(weight = sum(weight))


matches <- left_join(matches, ids, by = "id")

reg <- left_join(matches, tx, by = "VUID")

reg1 <- summary(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight))

c1 <- exp(confint(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight)))

reg2 <- summary(glm(voted_general ~ uncontested + 
                      GENDER + dem + rep + yob + pred.whi + pred.bla + pred.his + 
                      median_income + some_college, data = reg,
                    family = "binomial", weights = weight))

c2 <- exp(confint(glm(voted_general ~ uncontested +
                        GENDER + dem + rep + yob + pred.whi + pred.bla + pred.his +
                        median_income + some_college, data = reg,
                      family = "binomial", weights = weight)))

save(reg1, reg2, c1, c2, file = paste0("./temp/tx_40_65_match_reg.rdata"))

##### gt65

m2 <- filter(m1, (2018 - yob) >= 65) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>% 
  group_by(id) %>% 
  summarize(weight = sum(weight))


matches <- left_join(matches, ids, by = "id")

reg <- left_join(matches, tx, by = "VUID")

reg1 <- summary(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight))

c1 <- exp(confint(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight)))

reg2 <- summary(glm(voted_general ~ uncontested + 
                      GENDER + dem + rep + yob + pred.whi + pred.bla + pred.his + 
                      median_income + some_college, data = reg,
                    family = "binomial", weights = weight))

c2 <- exp(confint(glm(voted_general ~ uncontested +
                        GENDER + dem + rep + yob + pred.whi + pred.bla + pred.his +
                        median_income + some_college, data = reg,
                      family = "binomial", weights = weight)))

save(reg1, reg2, c1, c2, file = paste0("./temp/tx_gt65_match_reg.rdata"))

##### men

m2 <- filter(m1, GENDER == F) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>% 
  group_by(id) %>% 
  summarize(weight = sum(weight))


matches <- left_join(matches, ids, by = "id")

reg <- left_join(matches, tx, by = "VUID")

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

save(reg1, reg2, c1, c2, file = paste0("./temp/tx_men_match_reg.rdata"))

##### women

m2 <- filter(m1, GENDER == T) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.04) %>% 
  group_by(id) %>% 
  summarize(weight = sum(weight))


matches <- left_join(matches, ids, by = "id")

reg <- inner_join(matches, tx, by = "VUID")

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

save(reg1, reg2, c1, c2, file = paste0("./temp/tx_women_match_reg.rdata"))