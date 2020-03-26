library(Matching)
library(scales)
library(kableExtra)
library(tidyverse)
library(data.table)

fl <- readRDS("./temp/florida_race_census.RDS") %>% 
  ungroup()

ids <- fl %>% 
  mutate(id = row_number()) %>% 
  select(id, voter_id)

# X <- fl %>%
#   dplyr::select(dem, rep, yob, gender, white, black, latino.x, median_income, some_college)
# 
# 
# load("./temp/fl_genmatch_1.Rdata")
# 
# mout <- Matchby(Tr = fl$uncontested, X = X, by = c(X$dem, X$rep, X$gender), estimand = "ATT", Weight.matrix = genout, M = 25)
# 
# save(mout, file = "./temp/mout_fl_1.RData")
# 
# load("./temp/mout_fl_1.RData")
# 
# m1 <- data.frame("treat" = mout[["index.treated"]],
#                  "control" = mout[["index.control"]])
# 
# m1 <- left_join(m1, ids, by = c("treat" = "id"))
# m1 <- left_join(m1, fl, by = "voter_id")
# 
# saveRDS(m1, "./temp/fl_reg_data.rds")

m1 <- readRDS("./temp/fl_reg_data.rds")

########

m2 <- filter(m1) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.01) %>%
  group_by(id) %>%
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(fl, matches, by = "voter_id")

reg1 <- summary(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight))

c1 <- exp(confint(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight)))


reg2 <- summary(glm(voted_general ~ uncontested +
                      gender + dem + rep + yob + white + black + latino.x +
                      median_income + some_college, data = reg,
                    family = "binomial", weights = weight))

c2 <- exp(confint(glm(voted_general ~ uncontested +
                        gender + dem + rep + yob + white + black + latino.x +
                        median_income + some_college, data = reg,
                      family = "binomial", weights = weight)))

save(reg1, reg2, c1, c2, file = paste0("./temp/fl_match_reg.rdata"))

######## white

m2 <- filter(m1, white == T) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.01) %>% 
  group_by(id) %>% 
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(fl, matches, by = "voter_id")

reg1 <- summary(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight))

c1 <- exp(confint(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight)))


reg2 <- summary(glm(voted_general ~ uncontested + 
                      gender + dem + rep + yob + 
                      median_income + some_college, data = reg,
                    family = "binomial", weights = weight))

c2 <- exp(confint(glm(voted_general ~ uncontested + 
                        gender + dem + rep + yob + 
                        median_income + some_college, data = reg,
                      family = "binomial", weights = weight)))

save(reg1, reg2, c1, c2, file = paste0("./temp/fl_white_match_reg.rdata"))

############ black
m2 <- filter(m1, black == T) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.01) %>% 
  group_by(id) %>% 
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(fl, matches, by = "voter_id")

reg1 <- summary(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight))

c1 <- exp(confint(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight)))


reg2 <- summary(glm(voted_general ~ uncontested + 
                      gender + dem + rep + yob + 
                      median_income + some_college, data = reg,
                    family = "binomial", weights = weight))

c2 <- exp(confint(glm(voted_general ~ uncontested + 
                        gender + dem + rep + yob + 
                        median_income + some_college, data = reg,
                      family = "binomial", weights = weight)))

save(reg1, reg2, c1, c2, file = paste0("./temp/fl_black_match_reg.rdata"))

############ latinx
m2 <- filter(m1, latino.x == T) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.01) %>% 
  group_by(id) %>% 
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(fl, matches, by = "voter_id")

reg1 <- summary(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight))

c1 <- exp(confint(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight)))


reg2 <- summary(glm(voted_general ~ uncontested + 
                      gender + dem + rep + yob + 
                      median_income + some_college, data = reg,
                    family = "binomial", weights = weight))

c2 <- exp(confint(glm(voted_general ~ uncontested + 
                        gender + dem + rep + yob + 
                        median_income + some_college, data = reg,
                      family = "binomial", weights = weight)))

save(reg1, reg2, c1, c2, file = paste0("./temp/fl_latinx_match_reg.rdata"))

############ dem
m2 <- filter(m1, dem == T) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.01) %>% 
  group_by(id) %>% 
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(fl, matches, by = "voter_id")

reg1 <- summary(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight))

c1 <- exp(confint(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight)))

reg2 <- summary(glm(voted_general ~ uncontested + 
                      gender + yob + white + black + latino.x + 
                      median_income + some_college, data = reg,
                    family = "binomial", weights = weight))

c2 <- exp(confint(glm(voted_general ~ uncontested + 
                        gender + yob + white + black + latino.x + 
                        median_income + some_college, data = reg,
                      family = "binomial", weights = weight)))

save(reg1, reg2, c1, c2, file = paste0("./temp/fl_dem_match_reg.rdata"))

############ rep
m2 <- filter(m1, rep == T) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.01) %>% 
  group_by(id) %>% 
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(fl, matches, by = "voter_id")

reg1 <- summary(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight))

c1 <- exp(confint(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight)))

reg2 <- summary(glm(voted_general ~ uncontested + 
                      gender + yob + white + black + latino.x + 
                      median_income + some_college, data = reg,
                    family = "binomial", weights = weight))

c2 <- exp(confint(glm(voted_general ~ uncontested + 
                        gender + yob + white + black + latino.x + 
                        median_income + some_college, data = reg,
                      family = "binomial", weights = weight)))


save(reg1, reg2, c1, c2, file = paste0("./temp/fl_rep_match_reg.rdata"))

############ lt40
m2 <- filter(m1, (2018 - yob) < 40) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.01) %>% 
  group_by(id) %>% 
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(fl, matches, by = "voter_id")

reg1 <- summary(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight))

c1 <- exp(confint(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight)))

reg2 <- summary(glm(voted_general ~ uncontested + 
                      gender + dem + rep + yob + white + black + latino.x + 
                      median_income + some_college, data = reg,
                    family = "binomial", weights = weight))

c2 <- exp(confint(glm(voted_general ~ uncontested + 
                        gender + dem + rep + yob + white + black + latino.x + 
                        median_income + some_college, data = reg,
                      family = "binomial", weights = weight)))

save(reg1, reg2, c1, c2, file = paste0("./temp/fl_lt40_match_reg.rdata"))

############ 40 - 65
m2 <- filter(m1, (2018 - yob) >= 40,
             (2018 - yob) < 65) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.01) %>% 
  group_by(id) %>% 
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(fl, matches, by = "voter_id")

reg1 <- summary(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight))

c1 <- exp(confint(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight)))

reg2 <- summary(glm(voted_general ~ uncontested + 
                      gender + dem + rep + yob + white + black + latino.x + 
                      median_income + some_college, data = reg,
                    family = "binomial", weights = weight))

c2 <- exp(confint(glm(voted_general ~ uncontested + 
                        gender + dem + rep + yob + white + black + latino.x + 
                        median_income + some_college, data = reg,
                      family = "binomial", weights = weight)))

save(reg1, reg2, c1, c2, file = paste0("./temp/fl_40_65_match_reg.rdata"))

############ o65
m2 <- filter(m1, (2018 - yob) >= 65) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.01) %>% 
  group_by(id) %>% 
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(fl, matches, by = "voter_id")

reg1 <- summary(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight))

c1 <- exp(confint(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight)))


reg2 <- summary(glm(voted_general ~ uncontested + 
                      gender + dem + rep + yob + white + black + latino.x + 
                      median_income + some_college, data = reg,
                    family = "binomial", weights = weight))

c2 <- exp(confint(glm(voted_general ~ uncontested + 
                        gender + dem + rep + yob + white + black + latino.x + 
                        median_income + some_college, data = reg,
                      family = "binomial", weights = weight)))

save(reg1, reg2, c1, c2, file = paste0("./temp/fl_gt65_match_reg.rdata"))

############ men
m2 <- filter(m1, gender == F) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.01) %>% 
  group_by(id) %>% 
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(fl, matches, by = "voter_id")

reg1 <- summary(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight))

c1 <- exp(confint(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight)))


reg2 <- summary(glm(voted_general ~ uncontested + 
                      dem + rep + yob + white + black + latino.x + 
                      median_income + some_college, data = reg,
                    family = "binomial", weights = weight))


save(reg1, reg2, c1, c2, file = paste0("./temp/fl_men_match_reg.rdata"))

############ women
m2 <- filter(m1, gender == T) ## filter observations based on characteristics of treated voter

matches <- data.frame("id" = c(m2$treat, m2$control),
                      "weight" = 0.01) %>% 
  group_by(id) %>% 
  summarize(weight = sum(weight))

matches <- left_join(matches, ids, by = "id")

reg <- inner_join(fl, matches, by = "voter_id")

reg1 <- summary(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight))

c1 <- exp(confint(glm(voted_general ~ uncontested, data = reg, family = "binomial", weights = weight)))


reg2 <- summary(glm(voted_general ~ uncontested + 
                      dem + rep + yob + white + black + latino.x + 
                      median_income + some_college, data = reg,
                    family = "binomial", weights = weight))

c2 <- exp(confint(glm(voted_general ~ uncontested + 
                        dem + rep + yob + white + black + latino.x + 
                        median_income + some_college, data = reg,
                      family = "binomial", weights = weight)))


save(reg1, reg2, c1, c2, file = paste0("./temp/fl_women_match_reg.rdata"))