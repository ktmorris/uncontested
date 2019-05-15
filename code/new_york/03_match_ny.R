library(tidyverse)
library(Matching)
library(data.table)
library(snow)
library(parallel)
library(scales)
library(kableExtra)

setwd("H:/Public/Democracy/Voting Rights & Elections/data/uncontested")


i <- 1


load(file = paste0("./temp/ny_genmatch_", i, ".Rdata"))

ny <- readRDS("./temp/new_york_race_census.RDS")


X <- ny %>% 
  dplyr::select(gender, voted_primary, dem, rep, yob, pred.whi, pred.bla, pred.his, median_income, some_college)



mout <- Match(Tr = ny$uncontested, X = X, estimand = "ATT", Weight.matrix = genout, version = "fast", M = 100)
summary(mout)

save(mout, file = paste0("./temp/mout_ny_", i, ".RData"))

matches <- data.frame("treated" = mout[["index.treated"]],
                      "control" = mout[["index.control"]])

ny <- ny %>% 
  mutate(id = row_number())

treat_row <- ny %>% 
  filter(uncontested) %>% 
  select(id, nys_id)

untreat_row <- ny %>% 
  filter(!uncontested) %>% 
  select(id, control_nys_id = nys_id)

matches <- left_join(matches, treat_row, by = c("treated" = "id"))
matches <- left_join(matches, untreat_row, by = c("control" = "id"))


matches_v <- as.data.frame(c(matches$nys_id, matches$control_nys_id))
colnames(matches_v) = "nys_id"

match_w <- matches_v %>% 
  group_by(nys_id) %>% 
  summarize(weight = n())

reg <- inner_join(ny, match_w, by = "nys_id")
reg$voted_general <- ifelse(reg$voted_general >= 1, 1, 0)

reg_output <- glm(voted_general ~ uncontested, data = reg, weights = weight, family = "binomial")
saveRDS(reg_output, paste0("./temp/match_reg_ny_", i, ".rds"))


###########
load(paste0("./temp/mout_ny_", i, ".RData"))
order <- fread("./raw_data/var_orders.csv")

balance <- MatchBalance(uncontested ~ gender + voted_primary + dem + rep + yob +
                          pred.whi + pred.bla + pred.his + median_income + some_college, match.out = mout,
                        data = ny)
TrMean <- c()
PreMean <- c()
PreQQmed <- c()
PreQQmean <- c()
PreQQmax <- c()
PostMean <- c()
PostQQmed <- c()
PostQQmean <- c()
PostQQmax <- c()

for(j in c(1:length(balance$BeforeMatching))){
  TrMean <- unlist(c(TrMean, balance$BeforeMatching[[j]][3][1]))
  PreMean <- unlist(c(PreMean, balance$BeforeMatching[[j]][4][1]))
  PreQQmed <- unlist(c(PreQQmed, balance$BeforeMatching[[j]]$qqsummary[2]))
  PreQQmean <- unlist(c(PreQQmean, balance$BeforeMatching[[j]]$qqsummary[1]))
  PreQQmax <- unlist(c(PreQQmax, balance$BeforeMatching[[j]]$qqsummary[3]))
  
  PostMean <- unlist(c(PostMean, balance$AfterMatching[[j]][4][1]))
  PostQQmed <- unlist(c(PostQQmed, balance$AfterMatching[[j]]$qqsummary[2]))
  PostQQmean <- unlist(c(PostQQmean, balance$AfterMatching[[j]]$qqsummary[1]))
  PostQQmax <- unlist(c(PostQQmax, balance$AfterMatching[[j]]$qqsummary[3]))
}

varnames <- c("gender", "voted_primary", "dem", "rep", "yob",
              "pred.whi", "pred.bla", "pred.his", "median_income", "some_college")


df <- data.frame("TrMean" = TrMean,
                 "TrMean2" = TrMean,
                 "PreMean" = PreMean,
                 "PreQQmed" = PreQQmed,
                 "PreQQmean" = PreQQmean,
                 "PreQQmax" = PreQQmax,
                 "PostMean" = PostMean,
                 "PostQQmed" = PostQQmed,
                 "PostQQmean" = PostQQmean,
                 "PostQQmax" = PostQQmax,
                 "names" = varnames) %>%
  mutate(change_mean = 1 - (abs(TrMean - PostMean) / abs(TrMean - PreMean)),
         change_eqqmed = 1 - abs(PostQQmed / PreQQmed),
         change_eqqmean = 1 - abs(PostQQmean / PreQQmean),
         change_eqqmax = 1 - abs(PostQQmax / PreQQmax)) %>%
  mutate_at(vars(TrMean, PreMean, TrMean2, PostMean), funs(comma(round(., 2), accuracy = .01))) %>%
  mutate_at(vars(change_mean, change_eqqmed, change_eqqmean, change_eqqmax), funs(round(. * 100, 2)))

df <- full_join(df, order, by = c("names" = "variable")) %>%
  arrange(order) %>%
  select(name, TrMean, PreMean, TrMean2, PostMean, change_mean, change_eqqmed, change_eqqmean, change_eqqmax) %>%
  filter(!is.na(TrMean))

colnames(df) <- c("", "Treated", "Control", "Treated", "Control", "Mean Diff", "eQQ Med", "eQQ Mean", "eQQ Max")

saveRDS(df, paste0("./temp/match_table_ny_", i, ".rds"))

kable(df, escape = FALSE, align = c('l', rep('c', 99))) %>%
  add_header_above(c(" " = 1, "Means: Unmatched Data" = 2, "Means: Matched Data" = 2, "Percent Improvement" = 4), align = "c") %>%
  kable_styling(font_size = 12, full_width = F) %>%
  save_kable(file = paste0("./output/matches_ny_", i, ".html"), self_contained = T)

to <- reg %>% 
  group_by(uncontested) %>% 
  summarize(weighted.mean(voted_general, weight))
saveRDS(to, paste0("./temp/turnout_match_ny_", i, ".rds"))
