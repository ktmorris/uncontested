
tx <- readRDS("./temp/texas_race_census.RDS")


X <- tx %>% 
  dplyr::select(GENDER, voted_primary, dem, rep, yob, pred.whi, pred.bla, pred.his, median_income, some_college)

for(i in c(1:10)){
  
  load(paste0("./temp/tx_genmatch_", i, ".Rdata"))
  
  mout <- Match(Tr = tx$uncontested, X = X, estimand = "ATT", Weight.matrix = genout, version = "fast", M = 100)
  summary(mout)
  
  save(mout, file = paste0("./temp/mout_tx_", i, ".RData"))
  
  matches <- data.frame("treated" = mout[["index.treated"]],
                        "control" = mout[["index.control"]])
  
  tx <- tx %>% 
    mutate(id = row_number())
  
  treat_row <- tx %>% 
    filter(uncontested) %>% 
    select(id, VUID)
  
  untreat_row <- tx %>% 
    filter(!uncontested) %>% 
    select(id, control_VUID = VUID)
  
  matches <- left_join(matches, treat_row, by = c("treated" = "id"))
  matches <- left_join(matches, untreat_row, by = c("control" = "id"))
  
  
  matches_v <- as.data.frame(c(matches$VUID, matches$control_VUID))
  colnames(matches_v) = "VUID"
  
  match_w <- matches_v %>% 
    group_by(VUID) %>% 
    summarize(weight = n())
  
  reg <- inner_join(tx, match_w, by = "VUID")
  reg$voted_general <- ifelse(reg$voted_general >= 1, 1, 0)
  
  reg_output <- glm(voted_general ~ uncontested, data = reg, weights = weight, family = "binomial")
  saveRDS(reg_output, paste0("./temp/match_reg_tx_", i, ".rds"))
  
  
  ###########
  load(paste0("./temp/mout_tx_", i, ".RData"))
  order <- fread("./raw_data/var_orders.csv")
  
  balance <- MatchBalance(uncontested ~ GENDER + voted_primary + dem + rep + yob +
                            pred.whi + pred.bla + pred.his + median_income + some_college, match.out = mout,
                          data = tx)
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
  
  varnames <- c("GENDER", "voted_primary", "dem", "rep", "yob",
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
  
  saveRDS(df, paste0("./temp/match_table_tx_", i, ".rds"))
  
  kable(df, escape = FALSE, align = c('l', rep('c', 99))) %>%
    add_header_above(c(" " = 1, "Means: Unmatched Data" = 2, "Means: Matched Data" = 2, "Percent Improvement" = 4), align = "c") %>%
    kable_styling(font_size = 12, full_width = F) %>%
    save_kable(file = paste0("./output/matches_tx_", i, ".html"), self_contained = T)
}