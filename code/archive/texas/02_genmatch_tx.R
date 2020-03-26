library(tidyverse)
library(Matching)
library(data.table)
library(snow)
library(parallel)
library(scales)
library(kableExtra)


##this needs to be run on NYU's high powered computer system

setwd("/scratch/km3815/matching")

NodeFile = Sys.getenv("MY_HOSTFILE")


cl <- makeCluster(c(readLines(NodeFile)), type="SOCK")
for(i in 1:10){
  tx_sample <- readRDS("./temp/texas_race_census.RDS") %>%
    group_by(uncontested) %>%
    sample_frac(0.01) %>%
    ungroup()
  
  
  match_data <- tx_sample %>%
    dplyr::select(GENDER, dem, rep, yob, pred.whi, pred.bla, pred.his, median_income, some_college)
  
  
  genout <- GenMatch(Tr = tx_sample$uncontested, X = match_data, pop.size = 1000, cluster = cl)
  
  save(genout, file = paste0("./temp/tx_genmatch_", i, ".Rdata"))
}


