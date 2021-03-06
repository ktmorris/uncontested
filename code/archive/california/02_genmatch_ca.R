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
for(i in 1:1){
  ca_sample <- readRDS("./temp/california_race_census.RDS") %>%
    group_by(uncontested) %>%
    sample_frac(0.01) %>%
    ungroup() %>% 
    dplyr::select(uncontested, gender, voted_primary, dem, rep, yob, pred.whi, pred.bla, pred.his, median_income, some_college)


  match_data <- ca_sample %>%
    dplyr::select(gender, voted_primary, dem, rep, yob, pred.whi, pred.bla, pred.his, median_income, some_college)
            

  genout <- GenMatch(Tr = ca_sample$uncontested, X = match_data, pop.size = 1000, cluster = cl)

  save(genout, file = paste0("./temp/ca_genmatch_", i, ".Rdata"))
}


