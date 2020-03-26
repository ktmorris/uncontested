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

# source("./code/r_code/misc/AutoCluster4.R")
# cl <- NCPUS(detectCores() - 1)



for(i in 1:1){
  ma_sample <- readRDS("./temp/mass_race_census.RDS") %>%
    group_by(uncontested) %>%
    sample_frac(0.01) %>%
    ungroup() %>% 
    dplyr::select(uncontested, gender, dem, rep, yob, pred.whi, pred.bla, pred.his, median_income, some_college)


  match_data <- ma_sample %>%
    dplyr::select(gender, dem, rep, yob, pred.whi, pred.bla, pred.his, median_income, some_college)
            

  genout <- GenMatch(Tr = ma_sample$uncontested, X = match_data, pop.size = 1000, cluster = cl)

  save(genout, file = paste0("./temp/ma_genmatch_", i, ".Rdata"))
}


