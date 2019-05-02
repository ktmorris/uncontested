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
  fl_sample <- readRDS("./temp/florida_race_census.RDS") %>%
    group_by(uncontested) %>%
    sample_frac(0.01) %>%
    ungroup() %>% 
    dplyr::select(uncontested, dem, rep, yob, gender, white, black, latino.x, voted_primary, median_income, some_college)


  match_data <- fl_sample %>%
    dplyr::select(dem, rep, yob, gender, white, black, latino.x, voted_primary, median_income, some_college)
            

  genout <- GenMatch(Tr = fl_sample$uncontested, X = match_data, pop.size = 1000, cluster = cl)

  save(genout, file = paste0("./temp/fl_genmatch_", i, ".Rdata"))
}


