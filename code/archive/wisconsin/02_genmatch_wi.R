library(Matching)
library(data.table)
library(snow)
library(parallel)
library(scales)
library(kableExtra)
library(tidyverse)


##this needs to be run on NYU's high powered computer system

setwd("/scratch/km3815/matching")

NodeFile = Sys.getenv("MY_HOSTFILE")

cl <- makeCluster(c(readLines(NodeFile)), type="SOCK")

wi_sample <- readRDS("./temp/wisconsin_race_census.RDS") %>%
  group_by(uncontested) %>%
  sample_frac(0.01) %>%
  ungroup() %>% 
  dplyr::select(uncontested, LALVOTERID, voted_general, gender, dem, rep, median_age,
                pred.whi, pred.bla, pred.his, median_income, some_college)


match_data <- wi_sample %>%
  dplyr::select(gender, dem, rep, median_age,
                pred.whi, pred.bla, pred.his, median_income, some_college)


genout <- GenMatch(Tr = wi_sample$uncontested, X = match_data, pop.size = 1000, cluster = cl)

save(genout, file = "./temp/wi_genmatch_1.Rdata")
