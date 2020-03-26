library(tidyverse)
library(Matching)
library(data.table)
library(snow)
library(parallel)
library(scales)
library(kableExtra)


##this needs to be run on NYU's high powered computer system

# 
# setwd("/scratch/km3815/matching")
# 
# NodeFile = Sys.getenv("MY_HOSTFILE")

source("./code/r_code/misc/AutoCluster4.R")
cl <- NCPUS(detectCores() - 1)


#cl <- makeCluster(c(readLines(NodeFile)), type="SOCK")

ga_sample <- readRDS("./temp/georgia_race_census.RDS") %>%
  group_by(uncontested) %>%
  sample_frac(0.01) %>%
  ungroup() %>% 
  dplyr::select(uncontested, dem, rep, yob, gender, white, black, latino.x, median_income, some_college)


match_data <- ga_sample %>%
  dplyr::select(dem, rep, yob, gender, white, black, latino.x, median_income, some_college)
          

genout <- GenMatch(Tr = ga_sample$uncontested, X = match_data, pop.size = 1000, cluster = cl)

save(genout, file = "./temp/ga_genmatch_1.Rdata")



