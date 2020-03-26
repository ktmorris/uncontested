#### set up project
library(jtools)
library(extrafont)
library(readxl)
library(miceadds)
library(butcher)
library(splitstackshape)
library(kableExtra)
library(scales)
library(Matching)
library(tidyverse)
library(data.table)
library(rgdal)
library(wru)
library(tidycensus)
library(gender)

source("./code/r_code/misc/get_basic_census.R")

## NOTE: USER MUST HAVE CENSUS API KEY SAVED IN SYS.ENV

api_key <- Sys.getenv("CENSUS_API_KEY")


## IF YOU AREN'T ON MAIN COMPUTER, YOU CAN'T PULL VOTER FILE DATA
db_access <- Sys.info()["nodename"] == "BCJ-HIGHSPEED"


library(sqldf)
db <- dbConnect(SQLite(), "D:/rolls.db")
library(kevostools)
