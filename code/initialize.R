#### set up project

library(Matching)
library(tidyverse)
library(data.table)
library(rgdal)
library(wru)
library(tidycensus)

## NOTE: USER MUST HAVE CENSUS API KEY SAVED IN SYS.ENV

api_key <- Sys.getenv("CENSUS_API_KEY")


## IF YOU AREN'T ON MAIN COMPUTER, YOU CAN'T PULL VOTER FILE DATA
db_access <- Sys.info()["nodename"] == "BCJ-HIGHSPEED"

if(db_access){
  library(sqldf)
  db <- dbConnect(SQLite(), "D:/rolls.db")
  library(kevostools)
}
