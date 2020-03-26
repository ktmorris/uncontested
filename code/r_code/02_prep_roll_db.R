library(data.table)
library(tidyverse)
library(tidycensus)
library(RSQLite)
library(sqldf)

db <- dbConnect(SQLite(), "D:/national_file.db")

j <- list.files("D:/national/04-11-2019-delivery/voter-files-by-state", full.names = T,
                pattern = "*.tab", include.dirs = T, recursive = T)

files <- j[grepl("DEMOGRAPHIC", j)]


for(f in files){
  s <- substring(f, 59, 60)
  print(s)
  if(!s %in% c("HI", "NC")){
    k <- read.csv.sql(f, sep = "\t",
                      sql = "select LALVOTERID, Voters_Active, 
                  Voters_StateVoterID, Voters_CountyVoterID, 
                  Voters_FirstName, Voters_MiddleName, 
                  Voters_LastName, Voters_NameSuffix, 
                  Residence_Addresses_AddressLine, 
                  Residence_Addresses_ExtraAddressLine, 
                  Residence_Addresses_City, 
                  Residence_Addresses_State, 
                  Residence_Addresses_Zip, 
                  Residence_Addresses_ZipPlus4, 
                  Residence_Addresses_HouseNumber, 
                  Residence_Addresses_PrefixDirection, 
                  Residence_Addresses_StreetName, 
                  Residence_Addresses_Designator, 
                  Residence_Addresses_SuffixDirection, 
                  Residence_Addresses_ApartmentNum, 
                  Residence_Addresses_ApartmentType, 
                  Residence_Addresses_CensusTract, 
                  Residence_Addresses_CensusBlockGroup, 
                  Residence_Addresses_CensusBlock, 
                  Residence_Addresses_Latitude, 
                  Residence_Addresses_Longitude, 
                  Voters_Gender, Voters_Age, Voters_BirthDate, 
                  DateConfidence_Description, Parties_Description, 
                  EthnicGroups_EthnicGroup1Desc,
                  Voters_CalculatedRegDate, 
                  Voters_OfficialRegDate, US_Congressional_District, 
                  State_Senate_District, State_House_District, 
                  State_Legislative_District, County, Voters_FIPS, 
                  CommercialData_EstimatedHHIncome,
                  CommercialData_Education,
                  Precinct from file") %>% 
      mutate(state = s)
  }
  if(s %in% c("HI", "NC")){
    k <- fread(f) %>% 
      select(LALVOTERID, Voters_Active, 
             Voters_StateVoterID, Voters_CountyVoterID, 
             Voters_FirstName, Voters_MiddleName, 
             Voters_LastName, Voters_NameSuffix, 
             Residence_Addresses_AddressLine, 
             Residence_Addresses_ExtraAddressLine, 
             Residence_Addresses_City, 
             Residence_Addresses_State, 
             Residence_Addresses_Zip, 
             Residence_Addresses_ZipPlus4, 
             Residence_Addresses_HouseNumber, 
             Residence_Addresses_PrefixDirection, 
             Residence_Addresses_StreetName, 
             Residence_Addresses_Designator, 
             Residence_Addresses_SuffixDirection, 
             Residence_Addresses_ApartmentNum, 
             Residence_Addresses_ApartmentType, 
             Residence_Addresses_CensusTract, 
             Residence_Addresses_CensusBlockGroup, 
             Residence_Addresses_CensusBlock, 
             Residence_Addresses_Latitude, 
             Residence_Addresses_Longitude, 
             Voters_Gender, Voters_Age, Voters_BirthDate, 
             DateConfidence_Description, Parties_Description, 
             EthnicGroups_EthnicGroup1Desc, Voters_CalculatedRegDate, 
             Voters_OfficialRegDate, US_Congressional_District, 
             State_Senate_District, State_House_District, 
             State_Legislative_District, County, Voters_FIPS, 
             Precinct)  %>% 
      mutate(state = s)
  }
  RSQLite::dbWriteTable(db, name = s, value = k, append = F, overwrite = T) 
}
#####

files <- c(
  "D:/national/l2-11-11-2019-delivery/VM2--CO--2019-08-31/VM2--CO--2019-08-31-DEMOGRAPHIC.tab",
  "D:/national/l2-11-11-2019-delivery/VM2--CT--2019-06-03/VM2--CT--2019-06-03-DEMOGRAPHIC.tab",
  "D:/national/l2-11-11-2019-delivery/VM2--MO--2019-06-03/VM2--MO--2019-06-03-DEMOGRAPHIC.tab",
  "D:/national/l2-11-11-2019-delivery/VM2--PA--2019-09-23/VM2--PA--2019-09-23-DEMOGRAPHIC.tab"
)

for(f in files){
  s <- substring(f, 41, 42)
  print(s)

  k <- read.csv.sql(f, sep = "\t",
                    sql = "select LALVOTERID, Voters_Active, 
                Voters_StateVoterID, Voters_CountyVoterID, 
                Voters_FirstName, Voters_MiddleName, 
                Voters_LastName, Voters_NameSuffix, 
                Residence_Addresses_AddressLine, 
                Residence_Addresses_ExtraAddressLine, 
                Residence_Addresses_City, 
                Residence_Addresses_State, 
                Residence_Addresses_Zip, 
                Residence_Addresses_ZipPlus4, 
                Residence_Addresses_HouseNumber, 
                Residence_Addresses_PrefixDirection, 
                Residence_Addresses_StreetName, 
                Residence_Addresses_Designator, 
                Residence_Addresses_SuffixDirection, 
                Residence_Addresses_ApartmentNum, 
                Residence_Addresses_ApartmentType, 
                Residence_Addresses_CensusTract, 
                Residence_Addresses_CensusBlockGroup, 
                Residence_Addresses_CensusBlock, 
                Residence_Addresses_Latitude, 
                Residence_Addresses_Longitude, 
                Voters_Gender, Voters_Age, Voters_BirthDate, 
                DateConfidence_Description, Parties_Description, 
                EthnicGroups_EthnicGroup1Desc,
                Voters_CalculatedRegDate, 
                Voters_OfficialRegDate, US_Congressional_District, 
                State_Senate_District, State_House_District, 
                State_Legislative_District, County, Voters_FIPS, 
                CommercialData_EstimatedHHIncome,
                CommercialData_Education,
                Precinct from file") %>% 
    mutate(state = s)
  RSQLite::dbWriteTable(db, name = s, value = k, append = F, overwrite = T) 
}