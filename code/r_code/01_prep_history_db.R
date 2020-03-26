library(data.table)
library(tidyverse)
library(tidycensus)
library(RSQLite)
library(sqldf)

db <- dbConnect(SQLite(), "D:/national_file_history.db")

j <- list.files("D:/national/04-11-2019-delivery/voter-files-by-state", full.names = T,
                pattern = "*.tab", include.dirs = T, recursive = T)

files <- j[grepl("HISTORY", j)]

states <- c()
for(f in files){
  s <- substring(f, 59, 60)
  states <- c(states, s)
}

for(f in files){
  s <- substring(f, 59, 60)
  print(s)
  tryCatch({
    k <- read.csv.sql(f, sep = "\t",
                      sql = "select LALVOTERID, 
                      General_2018_11_06,
                      General_2016_11_08,
                      General_2014_11_04,
                      General_2012_11_06,
                      General_2010_11_02
                      from file") %>%  
      mutate(state = s)
    RSQLite::dbWriteTable(db, name = paste0(s, "_history_18"), value = k, append = F, overwrite = T) 
  }, error = function(s){
    tryCatch({k <- fread(f) %>% 
      select(LALVOTERID, General_2018_11_06,
             General_2016_11_08,
             General_2014_11_04,
             General_2012_11_06,
             General_2010_11_02)
    RSQLite::dbWriteTable(db, name = paste0(s, "_history_18"), value = k, append = F, overwrite = T) },error = function(e){})
  })
  
}

n <- gsub("_history_18", "", dbListTables(db))

no_good <- files[!(states %in% n)]
########
k <- read.csv.sql("D:/national/l2-11-11-2019-delivery/VM2--AR--2019-09-21/VM2--AR--2019-09-21-VOTEHISTORY.tab",
                  sep = "\t",
                  sql = "select LALVOTERID, 
                      General_2018_11_06,
                      General_2016_11_08,
                      General_2014_11_04,
                      General_2012_11_06,
                      General_2010_11_02
                      from file") %>%  
  mutate(state = "AR")
dbWriteTable(db, name = paste0("AR", "_history_18"), value = k, append = F, overwrite = T)

########
k <- read.csv.sql("D:/national/l2-11-11-2019-delivery/VM2--AZ--2019-10-21/VM2--AZ--2019-10-21-VOTEHISTORY.tab",
                  sep = "\t",
                  sql = "select LALVOTERID, 
                      General_2018_11_06,
                      General_2016_11_08,
                      General_2014_11_04,
                      General_2012_11_06,
                      General_2010_11_02
                      from file") %>%  
  mutate(state = "AZ")
dbWriteTable(db, name = paste0("AZ", "_history_18"), value = k, append = F, overwrite = T)
########
k <- read.csv.sql("D:/national/l2-11-11-2019-delivery/VM2--ME--2019-07-17/VM2--ME--2019-07-17-VOTEHISTORY.tab",
                  sep = "\t",
                  sql = "select LALVOTERID, 
                      General_2018_11_06,
                      General_2016_11_08,
                      General_2014_11_04,
                      General_2012_11_06,
                      General_2010_11_02
                      from file") %>%  
  mutate(state = "ME")
dbWriteTable(db, name = paste0("ME", "_history_18"), value = k, append = F, overwrite = T)

########
f <- "D:/national/04-11-2019-delivery/voter-files-by-state/VM2--NE--2019-01-10/VM2--NE--2019-01-10-VOTEHISTORY.tab"
s <- "NE"

k <- fread(f)
k <- k %>% 
  select(LALVOTERID, General_2018_11_06,
         General_2016_11_08,
         General_2014_11_04,
         General_2012_11_06,
         General_2010_11_02) %>% 
  mutate(state = s)
dbWriteTable(db, name = paste0("NE", "_history_18"), value = k, append = F, overwrite = T)
########
f <- "D:/national/04-11-2019-delivery/voter-files-by-state/VM2--NM--2019-02-22/VM2--NM--2019-02-22-VOTEHISTORY.tab"
s <- "NM"

k <- fread(f)
k <- k %>% 
  select(LALVOTERID, General_2018_11_06,
         General_2016_11_08,
         General_2014_11_04,
         General_2012_11_06,
         General_2010_11_02) %>% 
  mutate(state = s)
dbWriteTable(db, name = paste0("NM", "_history_18"), value = k, append = F, overwrite = T)
########
f <- "D:/national/04-11-2019-delivery/voter-files-by-state/VM2--WV--2019-03-22/VM2--WV--2019-03-22-VOTEHISTORY.tab"
s <- "WV"

k <- fread(f)
k <- k %>% 
  select(LALVOTERID, General_2018_11_06,
         General_2016_11_08,
         General_2014_11_04,
         General_2012_11_06,
         General_2010_11_02) %>% 
  mutate(state = s)
dbWriteTable(db, name = paste0("WV", "_history_18"), value = k, append = F, overwrite = T)

#####

still_bad <- setdiff(unique(fips_codes$state), n)


#######

files <- c(
  "D:/national/l2-11-11-2019-delivery/VM2--CO--2019-08-31/VM2--CO--2019-08-31-VOTEHISTORY.tab",
  "D:/national/l2-11-11-2019-delivery/VM2--CT--2019-06-03/VM2--CT--2019-06-03-VOTEHISTORY.tab",
  "D:/national/l2-11-11-2019-delivery/VM2--MO--2019-06-03/VM2--MO--2019-06-03-VOTEHISTORY.tab",
  "D:/national/l2-11-11-2019-delivery/VM2--PA--2019-09-23/VM2--PA--2019-09-23-VOTEHISTORY.tab"
)

for(f in files){
  s <- substring(f, 41, 42)
  print(s)
  tryCatch({
    k <- read.csv.sql(f, sep = "\t",
                      sql = "select LALVOTERID, 
                      General_2018_11_06,
                      General_2016_11_08,
                      General_2014_11_04,
                      General_2012_11_06,
                      General_2010_11_02
                      from file") %>%  
      mutate(state = s)
    RSQLite::dbWriteTable(db, name = paste0(s, "_history_18"), value = k, append = F, overwrite = T) 
  }, error = function(s){
    tryCatch({k <- fread(f) %>% 
      select(LALVOTERID, General_2018_11_06,
             General_2016_11_08,
             General_2014_11_04,
             General_2012_11_06,
             General_2010_11_02)
    RSQLite::dbWriteTable(db, name = paste0(s, "_history_18"), value = k, append = F, overwrite = T) },error = function(e){})
  })
  
}


########

f <- "D:/national/l2-11-11-2019-delivery/VM2--KY--2019-11-18/VM2--KY--2019-11-18-VOTEHISTORY.tab"
s <- "KY"

k <- fread(f)
k <- k %>% 
  select(LALVOTERID, General_2018_11_06,
         General_2016_11_08,
         General_2014_11_04,
         General_2012_11_06,
         General_2010_11_02) %>% 
  mutate(state = s)
dbWriteTable(db, name = paste0("KY", "_history_18"), value = k, append = F, overwrite = T)
