library(readr)
library(tidyr)
library(dplyr)
library(janitor)
# read in datasets imported from dropbox 

base = read_csv('ALS_Registry_Base.csv', 
                locale = locale(encoding = "CP949"), 
                col_types = cols_only(
                  Study_ID = col_factor(), 
                  Hosp_ID = col_character(), 
                  Name = col_character()
                )) %>% remove_empty("rows")

dx = read_csv("ALS_Registry_Dx.csv", 
              col_types = cols_only(
                Study_ID = col_factor(), 
                Sex = col_factor(), 
                Date_birth = col_date(format = "%Y.%m.%d"),
                Date_onset = col_date(format = "%Y.%m.%d"),
                Date_dx = col_date(format = "%Y.%m.%d"),
                Date_enrollment = col_date(format = "%Y.%m.%d"),
                Onset_region = col_factor(), 
                Dx = col_factor()
              )) %>% remove_empty("rows")

dx = dx %>%
  mutate(Age_dx = as.numeric(format(Date_dx, "%Y")) - 
           as.numeric(format(Date_birth, "%Y"))) 

base = merge(base, dx, all.x = T, by = "Study_ID") %>%
  select(Study_ID, Hosp_ID, Name,Sex, Age_dx, Dx)

fu = read_csv('ALS_Registry_Followup.csv', 
              col_types = cols(
                Study_ID = col_factor(),
                Date_visit = col_date(format = "%Y.%m.%d"), 
                Wt = col_double(),
                FVC_percent = col_double(),
                ALSFRS = col_integer(),
                Mitos = col_factor()
              )) %>% remove_empty("rows")

fu = fu %>% 
  group_by(Study_ID) %>%
  arrange(Date_visit) %>%
  mutate(Visit_interval = as.numeric(
    difftime(Date_visit, Date_visit[1], units = "weeks"))) 

fu_alsfrs = fu %>%
  filter(!is.na(ALSFRS)) 

fu_fvc = fu %>%
  filter(!is.na(FVC_percent))

event = read_csv('ALS_Registry_Event.csv',
                 col_types = cols(
                   Study_ID = col_factor(),
                   Event = col_factor(), 
                   Date_event = col_date(format = "%Y.%m.%d")
                 ))
event = event[complete.cases(event),]

close = read_csv('ALS_Registry_Close.csv', 
                 col_types = cols(
                   Study_ID = col_factor(),
                   Close_reason = col_factor(),
                   Date_close = col_date(format = "%Y.%m.%d")
                 ))
close = close[complete.cases(close),]

bio1 = read_csv("ALS_Registry_Biobank1.csv", 
                col_types = cols(
                  Study_ID = col_factor(), 
                  Hosp_ID = col_factor(),
                  Provider_Ocode = col_factor()
                ))
bio1 = bio1[complete.cases(bio1),]

bio2 = read_csv("ALS_Registry_Biobank2.csv", 
                col_types = cols(
                  Provider_Ocode = col_factor(), 
                  Sample_Bcode = col_character()
                ))
bio2 = bio2[complete.cases(bio2),]

bio = merge(bio1, bio2, all.y = T, by = "Provider_Ocode")
# setdiff(bio2$Provider_Ocode, bio1$Provider_Ocode)

ser = bio[grep("SER", bio$Sample_Bcode), ]
buf = bio[grep("BUF", bio$Sample_Bcode), ]
pla = bio[grep("PLA", bio$Sample_Bcode), ]
csf = bio[grep("CSF", bio$Sample_Bcode), ]

ser$visit_no = regmatches(ser$Sample_Bcode, regexpr("SER..", ser$Sample_Bcode))
ser$visit_no = regmatches(ser$visit_no, regexpr("[0-9][0-9]", ser$visit_no))
ser$visit_no = as.integer(ser$visit_no)







