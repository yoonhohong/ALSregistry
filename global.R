library(readr)
library(tidyr)
library(dplyr)
library(janitor)
# read in datasets imported from dropbox 

base = read_csv('ALS_Registry_Base.csv', 
                locale = locale(encoding = "CP949"), 
                col_types = cols_only(
                  Study_ID = col_character(), 
                  Hosp_ID = col_character(), 
                  Name = col_character()
                )) %>% remove_empty("rows")

dx = read_csv("ALS_Registry_Dx.csv", 
              col_types = cols_only(
                Study_ID = col_character(), 
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
                Study_ID = col_character(),
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
                   Study_ID = col_character(),
                   Event = col_factor(), 
                   Date_event = col_date(format = "%Y.%m.%d")
                 ))
event = event[complete.cases(event),]

# close = read_csv('ALS_Registry_')





