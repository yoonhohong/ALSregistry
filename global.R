# ALSregistry2 
# https://github.com/yoonhohong/ALSregistry2
# Shiny app for interactive case review 

library(readr)
library(tidyr)
library(dplyr)
library(janitor)

# Set data files
list_files = list.files("./data", pattern="*.csv")
base_file = paste("./data", list_files[grep("Base", list_files)], sep = "/")
dx_file = paste("./data", list_files[grep("Dx", list_files)], sep = "/")
fu_file = paste("./data", list_files[grep("Follow", list_files)], sep = "/")
event_file = paste("./data", list_files[grep("Event", list_files)], sep = "/")
close_file = paste("./data", list_files[grep("Close", list_files)], sep = "/")
Biobank1_file = paste("./data", list_files[grep("Biobank1", list_files)], sep = "/")
Biobank2_file = paste("./data", list_files[grep("Biobank2", list_files)], sep = "/")

# Date of registry updated
date_update = strsplit(dx_file, split = "_")[[1]][3]
date_update_registry = as.Date(date_update, format = "%Y%m%d")

# Read-in data from data files 
base = read_csv(base_file,
                col_types = cols_only(
                  Study_ID = col_factor(), 
                  Hosp_ID = col_character())
                ) %>% 
  remove_empty("rows")

dx = read_csv(dx_file, 
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
  select(Study_ID, Hosp_ID, Sex, Age_dx, Dx)

fu = read_csv(fu_file, 
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
    difftime(Date_visit, Date_visit[1], units = "weeks"))) %>%
  mutate(Time_from_latest_visit = as.numeric(
    difftime(date_update_registry, last(Date_visit), units = "weeks")))


fu_alsfrs = fu %>%
  filter(!is.na(ALSFRS)) 

fu_fvc = fu %>%
  filter(!is.na(FVC_percent))

event = read_csv(event_file,
                 col_types = cols(
                   Study_ID = col_factor(),
                   Event = col_factor(), 
                   Date_event = col_date(format = "%Y.%m.%d")
                 ))
event = event[complete.cases(event),]

close = read_csv(close_file, 
                 col_types = cols(
                   Study_ID = col_factor(),
                   Close_reason = col_factor(),
                   Date_close = col_date(format = "%Y.%m.%d")
                 ))
close = close[complete.cases(close),]

tracheo = event %>%
  filter(Event == "Tracheostomy")

colnames(tracheo) = c("Study_ID", "Close_reason", "Date_close")

close_tracheo = rbind(close, tracheo)
close_tracheo_first = close_tracheo %>%
  group_by(Study_ID) %>%
  arrange(Date_close) %>%
  filter(Date_close == first(Date_close))

close_all = merge(base, close_tracheo_first, all.x = T, by = "Study_ID") %>%
  mutate(Close_reason = as.character(Close_reason)) %>%
  select(Study_ID, Close_reason, Date_close) %>%
  mutate(Close_reason = if_else(is.na(Close_reason), "Undefined", 
                               Close_reason))
fu_latest = fu %>%
  group_by(Study_ID) %>%
  arrange(Date_visit) %>%
  filter(Date_visit == last(Date_visit)) %>%
  mutate(Date_visit_latest = Date_visit) %>%
  select(Study_ID, Date_visit_latest)

close_with_latest_visit = merge(close_all, fu_latest, all = T,
                                by = "Study_ID") 
close_with_latest_visit$Close_reason = factor(
  close_with_latest_visit$Close_reason)
levels(close_with_latest_visit$Close_reason) = list(
    Undefined = "Undefined",
    Death_or_tracheostomy = c("Death", "Tracheostomy"),
    Refer = "Refer",
    Lost_to_fu = "Lost to f/u")



bio1 = read_csv(Biobank1_file, 
                col_types = cols(
                  Study_ID = col_factor(), 
                  Hosp_ID = col_factor(),
                  Provider_Ocode = col_factor()
                ))
bio1 = bio1[complete.cases(bio1),]

bio2 = read_csv(Biobank2_file, 
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








