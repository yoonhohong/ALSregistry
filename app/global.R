# ALSregistry
# https://github.com/yoonhohong/ALSregistry
# Shiny app for interactive case review 

library(tidyverse)
library(googlesheets4)

base = read_sheet("https://docs.google.com/spreadsheets/d/1nIWHVDt2CHALpguWipeAtXn5jXwudliyXxvSe3i781o/edit?usp=sharing")
dx = read_sheet("https://docs.google.com/spreadsheets/d/1j1sFQdk9g3NvqqCO6V3mXtf0AoUXNCBmPPXF-WKj3Sk/edit?usp=sharing")
fu = read_sheet("https://docs.google.com/spreadsheets/d/1ONU-QmIXBHV2AdHkXTBsavlF_y-5EomIXOD686LpGHE/edit?usp=sharing")
event = read_sheet("https://docs.google.com/spreadsheets/d/1UE6xgj2wn4bs77NiJduMoPJGr_v2p6pNfaQW1GUr-ik/edit?usp=sharing")
close = read_sheet("https://docs.google.com/spreadsheets/d/1oEosSmRXCRr5gVxmpIIwGLH7G-1UyKTds9MtxA2p9S0/edit?usp=sharing")
# biobank1 = read_sheet("https://docs.google.com/spreadsheets/d/1s_hc50zUIa9htn9CWXUMfkGf41XlNqLUqU8vMexSlF4/edit?usp=sharing")
# biobank2 = read_sheet("https://docs.google.com/spreadsheets/d/1KKYsXyDcYk14XWdg-oBENa970-DKYdWO7cqmBsBy2D0/edit?usp=sharing")

# date conversion 
dx = within(dx, {
  Date_birth = as.Date(Date_birth, format = "%Y-%m-%d")
  Date_onset = as.Date(Date_onset, format = "%Y-%m-%d")
  Date_dx = as.Date(Date_dx, format = "%Y-%m-%d")
  Date_enrollment = as.Date(Date_enrollment, format = "%Y-%m-%d")
})
fu = within(fu, {
  Date_visit = as.Date(Date_visit, format = "%Y-%m-%d")
  Date_FVC = as.Date(Date_FVC, format = "%Y-%m-%d")
})
event$Date_event = as.Date(event$Date_event, format = "%Y-%m-%d")
event$Event = factor(event$Event)
close$Date_close = as.Date(close$Date_close, format = "%Y-%m-%d")
close$Close_reason = factor(close$Close_reason)

# data cleaning 
# base
# dx
# fu
# event
# close 

# age at dx 
dx = dx %>%
  mutate(Age_dx = as.numeric(format(Date_dx, "%Y")) - 
           as.numeric(format(Date_birth, "%Y"))) 

# base supplemented with Sex, Dx and Age_dx 
base = dx %>%
  inner_join(base, by = "Study_ID") %>%
  select(Study_ID, Hosp_ID, Name, Sex, Dx, Age_dx)

# dx, fu, event, and close 
dx_temp = dx %>%
  select(Study_ID, Date_enrollment) %>%
  rename(Date = Date_enrollment) %>%
  mutate(Milestone = "enrolled")

fu_temp = fu %>%
  select(Study_ID, Date_visit) %>%
  rename(Date = Date_visit) %>%
  mutate(Milestone = "fu")

dx_fu = rbind(dx_temp, fu_temp) 
dx_fu %>%
  group_by(Study_ID, Date) %>%
  summarise(n = n()) %>%
  filter(n > 1) -> temp 
dx_fu %>%
  inner_join(temp, by = c("Study_ID", "Date")) %>%
  arrange(Study_ID, Date) %>%
  filter(Milestone == "fu") -> temp2
dx_fu2 = dx_fu %>%
  anti_join(temp2, by = c("Study_ID", "Date")) 
dx_fu2 %>%
  group_by(Study_ID, Date) %>%
  summarise(n = n()) %>%
  filter(n > 1) 

event_temp = event %>%
  select(Study_ID, Date_event, Event) %>%
  rename(Date = Date_event, Milestone = Event) %>%
  filter(!is.na(Date))
event %>%
  filter(is.na(Date_event))

dx_fu2_event = rbind(dx_fu2, event_temp)
dx_fu2_event %>%
  group_by(Study_ID, Date) %>%
  summarise(n = n()) %>%
  filter(n > 1) -> temp3 
dx_fu2_event %>%
  inner_join(temp3, by = c("Study_ID", "Date")) %>%
  arrange(Study_ID, Date) -> temp4 
data.frame(temp4) %>%
  filter(!(Milestone == "fu"))

close_temp = close %>%
  select(Study_ID, Date_close, Close_reason) %>%
  rename(Date = Date_close, Milestone = Close_reason)

close_temp %>%
  group_by(Study_ID, Date) %>%
  summarise(n = n()) %>%
  filter(n > 1)

course = rbind(dx_fu2_event, close_temp)

# ALSFRS, FVC, Wt longitudinal change 
fu1 = fu %>%
  select(Study_ID, Date_visit, Wt, ALSFRS) %>%
  filter(!(is.na(Wt) & is.na(ALSFRS))) 
fu2 = fu %>%
  select(Study_ID, Date_FVC, FVC) %>%
  filter(!is.na(Date_FVC)) %>%
  rename(Date_visit = Date_FVC)
fu = fu1 %>%
  full_join(fu2, by = c("Study_ID", "Date_visit")) %>%
  rename(Date = Date_visit) %>%
  mutate(Milestone = "fu")

dx1 = dx %>%
  select(Study_ID, Date_enrollment, Wt_enr, ALSFRS) %>%
  rename(Date = Date_enrollment, Wt = Wt_enr) %>%
  mutate(FVC = NA, Milestone = "enrolled")
dx1$FVC = as.numeric(dx1$FVC)

fu3 = rbind(fu, dx1)
fu3 = fu3 %>% 
  select(-Milestone) %>%
  filter(!(is.na(Wt) & is.na(ALSFRS) & is.na(FVC))) %>%
  distinct()

fu3 %>%
  group_by(Study_ID, Date) %>%
  summarise(n = n()) %>%
  filter(n > 1) -> temp 

fu3 %>% 
  inner_join(temp, by = c("Study_ID", "Date")) %>%
  arrange(Study_ID, Date) -> temp2
data.frame(temp2) %>% select(-n)

fu4 = fu3 %>% 
  anti_join(temp2, by = c("Study_ID", "Date")) %>%
  arrange(Study_ID) 
fu4 %>%
  group_by(Study_ID, Date) %>%
  summarise(n = n()) %>%
  filter(n > 1)

fu = fu4

# fu_alsfrs 
fu_alsfrs = fu %>%
  filter(!is.na(ALSFRS)) %>%
  group_by(Study_ID) %>%
  mutate(Visit_interval = as.numeric(Date - min(Date))/365*12)

# fu_fvc 
fu_fvc = fu %>% 
  filter(!is.na(FVC)) %>%
  group_by(Study_ID) %>%
  mutate(Visit_interval = as.numeric(Date - min(Date))/365*12)


fu_wt = fu %>% 
  filter(!is.na(Wt)) %>%
  group_by(Study_ID) %>%
  mutate(Visit_interval = as.numeric(Date - min(Date))/365*12)


saveRDS(base, "data/base.rds")
saveRDS(dx, "data/dx.rds")
saveRDS(fu, "data/fu.rds")
saveRDS(fu_alsfrs, "data/fu_alsfrs.rds")
saveRDS(fu_fvc, "data/fu_fvc.rds")
saveRDS(fu_wt, "data/fu_wt.rds")
saveRDS(event, "data/event.rds")
saveRDS(close, "data/close.rds")
saveRDS(course, "data/course.rds")

# biorepository 
# bio = merge(bio1, bio2, all.y = T, by = "Provider_Ocode")
# bio$Provider_Bcode = regmatches(bio$Sample_Bcode, regexpr("^........", bio$Sample_Bcode))
# # setdiff(bio2$Provider_Ocode, bio1$Provider_Ocode)
# bio <- bio %>%
#   mutate(sample = case_when(
#     grepl("SER", Sample_Bcode) ~ "SERUM", 
#     grepl("BUF", Sample_Bcode) ~ "BUFFYCOAT", 
#     grepl("PLA", Sample_Bcode) ~ "PLASMA", 
#     grepl("CSF", Sample_Bcode) ~ "CSF", 
#     grepl("URN", Sample_Bcode) ~ "URINE"
#   ))
# 
# ser = bio[grep("SER", bio$Sample_Bcode), ]
# buf = bio[grep("BUF", bio$Sample_Bcode), ]
# pla = bio[grep("PLA", bio$Sample_Bcode), ]
# csf = bio[grep("CSF", bio$Sample_Bcode), ]
# urn = bio[grep("URN", bio$Sample_Bcode), ]
# 
# ser$visit_no = regmatches(ser$Sample_Bcode, regexpr("SER..", ser$Sample_Bcode))
# ser$visit_no = regmatches(ser$visit_no, regexpr("[0-9][0-9]", ser$visit_no))
# ser$visit_no = as.integer(ser$visit_no)
# 
# ser_temp = ser %>%
#   group_by(Study_ID, Provider_Ocode, Hosp_ID, visit_no) %>%
#   summarize(Sample_count = n())
# 
# pla$visit_no = regmatches(pla$Sample_Bcode, regexpr("PLA..", pla$Sample_Bcode))
# pla$visit_no = regmatches(pla$visit_no, regexpr("[0-9][0-9]", pla$visit_no))
# pla$visit_no = as.integer(pla$visit_no)
# 
# pla_temp = pla %>%
#   group_by(Study_ID, Provider_Ocode, Hosp_ID, visit_no) %>%
#   summarize(Sample_count = n())
# 
# 
# buf$visit_no = regmatches(buf$Sample_Bcode, regexpr("BUF..", buf$Sample_Bcode))
# buf$visit_no = regmatches(buf$visit_no, regexpr("[0-9][0-9]", buf$visit_no))
# buf$visit_no = as.integer(buf$visit_no)
# 
# buf_temp = buf %>%
#   group_by(Study_ID, Provider_Ocode, Hosp_ID, visit_no) %>%
#   summarize(Sample_count = n())
# 
# csf$visit_no = regmatches(csf$Sample_Bcode, regexpr("CSF..", csf$Sample_Bcode))
# csf$visit_no = regmatches(csf$visit_no, regexpr("[0-9][0-9]", csf$visit_no))
# csf$visit_no = as.integer(csf$visit_no)
# 
# csf_temp = csf %>%
#   group_by(Study_ID, Provider_Ocode, Hosp_ID, visit_no) %>%
#   summarize(Sample_count = n())
# 
# urn$visit_no = regmatches(urn$Sample_Bcode, regexpr("URN..", urn$Sample_Bcode))
# urn$visit_no = regmatches(urn$visit_no, regexpr("[0-9][0-9]", urn$visit_no))
# urn$visit_no = as.integer(urn$visit_no)
# 
# urn_temp = urn %>%
#   group_by(Study_ID, Provider_Ocode, Hosp_ID, visit_no) %>%
#   summarize(Sample_count = n())


