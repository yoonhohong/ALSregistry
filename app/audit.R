base = readRDS("data/base.rds")
dx = readRDS("data/dx.rds")
fu = readRDS("data/fu.rds")
fu_alsfrs = readRDS("data/fu_alsfrs.rds")
fu_fvc = readRDS("data/fu_fvc.rds")
fu_wt = readRDS("data/fu_wt.rds")
event = readRDS("data/event.rds")
close = readRDS("data/close.rds")

library(tidyverse)
library(googlesheets4)

# Dx missing 
dx_na = dx %>%
  filter(is.na(Dx)) %>%
  select(Study_ID, Date_dx, Dx)
dim(dx_na)  

dx_na %>%
  inner_join(base, by = "Study_ID") %>%
  select(Study_ID, Hosp_ID, Name, Date_dx, Dx.x, Dx.y) -> dx_na

ss1 = gs4_create("dx_missing_20211116", 
                 sheets = dx_na)

# Event fu 

date_update_registry = Sys.Date() # 2021-11-16

# 가장 초기 등록된 환자들부터 ᆫ분기별로 리뷰 











