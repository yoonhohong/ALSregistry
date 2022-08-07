
# load data 
base = readRDS("data/base.rds")
dx = readRDS("data/dx.rds")
fu = readRDS("data/fu.rds")
fu_alsfrs = readRDS("data/fu_alsfrs.rds")
fu_fvc = readRDS("data/fu_fvc.rds")
fu_wt = readRDS("data/fu_wt.rds")
event = readRDS("data/event.rds")
close = readRDS("data/close.rds")
course = readRDS("data/course.rds")

# define function 
conditional <- function(condition, success) {
  if (condition) success else TRUE
}

