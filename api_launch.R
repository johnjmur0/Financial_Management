# setwd("C:/source/financial-management")
# source("./.Rprofile")

source("renv/activate.R")

plumber::pr_run(plumber::pr("./financial_management_api.R"), port = 8000)