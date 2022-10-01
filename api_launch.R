# setwd("C:/source/financial-management")

# source("./.Rprofile")

plumber::pr("./financial_management_api.R") %>% plumber::pr_run(port = 8000)