library(stringr)
library(lubridate)
library(tidyverse)
library(dplyr)
library(readr)

processAccounts = function(dir, account)
{
  df = read_csv(str_c(dir, account, "_5.1.2016_1.27.2018.csv"),
                     col_types=cols(col_date("%m/%d/%Y"), col_character(), col_double(), col_double())) %>% 
    rename(balance = "Running Bal.") %>% 
    mutate(account = account) %>% 
    rename(amount = Amount) %>% 
    filter(!grepl("KEEP THE CHANGE TRANSFER", Description)) %>% 
    mutate(month = month(Date)) %>% 
    mutate(year = year(Date))
  
  return(df)
}

dir = "C:/Users/jjmur0/Documents/Expenses/Statements"

startMonth = 2
startYear = 2017

checking = processAccounts(str_c(dir, "/Checking/"), "checking")
saving = processAccounts(str_c(dir, "/Saving/"), "saving")

allAcct = rbind(checking, saving) %>% 
  processTimeCharges(min(checking$year))

filterYearMonth = ((startYear - min(allAcct$year)) * 12) + startMonth

allAcct = allAcct %>% filter(yearMonth >= filterYearMonth)

yearMonthList = seq(min(allAcct$yearMonth), max(allAcct$yearMonth))
  
profitDf = as.data.frame(do.call(rbind, 
             lapply(yearMonthList, function(x) evaluateProfit(allAcct, x, 2016)))) %>% 
          
  mutate(annualProfit = sum(singleMonthProfit)) %>% 
  
  mutate(extraIncome = ((59000 / 10) * .75) + 12000) %>% 

  mutate(annualSavePercent = annualProfit / 59000) %>%
  
  mutate(netProfit = (annualProfit + extraIncome) / 59000)
    


