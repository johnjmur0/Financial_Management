mint_getHistoricalSummary = function(historicalStartDate)
{
  transactions = getMostRecentFile() %>% processTimeCharges() %>% 
    
    #Cut out big one-time musical purchases - handle separately
    dplyr::filter(!(Category == "Hobbies" & Amount < -500))
  
  categoryList = getCategoryList(transactions, historicalStartDate, includeOutlier = TRUE)
  
  historicalSpendAnalysis(categoryList)
}

mint_getForecastSummary = function(forecastDateRange, historicalStartDate)
{
  forecastStart = min(forecastDateRange)
  forecastEnd = max(forecastDateRange)
  years = seq(lubridate::year(forecastStart), lubridate::year(forecastEnd))
  forecastTime = seq(forecastStart, forecastEnd, by = "month")
  
  transactions = getMostRecentFile() %>% processTimeCharges()
  
  categoryList = getCategoryList(transactions, historicalStartDate)
  
  timeAdjustmentDf = categoryList %>% getTimeAdjustments(transactions, years, zeroGrowth = FALSE) %>% 
    dplyr::filter(between(TimeAdj, forecastStart, forecastEnd))
  
  loanStructure = forecastTime %>% getStrucutralData(growthRate)
  currentBalances = getCurrentBalances(forecastTime)
  
  finalDf = categoryList %>% createFinalDf(transactions, currentBalances, 
                                           loanStructure, forecastTime) %>%
    
    predictDf_Forward(timeAdjustmentDf, categoryList, monthsOfSavings)
  
  if (finalDf %>% select(Total_Loans) %>% colSums() == 0) {
    finalDf = finalDf %>% select(-contains("Loan"))
  }
  
  return (finalDf)
}

#TODO - break out forecast vs historical - seperate files, main calls

# Instructions: 
#   0. Put Mint transaction file in Mint folder
#   1. Source everything in Code folder
#   2. Update values in "Custom Functions"
#   3. Jump into main, should be good to go

#finalDf = main(10000, 0) %>% select(-contains("Contribution")
#print (finalDf)



###########################    INPUTS    #################################
startDate = createDateTime(2020, 1)
endDate = createDateTime(2021, 12)
historicalStartDate = createDateTime(2018, 1)

