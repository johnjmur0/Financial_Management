mint_getHistoricalSummary = function(transactions, historical_start_date)
{
  transactions = transactions %>% get_monthly_summary() %>% 
    
    #Look at outliers from user config file, integrate with monthly_category_sum
    #Cut out big one-time musical purchases - handle separately
    #dplyr::filter(!(Category == "Hobbies" & Amount < -500))
  
  category_df = transactions %>% monthly_category_sum(historical_start_date, include_outlier = TRUE)
  
  historicalSpendAnalysis(category_df)
}

mint_getForecastSummary = function(transactions, forecast_date_range, historicalStartDate)
{
  forecast_start = min(forecast_date_range)
  forecast_end = max(forecast_date_range)
  
  years = seq(lubridate::year(forecast_start), lubridate::year(forecast_end))
  forecast_time_series = seq(forecast_start, forecast_end, by = "month")
  
  transactions = transactions %>% get_monthly_summary()
  
  category_df = transactions %>% monthly_category_sum(historical_start_date)
  
  #Get these from config
  timeAdjustmentDf = category_df %>% getTimeAdjustments(transactions, years, zeroGrowth = FALSE) %>% 
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
historical_start_date = create_datetime(2018, 1)

