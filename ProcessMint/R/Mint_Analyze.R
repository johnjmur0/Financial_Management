mint_getHistoricalSummary = function(transactions, config_file, historical_start_date)
{
  transactions = transactions %>% get_monthly_summary() %>% 
    
  category_df = transactions %>% monthly_category_sum(historical_start_date, include_outlier = TRUE)
  
  historicalSpendAnalysis(category_df)
}

#' Create financial forecast based on past data
#'
#' @param transactions df of transactions from mint by category
#' @param historical_start_date start of historical data
#' @param forecast_date_range date range to forecast
#' @param config_file user config file 
#'
#' @return
#' @export
#'
#' @examples
mint_getForecastSummary = function(transactions, account_df, config_file, forecast_date_range, historical_start_date)
{
  forecast_start = min(forecast_date_range)
  forecast_end = max(forecast_date_range)
  
  years = seq(lubridate::year(forecast_start), lubridate::year(forecast_end))
  forecast_time_series = seq(forecast_start, forecast_end, by = "month")
  
  transactions = transactions %>% get_monthly_summary()
  category_df = transactions %>% monthly_category_sum(config_file, historical_start_date)
  
  manual_adjustments = category_df %>% get_manual_adjustments(transactions, config_file, years, zeroGrowth = FALSE) %>% 
    dplyr::filter(between(TimeAdj, forecast_start, forecast_end))
  
  account_changes = account_df %>% get_account_changes(transactions, config_file, forecast_date_range, 
                                                       Configuration::get_investment_growth(config_file))
  
  current_accounts = account_df %>% get_account_balances(config_file, forecast_date_range)
  
  forecast_df = category_df %>% createFinalDf(transactions, 
                                              current_accounts, 
                                              account_changes, 
                                              forecast_date_range) %>%
    
    predictDf_Forward(current_accounts, category_df, Configuration::get_min_savings_month(config_file))
  
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

