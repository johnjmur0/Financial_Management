mint_getHistoricalSummary = function(transactions, config_file, historical_start_date)
{
  transactions = transactions %>% get_monthly_summary() %>% 
    
  category_df = transactions %>% monthly_category_sum(historical_start_date, include_outlier = TRUE)
  
  historicalSpendAnalysis(category_df)
}

#' Create financial projections based on past data and manual adjustments
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
mint_get_projections = function(transactions, account_df, config_file, forecast_date_range, historical_start_date)
{
  forecast_start = min(forecast_date_range)
  forecast_end = max(forecast_date_range)
  
  years = seq(lubridate::year(forecast_start), lubridate::year(forecast_end))
  forecast_time_series = seq(forecast_start, forecast_end, by = "month")
  
  transactions = transactions %>% get_monthly_summary()
  category_df = transactions %>% monthly_category_sum(config_file, historical_start_date)
  
  manual_adjustments = category_df %>% get_manual_adjustments(transactions, 
                                                              config_file, 
                                                              years, 
                                                              zeroGrowth = FALSE) %>% 
    
    dplyr::filter(between(TimeAdj, forecast_start, forecast_end))
  
  fixed_payments = account_df %>% get_fixed_payments(transactions, 
                                                     config_file, 
                                                     forecast_time_series, 
                                                     Configuration::get_investment_growth(config_file))
  
  current_accounts = account_df %>% get_account_balances(config_file, forecast_time_series)
  
  projection_df = category_df %>% get_projection_inputs(transactions, 
                                                        current_accounts, 
                                                        fixed_payments, 
                                                        forecast_time_series,
                                                        historical_start_date) %>%
    
    creat_projection_df(manual_adjustments, category_df, fixed_payments, Configuration::get_min_savings_month(config_file))
  
  if (projection_df %>% select(Total_Loans) %>% colSums() == 0) {
    projection_df = projection_df %>% select(-contains("Loan"))
  }
  
  return (projection_df)
}
