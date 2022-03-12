mint_get_historical_summary = function(transactions, config_file, historical_start_date)
{
  transactions = transactions %>% get_monthly_summary()
    
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
#' @examples mint_get_projections(transactions, account_df, config_file, forecast_date_range, historical_start_date)
mint_get_projections = function(transactions, account_df, config_file, forecast_date_range, historical_start_date)
{
  forecast_time_series = seq(min(forecast_date_range), max(forecast_date_range), by = "month")
  
  transactions = transactions %>% get_monthly_summary()
  category_df = transactions %>% monthly_category_sum(config_file, historical_start_date)
  
  manual_adjustments = get_manual_adjustments(config_file) %>% 
    dplyr::filter(between(TimeAdj, min(forecast_date_range), max(forecast_date_range)))
  
  #TODO can also base this off historical
  annual_investment_growth = Configuration::get_numeric_val_from_config(config_file, 'Average_Investment_Growth')
  
  fixed_payments = account_df %>% get_fixed_payments(transactions, 
                                                     config_file, 
                                                     forecast_time_series, 
                                                     annual_investment_growth)
  
  current_accounts = account_df %>% get_account_balances(config_file, forecast_time_series)
  
  projection_inputs = category_df %>% get_projection_inputs(transactions, 
                                                            current_accounts,
                                                            fixed_payments,
                                                            forecast_time_series,
                                                            historical_start_date)
  
  min_monthly_savings = Configuration::get_numeric_val_from_config(config_file, 'Minimum_Monthly_Savings')
  
  projection_df = projection_inputs %>% create_projection_df(manual_adjustments, 
                                                             category_df, 
                                                             min_monthly_savings)
  #TODO deal with this better
  if (projection_df %>% select(Total_Loans) %>% colSums() == 0) {
    projection_df = projection_df %>% select(-contains("Loan"))
  }
  
  return(projection_df)
}