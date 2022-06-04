library(dplyr)
library(reticulate)

#pr('./test_main.R') %>% pr_run(port=8000)

#* get historical transactions aggregated by month and category 
#* @param user_name key for config file to use
#* @param time_vec time vector to aggregate on
#* @param read_cache whether to read transactions from cache if possible
#* @param write_cache whether to overwrite transactions in cache if not reading
#* @post /get_historical_by_category
get_historical_by_category = function(user_name = 'jjm', 
                                      time_vec = c('year', 'month', 'day'), 
                                      read_cache = TRUE, 
                                      write_cache = TRUE) {
  
  config_list = utilities::get_user_config(user_name)
  
  transactions_df = mint.processor::get_mint_data_by_type_memoised('transactions', 
                                                                   user_name, 
                                                                   read_cache, 
                                                                   write_cache)
  
  historical_start_date = utilities::create_datetime(2018, 1)

  transactions_df %>% 
    mint.processor::summarize_transactions(time_vec, config_list, historical_start_date)
}

#* get current account values
#* @param user_name key for config file to us
#* @param read_cache whether to read accounts from cache if possible
#* @param write_cache whether to overwrite transactions in cache if not reading
#* @post /get_current_accounts
get_current_accounts = function(user_name = 'jjm', 
                                read_cache = FALSE, 
                                write_cache = FALSE) {

  accounts_df = mint.processor::get_mint_data_by_type_memoised('accounts', user_name, read_cache, write_cache)

  accounts_df %>% mint.processor::clean_accounts_df()
}

#* get current monthly account projection 
#* @param user_name key for config file to us
#* @param forecast_end_year last full year to be forecasted
#* @param read_cache whether to read accounts from cache if possible
#* @param write_cache whether to overwrite transactions in cache if not reading
#* @post /get_current_projections
get_current_projections = function(user_name = 'jjm', 
                                  time_vec = c('year', 'month'), 
                                  forecast_end_year = 2025,
                                  read_cache = TRUE) {

  config_list = utilities::get_user_config(user_name)
  
  historical_transactions_df = get_historical_by_category(user_name, time_vec, read_cache, write_cache) 
  
  accounts_df = get_current_accounts(user_name, read_cache, write_cache)

  forecast_date_range = c(
    lubridate::mdy(stringr::str_c(lubridate::month(Sys.time()) + 1, 1, lubridate::year(Sys.time()), sep = '/')), 
    lubridate::mdy(stringr::str_c(12, 1, forecast_end_year, sep = '/')))

  mint.processor::get_current_projections(
    historical_transactions_df, 
    accounts_df, 
    config_list, 
    forecast_date_range)
}