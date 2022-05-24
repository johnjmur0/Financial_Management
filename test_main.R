library(tidyverse)
library(plumber)
library(reticulate)
devtools::load_all('./Configuration')
devtools::load_all('./Process_Mint')
devtools::load_all('./Utilities')

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
  
  config_file = Configuration::get_user_config(user_name)
  
  transactions_df = Process_Mint::get_mint_data_by_type_memoised('transactions', read_cache, write_cache) %>%
    mutate(date = lubridate::with_tz(date, 'UTC'))
  
  historical_start_date = create_datetime(2018, 1)

  transactions_df = transactions_df %>% 

    filter(date <= Sys.time()) %>%
    
    mutate(amount = if_else(transaction_type == 'debit', amount * -1, amount),
           year = lubridate::year(date),
           month = lubridate::month(date),
           day = as.numeric(lubridate::day(date)))

  transactions_df %>% 
    Process_Mint::get_historical_summary(time_vec, config_file, historical_start_date)
}

#* get currnet account values
#* @param user_name key for config file to us
#* @param read_cache whether to read accounts from cache if possible
#* @param write_cache whether to overwrite transactions in cache if not reading
#* @post /get_current_accounts
get_current_accounts = function(user_name = 'jjm', 
                                read_cache = FALSE, 
                                write_cache = FALSE) {

  accounts_df = Process_Mint::get_mint_data_by_type_memoised('accounts', read_cache, write_cache)

  accounts_df %>% 
    filter(accountSystemStatus == 'ACTIVE') %>% 
    mutate(value = unlist(value)) %>% 
    group_by(accountType) %>% 
    summarise(Total = sum(value))  
}