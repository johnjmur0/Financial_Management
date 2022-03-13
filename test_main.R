library(tidyverse)
library(plumber)
library(reticulate)
devtools::load_all('./Configuration')
devtools::load_all('./Process_Mint')
devtools::load_all('./Utilities')

#* get historical transactions aggregated by month and category 
#* @param user_name key for config file to use
#* @param time_vec time vector to aggregate on
#* @param read_cache whether to read transactions from cache if possible
#* @param write_cache whether to overite transactions in cache if not reading
#* @post /get_historical_by_category
get_historical_by_category = function(user_name = 'jjm', 
                                       = c('Year', 'Month', 'Day'), 
                                      read_cache = TRUE, 
                                      write_cache = TRUE) {
  
  config_file = Configuration::get_user_config(user_name)
  time_vec
  transactions_df = Process_Mint::get_mint_data_by_type_memoised('transactions', read_cache, write_cache)
  
  historical_start_date = create_datetime(2018, 1)

  transactions_df = transactions_df %>% 

    filter(date <= Sys.time()) %>%
    
    mutate(amount = if_else(transaction_type == 'debit', amount * -1, amount),
           Year = lubridate::year(date),
           Month = lubridate::month(date),
           Day = as.numeric(lubridate::day(date)))

  transactions_df %>% 
    Process_Mint::get_historical_summary(time_vec, config_file, historical_start_date)
}