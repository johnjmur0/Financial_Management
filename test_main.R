library(tidyverse)
library(plumber)
devtools::load_all('./Configuration')
devtools::load_all('./Process_Mint')
devtools::load_all('./Utilities')

#* get historical transactions aggregated 
#* @param user_name key for config file to use
#* @param read_cache whether to read transactions from cache if possible
#* @param write_cache whether to overite transactions in cache if not reading
#* @post /get_historical_data
get_historical_data = function(user_name = 'jjm', read_cache = TRUE, write_cache = TRUE) {
  
  config_file = Configuration::get_user_config(user_name)
  
  account_df = Process_Mint::get_mint_accounts(read_cache, write_cache)  
  transactions_df = Process_Mint::get_mint_transactions(read_cache, write_cache)
  investments_df = Process_Mint::get_mint_investments(read_cache, write_cache)

  if (!as.logical(read_cache)) {
    Process_Mint::close_mint_connection()
  }
  
  historical_start_date = create_datetime(2018, 1)

  transactions_df = transactions_df %>% 

    filter(date <= Sys.time()) %>%
    
    mutate(amount = if_else(transaction_type == 'debit', amount * -1, amount),
           Year = lubridate::year(date),
           Month = lubridate::month(date))

  historical_df = transactions_df %>% Process_Mint::get_historical_summary(config_file, historical_start_date)
}