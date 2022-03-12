library(tidyverse)
devtools::load_all('./Configuration')
devtools::load_all('./Process_Mint')
devtools::load_all('./Utilities')

test_main = function(user_name = 'jjm', read_cache = TRUE, write_cache = TRUE) {
  
  config_file = Configuration::get_user_config(user_name)
  
  account_df = Process_Mint::get_mint_accounts(read_cache, write_cache)  
  transactions_df = Process_Mint::get_mint_transactions(read_cache, write_cache)
  investments_df = Process_Mint::get_mint_investments(read_cache, write_cache)

  if (!read_cache) {
    Process_Mint::close_mint_connection()
  }
  
  forecast_range = c(create_datetime(2021, 11), create_datetime(2022, 12))
  historical_start_date = create_datetime(2018, 1)

  projection_df = Process_Mint::mint_get_projections(transactions_df, 
                                                    account_df,
                                                    config_file,
                                                    forecast_range, 
                                                    historical_start_date)
}