library(tidyverse)
devtools::load_all('./Configuration')
devtools::load_all('./Process_Mint')
devtools::load_all('./Utilities')

test_main = function(user_name = 'jjm')
{
  config_file = Configuration::get_user_config(user_name)
  
  #TODO investments not getting formatted correctly
  data_files = Process_Mint::get_mint_datasets(config_file)
  
  account_df = Configuration::get_account_df(data_files[[1]])
  
  projection_df = Process_Mint::mint_get_projections(data_files[[3]], 
                                                    account_df,
                                                    config_file,
                                                    c(create_datetime(2021, 11), create_datetime(2022, 12)), 
                                                    create_datetime(2018, 1))
}