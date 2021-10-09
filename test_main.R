devtools::load_all("./Configuration")
devtools::load_all("./ProcessMint")

test_main = function(user_name = "jjm")
{
  config_file = Configuration::get_user_config(user_name)
  
  data_files = ProcessMint::get_mint_datasets(config_file)
  
  #TODO investments not getting formatted correctly
  
  ProcessMint::mint_getForecastSummary(data_files[[3]], 
                                       data_files[[1]],
                                       config_file,
                                       c(create_datetime(2021, 11), create_datetime(2022, 12)), 
                                       create_datetime(2018, 1))
}