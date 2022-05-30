#' obtain user config_file
#'
#' @param user_name user name indicating config file
#'
#' @return list
#' @import rjson 
#' @import stringr
#' @export
#'
get_user_config = function(user_name) {
  config_path = file.path('./Config_Files', str_c('config_', user_name, '.json'))
  config_file = rjson::fromJSON(file = config_path)
}
  
get_config_field_generic = function(config_file, field_name) {
  #don't need try catch here b/c even if config_file is blank or NULL, this returns NULL
  field_val = config_file[[field_name]]
  
  if (is.null(field_val)) {
    stop(str_c('Getting', field_name, 'from config failed', sep = ' '))
  }
  
  return (field_val)
}

#' get_numeric_val_from_config
#'
#' @param config_file user config file
#' @param field_name name of numeric field
#'
#' @return numeric
#' @import stringr
#' @export
#'
get_numeric_val_from_config = function(config_file, field_name) {
  field_val = get_config_field_generic(config_file, field_name)
  
  if (is.na(as.numeric(field_val))) {

    stop(str_c(field_name, field_val, 'from config is not numeric', sep = ' ')) 
  }
  
  return(as.numeric(field_val))
}

#' translate account json into df
#'
#' @param accounts_json json returned from mint api for accounts
#'
#' @return tibble
#' @import dplyr
#' @export
#'
get_account_df = function(accounts_json) {
  
  account_name <- account_type <- account_system_status <- value <- interest_rate <- NULL
  
  accounts_json %>% 
  
    tidyjson::spread_all() %>% 
    
    as_tibble() %>%

    janitor::clean_names() %>%
    
    select(account_name, account_type, account_system_status, value, interest_rate) %>% 
    
    filter(account_system_status == 'ACTIVE') %>% 
    
    select(-account_system_status) %>% 
    
    mutate(interest_rate = if_else(is.na(interest_rate), 0, interest_rate))
}