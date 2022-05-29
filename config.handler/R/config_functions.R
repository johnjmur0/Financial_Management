#TODO sure how to handle user name at this point
#' obtain user config_file
#'
#' @param user_name user name indicating config file
#'
#' @return
#' @export
#'
#' @examples
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
#' @return
#' @export
#'
#' @examples
get_numeric_val_from_config = function(config_file, field_name) {
  field_val = get_config_field_generic(config_file, field_name)
  
  if (is.na(as.numeric(field_val))) { 
    stop(str_c(field_name, field_val, 'from config is not numeric', sep = ' ')) 
  }
  
  return(as.numeric(field_val))
}

#' translate account json into df
#'
#' @param account_json json returned from mint api for accounts
#'
#' @return
#' @export
#'
#' @examples
get_account_df = function(accounts_json) {
  
  accounts_json %>% 
  
    tidyjson::spread_all() %>% 
    
    as_tibble() %>%
    
    select(accountName, accountType, accountSystemStatus, value, interestRate) %>% 
    
    dplyr::filter(accountSystemStatus == 'ACTIVE') %>% 
    
    select(-accountSystemStatus) %>% 
    
    mutate(interestRate = if_else(is.na(interestRate), 0, interestRate))
}