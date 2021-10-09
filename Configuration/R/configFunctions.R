#TODO sure how to handle user name at this point
#' obtain user config_file
#'
#' @param user_name user name indicating config file
#'
#' @return
#' @export
#'
#' @examples
get_user_config = function(user_name)
{
  config_filePath = file.path("./Config_Files", str_c("Config_", user_name, ".json"))
  config_file = rjson::fromJSON(file = config_filePath)
}

#' current salary
#'
#' @param config_file user json config
#'
#' @return
#' @export
#'
#' @examples
get_base_salary = function(config_file) { config_file[["Base_Salary"]] }

#' minimum number of months of average spending required (avg spending * min_months = required savings)
#'
#' @param config_file 
#'
#' @return
#' @export
#'
#' @examples
get_min_savings_month = function(config_file) { config_file[["Minimum_Monthly_Savings"]] }

#' annual contribution in dollars
#'
#' @param config_file user json config
#'
#' @return
#' @export
#'
#' @examples
get_401k_contribution_annual = function(config_file) { config_file[["Annual_401k_Contribution"]] }

#TODO can also base this off historical
#' annual growth rate for investments
#'
#' @param config_file user json config
#'
#' @return
#' @export
#'
#' @examples
get_investment_growth = function(config_file) { config_file[["Average_Investment_Growth"]] }

#' average annual raise
#'
#' @param config_file user json config  
#'
#' @return
#' @export
#'
#' @examples
get_average_raise = function(config_file) 
{ 
  nominal_raise = config_file[["Average_Raise"]] * config_file[["Base_Salary"]]
}

#' month for first month of fiscal year (for bonuses, raises)
#'
#' @param config_file user json config 
#'
#' @return
#' @export
#'
#' @examples
get_fiscal_year_start = function(config_file) { config_file[["Fiscal_Year_Start"]] }

#' translate account json into df
#'
#' @param account_json json returned from mintapi for accounts
#'
#' @return
#' @export
#'
#' @examples
get_account_df = function(accounts_json)
{
  accounts_json %>% tidyjson::spread_all() %>% as_tibble() %>% 
    
    select(accountName, accountType, accountSystemStatus, value, interestRate) %>% 
    
    dplyr::filter(accountSystemStatus == "ACTIVE") %>% select(-accountSystemStatus) %>% 
    
    mutate(interestRate = if_else(is.na(interestRate), 0, interestRate))
  
  
}