#TODO WIP to have data as json and read via prompt. Not sure I'll go thru with it. 
#Depends on if I can get the python api to work
# get_config = function(configName)
# {
#   filePath = str_c("../Config_Files/", configName, ".json")
#   
#   if (!file.exists(filePath))
#   {
#     print ("Provided config file does not exists, prompting you to populate one...")
#     populate_configFile(configName)
#   }
#   configFile = rjson::fromJSON(configName)
#   
#   #TODO Config validation
#   return (configFile)
# }
# 
# populate_configFile = function(configName)
# {
#   config_template = rjson::fromJSON(file = "./Config_Files/Config_Template.json")
#   
#   params = config_template %>% length()
#   form = list()
#   
#   foo = function(level) { 
#     
#     lapply(level, function(x) { 
#       
#       if(is.list(x)) {  
#         print (str_c("Processing", names(x), sep = " "))
#         svDialogs::dlgInput(str_c("Enter", names(x), sep = " "), Sys.info()["user"])$res
#         return (foo(x)) 
#       } 
#       else { 
#         return (x) 
#       } 
#     })
#   }
#   
#   foo(config_template)
# }

#' #TODO Memoize this
#' read_table = function(userName, tableName)
#' {
#'   user_dir = file.path("./Config_Files", str_c("Config", userName, sep = "_"))
#'   overviewDf = list.files(user_dir, full.names = TRUE, pattern = tableName) %>% read_csv()
#' }
#' 
#' #' Ensure overview_df corresponds to expected structure
#' #'
#' #' @param overview_df general overview of financial data 
#' #'
#' #' @return None
#' #' @export
#' #'
#' #' @examples
#' inspect_overview_df = function(overview_df)
#' {
#'   assertthat::assert_that(is.data.frame(overview_df))
#'   assertthat::assert_that(length(overview_df) == 8)
#'   assertthat::assert_that(nrow(overview_df) == 1)
#'   
#'   expected_columns = c(
#'     "Directory","CurrentSalary","BonusMultiplier","MinSavings_Months",
#'     "ExpectedRaise_Annual","RaiseOccurs_Month","BonusOccurs_Month","Savings_Total")
#'   
#'   assertthat::assert_that(colnames(overview_df) == expected_columns)
#' }

#Note sure how to handle user name at this point
get_user_config = function(user_name)
{
  config_filePath = file.path("./Config_Files", str_c("Config_", user_name, ".json"))
  config_file = rjson::fromJSON(file = config_filePath)
}

get_base_salary = function(config_file) { config_file[["Base_Salary"]] }

get_min_savings_month = function(config_file) { config_file[["Minimum_Monthly_Savings"]] }

get_401k_contribution_annual = function(config_file) { config_file[["Annual_401k_Contribution"]] }

#TODO can also base this off historical
get_investment_growth = function(config_file) { config_file[["Average_Investment_Growth"]] }

get_average_raise = function(config_file) 
{ 
  nominal_raise = config_file[["Average_Raise"]] * config_file[["Base_Salary"]]
}

get_fiscal_year_start = function(config_file) { config_file[["Fiscal_Year_Start"]] }

#TODO transalte account json into df