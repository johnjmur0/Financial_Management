#' Year-Month total expenses, income, profit
#'
#' @param transactions dataframe of transactions 
#'
#' @return dataframe
#' @export
#'
#' @examples
get_monthly_summary = function(transactions)
{
  df_col_has_value(transactions, "date", "Date")
  df_col_has_value(transactions, "amount", "numeric")
  
  transactions %>% arrange(date) %>% 
  
    mutate(Amount = if_else(transaction_type == "debit", amount * -1, amount),
           Year = lubridate::year(date), 
           Month = lubridate::month(date),
           
           Income = if_else(Amount > 0, Amount, 0),
           Expense = if_else(Amount < 0, Amount, 0)) %>% 
    
    select(-amount) %>% rename(Date = date) %>%
    
    group_by(Year, Month) %>% mutate(Expense_Total_Monthly = sum(Expense),
                                     Income_Total_Monthly = sum(Income),
                                     Profit_Total_Monthly = Income_Total_Monthly + Expense_Total_Monthly) %>% 
    ungroup()
}

#' get monthly sum of spending by category
#'
#' @param transactions df of mint transactions 
#' @param start_date date to keep transactions 
#' @param include_outlier whether to remove outliers as defined in user config
#'
#' @return
#' @export
#'
#' @examples
monthly_category_sum = function(transactions, config_file, start_date, include_outlier = FALSE)
{
  transactions = transactions %>% 
    mutate(Year_Month = lubridate::mdy(str_c(Month, 1, Year, sep = "-")))
  
  if(!include_outlier) {
    
      transactions = lapply(config_file[["Outlier_Months"]], function(outlier, transactions) {
      
      exclude_month = lubridate::ymd(str_c(outlier[["Year"]], outlier[["Month"]], 1, sep = "-"))
      transactions %>% dplyr::filter(Year_Month != exclude_month)
      
      },transactions=transactions) %>% bind_rows() %>% distinct()
      
      #TODO handle outlier categories, plus category + amount combo
  }
  
  #Could I do this without saving data?
  categories = transactions %>% filter(Date > start_date) %>%
    mutate(Type = if_else(Amount < 0, "Debit", "Credit"))
  
  credits = categories %>% filter(Type == "Credit") %>% 
    group_by(Year, Month, category, Type) %>% summarise(Monthly_Total = sum(Amount))
  
  debits = categories %>% filter(Type == "Debit") %>% 
    group_by(Year, Month, category, Type) %>% summarise(Monthly_Total = sum(Amount))
  
  categoryList = bind_rows(credits, debits) %>% 
    na.omit() %>% filter(Monthly_Total != 0) %>% ungroup()
}


get_avg_spend_monthly = function(category_df, transactions, return_df = FALSE, remove_income = TRUE, remove_loans = TRUE)
{
  #TODO handle these categories in config
  #hide includes lump student loan payments, mattress, air/credit card are vacations
  one_time_categories = c("air travel", "hide from budgets & trends", "credit card payment")
  #Music purchases, nonrepeatable only greater than 1k
  #largeOneTimeCategories = c("Entertainment", "Hobbies")
  
  if (remove_income) {
    category_df = category_df %>% dplyr::filter(Type == "Debit")
  } 
  
  if (remove_loans) {
    #TODO handle buy/investments better
    category_df = category_df %>% dplyr::filter(!(category %in% c("student loan", "education", 'buy')))
  }

  spend_monthly = category_df %>% 
      
    dplyr::filter(!(category %in% one_time_categories) & Year <= lubridate::year(Sys.time())) %>%
    
    group_by(Year, Month, category) %>% summarise(Total_Spend = sum(Monthly_Total)) %>% ungroup()
  
  spend_df = spend_monthly %>% spread(key=category, value=Total_Spend) %>% 
    
    mutate_if(is.double, funs(if_else(is.na(.), 0, .))) %>% gather(key=category, value=Total_Spend, -Year, -Month)
  
  if(return_df) {
    return (spendDf)
  }
    
  spend_df %>% group_by(Year, Month) %>% summarise(Mean_Spend = sum(Total_Spend)) %>%
    
    ungroup() %>% summarise(Spend = mean(Mean_Spend)) %>% pull()
}

get_avg_income = function(category_df, start_date = NULL)
{
  start_date = if_else(is.null(start_date), 
                       lubridate::ymd(str_c(min(category_df[["Year"]]), min(category_df[["Month"]]), 1, sep="/")),
                       start_date)
  
  category_df %>% filter(Type == "Credit") %>%
    
    #Extra income, don't count in average
    dplyr::filter(!str_detect(category, "Tax") | !(category == "Income" & Monthly_Total > 1000)) %>% 
    
    dplyr::filter(Year >= lubridate::year(start_date) & Month >= lubridate::month(start_date) & 
                    Year <= lubridate::year(Sys.time()) &
                    #current month almost always incomplete
                    !(Year == lubridate::year(Sys.time()) & Month == lubridate::month(Sys.time()))) %>% 
    
    group_by(Year, Month) %>% summarise(meanIncome = sum(Monthly_Total)) %>% ungroup() %>% 
    
    summarise(meanIncome = mean(meanIncome)) %>% pull()
}