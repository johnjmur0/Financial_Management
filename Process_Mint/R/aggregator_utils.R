#' Year-Month total expenses, income, profit
#'
#' @param transactions dataframe of transactions 
#'
#' @return dataframe
#' @export
#'
#' @examples get_monthly_summary(transactions)
#' 
#' #TODO Hopefully delete this function
get_monthly_summary = function(transactions) {
  
  Utilities::df_col_has_value(transactions, "date", "Date")
  Utilities::df_col_has_value(transactions, "amount", "numeric")
  
  transactions %>% 
    arrange(date) %>%
    mutate(Amount = if_else(transaction_type == "debit", amount * -1, amount),
           Year = lubridate::year(date), 
           Month = lubridate::month(date),
           
           Income = if_else(Amount > 0, Amount, 0),
           Expense = if_else(Amount < 0, Amount, 0)) %>% 
    
    select(-amount) %>% 
    rename(Date = date) %>%
    group_by(Year, Month) %>% 
    
    mutate(Expense_Total_Monthly = sum(Expense),
           Income_Total_Monthly = sum(Income),
           Profit_Total_Monthly = Income_Total_Monthly + Expense_Total_Monthly) %>% 
    ungroup()
}

#TODO Hopefully delete this function
get_avg_spend_monthly = function(category_df, 
                                 transactions, 
                                 return_df = FALSE, 
                                 remove_income = TRUE, 
                                 remove_loans = TRUE) {
  
  #TODO handle these categories in config
  one_time_categories = c("air travel", "hide from budgets & trends", "credit card payment")
  
  if (remove_income) {
    category_df = category_df %>% filter(Type == "Debit")
  } 
  
  if (remove_loans) {
    #TODO handle buy/investments better
    category_df = category_df %>% filter(!(category %in% c("student loan", "education", 'buy')))
  }

  spend_monthly_df = category_df %>% 
      
    filter(!(category %in% one_time_categories)) %>%
    
    group_by(Year, Month, category) %>% 
    
    summarise(Total_Spend = sum(Total)) %>% 
    
    ungroup()
  
  spend_df = spend_monthly_df %>% 
  
    spread(key = category, value = Total_Spend) %>% 
    
    mutate_if(is.double, funs(if_else(is.na(.), 0, .))) %>% 
    
    gather(key = category, value = Total_Spend, -Year, -Month)
  
  if (return_df) {
    return(spend_df)
  }
    
  spend_df %>% 
  
    group_by(Year, Month) %>% 
    
    summarise(Mean_Spend = sum(Total_Spend)) %>%
    
    ungroup() %>% 
    
    summarise(Spend = mean(Mean_Spend)) %>% 
    
    pull()
}

#TODO Hopefully delete this function
get_avg_income = function(category_df, start_date = NULL) {
  
  start_date = if_else(is.null(start_date), 
                       lubridate::ymd(str_c(min(category_df[["Year"]]), min(category_df[["Month"]]), 1, sep = "/")),
                       start_date)
  
  category_df %>% 
  
    filter(Type == "Credit") %>%
    
    mutate(Year_Month = lubridate::ymd(str_c(Year, Month, 1, sep = "/"))) %>% 
    
    #TODO handle this in config better
    #Extra income, don't count in average
    filter(!str_detect(category, "Tax") | !(category == "Income" & Monthly_Total > 1000)) %>% 
    
    #Current month almost always incomplete, can distort average
    filter(between(Year_Month, start_date, Sys.time())) %>% 
    
    group_by(Year, Month) %>%
    
    summarise(meanIncome = sum(Monthly_Total)) %>% 
    ungroup() %>% 
    summarise(meanIncome = mean(meanIncome)) %>% 
    pull()
}

# '
#' @param transactions df of mint transactions 
#' @param start_date date to keep transactions after
#' @param include_outlier whether to remove outliers as defined in user config
#'
#' @return
#' @export
#'
#' @examples
summarise_categories = function(transactions, config_file, start_date, agg_vec, include_outlier = FALSE) {
  
  lapply(agg_vec, function(val) {
    Utilities::df_col_has_value(transactions, val, "numeric") 
  })

  Utilities::df_col_has_value(transactions, "category", "character")
  Utilities::df_col_has_value(transactions, "transaction_type", "character")
    
  #TODO handle outlier categories, plus category + amount combo
  if (!include_outlier) {
    
      transactions = lapply(config_file[["Outlier_Months"]], function(outlier, transactions) {
      
        exclude_month = lubridate::ymd(str_c(outlier[["Year"]], outlier[["Month"]], 1, sep = "-"))
        transactions %>% 
        mutate(Year_Month = lubridate::ymd(str_c(Year, Month, 1, sep = "-"))) %>%
        filter(Year_Month != exclude_month)
      
      },
      transactions = transactions) %>% 
      bind_rows() %>% 
      distinct()
  }

  #TODO handle these categories in config
  one_time_categories = c("air travel", "hide from budgets & trends", "credit card payment")
  
  categories = transactions %>% 
  
    filter(date >= start_date & !(category %in% one_time_categories)) %>%
    
    group_by(!!!syms(agg_vec), category, transaction_type) %>% 
    
    summarise(total = sum(amount)) %>%
    
    filter(total != 0) %>% 
    
    ungroup()
}