#' gets root directory for transaction files
#'
#' @param overview_df general financial data, contains directory 
#'
#' @return character for directory path
#' @export
#'
#' @examples
get_transactions_dir = function(overview_df) 
{
  Configuration::inspect_overview_df(overview_df)
  
  df_col_has_value(overview_df, "Directory", "character")
  
  return (overview_df[["Directory"]])
}

#' Parse out date in file name, get most recent
#'
#' @param all_transaction_files all available transaction files 
#'
#' @return formatted most recent date string
#' @export
#'
#' @examples
get_most_recent_file_date = function(all_transaction_files)
{
  if (FALSE %in% str_detect(all_transaction_files, "transactions")) {
    stop(str_c("Transaction files not named properly, need 'transaction_date'", all_transaction_files, sep = " "))
  }
  
  tryCatch(
    {
      latest_date = all_transaction_files %>% as_tibble() %>% 
        separate(value, sep = "transactions_", into = c("EmptyString", "FileDate")) %>% 
        
        mutate(FileDate = lubridate::parse_date_time(FileDate %>% str_remove(".csv"), 
                                                     orders="%m-%d-%Y")) %>% 
        arrange(desc(FileDate)) %>% 
        data.table::first() %>% pull(FileDate) %>% as.character(format = "%m-%d-%Y")    
      
      return (latest_date)
    },
    error=function(cond) {
      stop(str_c("Date parsing failed on files", all_transaction_files, cond, sep = " "))
    }
  )
}

#' read_most_recent_transaction_file
#'
#' @param overview_df general financial data 
#'
#' @return most recent set of transactions
#' @export
#'
#' @examples
read_most_recent_transaction_file = function(overview_df)
{
  all_transaction_files = list.files(get_transactions_dir(overview_df), full.names = TRUE)
  
  if (length(all_transaction_files) == 0) {
    stop(str_c("No files in provided directory", get_transactions_dir(overview_df), "check the configuration", sep=" "))
  }

  most_recent_file_date = get_most_recent_file_date(all_transaction_files)
  
  all_transaction_files %>% grep(str_c("transactions_", most_recent_file_date), ., value = TRUE) %>% 
    read_csv() %>% 
    mutate(Date = lubirdate::parse_date_time(Date, orders = "%m/%d/%Y"),
           Amount = if_else(`Transaction Type` == "debit", Amount * -1, Amount))
}

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
           Year_Month = ((Year) * 12) + Month,
           
           Income = if_else(Amount > 0, Amount, 0),
           Expense = if_else(Amount < 0, Amount, 0)) %>% 
    
    select(-amount) %>% rename(Date = date) %>%
    
    group_by(Year_Month) %>% mutate(Expense_Total_Monthly = sum(Expense),
                                    Income_Total_Monthly = sum(Income),
                                    Profit_Total_Monthly = Income_Total_Monthly + Expense_Total_Monthly)
}

#' get monthly sum of spending by category
#'
#' @param transactions todo 
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
    mutate(Year = lubridate::year(Date),
           Month = lubridate::month(Date),
           Year_Month = lubridate::ymd(str_c(Year, Month, 1, sep = "-")))
  
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
    group_by(Year, Month, category, Type) %>% summarise(meanAnnualAcct = sum(Amount))
  
  debits = categories %>% filter(Type == "Debit") %>% 
    group_by(Year, Month, category, Type) %>% summarise(meanAnnualAcct = sum(Amount))
  
  categoryList = bind_rows(credits, debits) %>% 
    na.omit() %>% filter(meanAnnualAcct != 0)
}


get_avg_spend_monthly = function(category_df, transactions, return_df = FALSE, remove_income = TRUE, remove_loans = TRUE)
{
  #TODO handle these categories in config
  #hide includes lump student loan payments, mattress, air/credit card are vacations
  nonRepeatableCategories = c("Air Travel", "Hide from Budgets & Trends", "Credit Card Payment")
  #Music purchases, nonrepeatable only greater than 1k
  #largeOneTimeCategories = c("Entertainment", "Hobbies")
  
  if (remove_income) {
    category_df = category_df %>% dplyr::filter(Type == "Debit")
  } 
  
  if (remove_loans) {
    category_df = category_df %>% dplyr::filter(!(category %in% c("Student Loan", "Education")))
  }

  spend_monthly = category_df %>% 
      
    dplyr::filter(!(category %in% nonRepeatableCategories)) %>%
    
    group_by(Year, Month, category) %>% summarise(totalSpend = sum(meanAnnualAcct)) %>% ungroup()
  
  spend_df = spend_monthly %>% spread(key=category, value=totalSpend) %>% 
    
    mutate_if(is.double, funs(if_else(is.na(.), 0, .))) %>% gather(key=category, value=totalSpend, -Year, -Month)
  
  if(return_df) {
    return (spendDf)
  }
    
  #spendDf %>% write_csv(file.path(dirname(dir), "SpendingData", str_c("Year_Month_Category_Spend_", mostRecentFileDate, ".csv")))
    
  spend_df %>% group_by(Year, Month) %>% summarise(meanSpend = sum(totalSpend)) %>%
    
    ungroup() %>% summarise(spend = mean(meanSpend)) %>% pull()
}

get_net_income = function(category_df, start_date = NULL)
{
  start_date = if_else(is.null(start_date), 
                       lubridate::ymd(str_c(min(category_df[["Year"]]), min(category_df[["Month"]]), 1, sep="/")),
                       start_date)
  
  category_df %>% filter(Type == "Credit") %>%
    
    dplyr::filter(!str_detect(category, "Tax") | !(category == "Income" & meanAnnualAcct > 1000)) %>% 
    
    dplyr::filter(Year >= lubridate::year(start_date) & Month >= lubridate::month(start_date) & 
                    #current month almost always incomplete
                    !(Year == lubridate::year(Sys.time()) & Month == lubridate::month(Sys.time()))) %>% 
    
    group_by(Year, Month) %>% summarise(meanIncome = sum(meanAnnualAcct)) %>% ungroup() %>% 
    
    summarise(meanIncome = mean(meanIncome)) %>% pull()
}