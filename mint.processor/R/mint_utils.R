aggregate_categories = function(spend_df, category_list, col_name, time_vec)  {

  lapply(seq_along(category_list), function(i, spend_df, col_name, time_vec) {

    category_name = names(category_list)[i]
    category_definition = category_list[i][[1]] %>% tolower()

    spend_df %>% 
      filter(tolower(category) %in% category_definition) %>% 
      group_by(!!!syms(time_vec)) %>% 
      summarise(total := sum(!!sym(col_name))) %>% 
      mutate(category = category_name)
  },
  spend_df = spend_df,
  col_name = col_name,
  time_vec = time_vec) %>% 
  bind_rows()
}

#' clean raw transactions df
#'
#' @param transactions_df raw mint transactions df
#'
#' @export
#'
clean_transactions_df = function(transactions_df) {

  transactions_df %>% 

    tidyr::unnest_wider(category) %>%

    mutate(date = lubridate::with_tz(date, 'UTC')) %>%
  
    filter(date <= Sys.time()) %>%
    
    mutate(amount = unlist(amount),
           year = lubridate::year(date),
           month = lubridate::month(date),
           day = as.numeric(lubridate::day(date))) %>%

    rename(category = name)
}

#' clean raw accounts df
#'
#' @param accounts_df raw mint accounts df
#'
#' @export
#'
clean_accounts_df = function(accounts_df) {

  accounts_df %>% 
    
    filter(systemStatus == 'ACTIVE') %>% 
    
    mutate(currentBalance = unlist(currentBalance)) %>% 
    
    group_by(type) %>% 
    
    summarise(total = sum(currentBalance)) %>%
    
    rename(account_type = type) %>% 
    
    select(account_type, total) %>%

    mutate(account_type = case_when(account_type == 'BankAccount' ~ 'bank',
                                    account_type == 'InvestmentAccount' ~ 'investment',
                                    TRUE ~ account_type))
}

summarise_categories = function(transactions_df, config_list, start_date, agg_vec, include_outlier = FALSE) {
  
  lapply(agg_vec, function(val) {
    utilities::df_col_has_value(transactions_df, val, "numeric") 
  })

  utilities::df_col_has_value(transactions_df, "category", "character")
  utilities::df_col_has_value(transactions_df, "amount", "numeric")
    
  #TODO handle outlier categories, plus category + amount combo
  if (!include_outlier) {
    
      transactions_df = lapply(config_list[["outlier_months"]], function(outlier, transactions_df) {
      
        exclude_month = lubridate::ymd(str_c(outlier[["year"]], outlier[["month"]], 1, sep = "-"))
        transactions_df %>% 
        mutate(Year_Month = lubridate::ymd(str_c(Year, Month, 1, sep = "-"))) %>%
        filter(Year_Month != exclude_month)
      
      },
      transactions_df = transactions_df) %>% 
      bind_rows() %>% 
      distinct()
  }

  ignore_categories = config_list[['transactions_params']][['ignore_categories']]
  
  transactions_df %>% 
  
    filter(date >= start_date & !(category %in% ignore_categories)) %>%
    
    group_by(!!!syms(agg_vec), category) %>% 
    
    summarise(total = sum(amount)) %>%
    
    filter(total != 0) %>% 
    
    ungroup()
}