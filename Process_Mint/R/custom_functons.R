#' Extract manual account adjustments from config
#'
#' @param config_file user json config
#'
#' @return tibble of adjustments
#' @export
#'
#' @examples
get_manual_adjustments = function(config_file)
{
  lapply(config_file[['Manual_Adjustments']], function(adj) {
    
    #TODO Type, Var should be enum
    tibble(
      'TimeAdj' = create_datetime(adj[['Year']], adj[['Month']]), 
      'Var' = adj[['Var']], 
      'Amt' = adj[['Amount']], 
      'Type' = adj[['Type']])
    
  }) %>% bind_rows()
}

get_account_balances = function(account_df, config_file, forecast_time_series)
{
  total_df = account_df %>% group_by(accountType) %>% summarise(Sum = sum(value))
  
  #TODO figure out how to handle multiple loans
  tibble('Timestamp' = forecast_time_series %>% dplyr::first(),
         'Total_Savings' = total_df %>% dplyr::filter(accountType == 'bank') %>% pull(Sum),
         'Investments' = total_df %>% dplyr::filter(accountType == 'investment') %>% pull(Sum),
         'Public_Loans' = total_df %>% dplyr::filter(accountType == 'loan') %>% pull(Sum),
         'BaseSalary' = Configuration::get_numeric_val_from_config(config_file, 'Base_Salary'))
}

get_fixed_payments = function(account_df, transactions, config_file, forecast_time_series, growth_rate = 0)
{
  #TODO same as above, handle multiple loans
  interest_rate = account_df %>% dplyr::filter(accountType == 'loan' & interestRate > 0) %>% pull(interestRate)
  loan_payment = transactions %>% dplyr::filter(str_detect(category, 'loan')) %>% data.table::first() %>% pull(Amount)

  contribution_401k = Configuration::get_numeric_val_from_config(config_file, 'Annual_401k_Contribution')
  
  tibble('Timestamp' = forecast_time_series %>% first(),
         'Public_Loan_Payment' = loan_payment,
         'Public_Loan_Interest_Rate' = interest_rate,
         
         #Connect to fidelity api for this?
         'Annual_401k_Contribution' = contribution_401k,
         'Investment_Return_Rate' = growth_rate)
}

#TODO not sure what I want to do with this yet
create_structure_projections = function(category_df, transactions, years, zero_growth, config_file)
{
  base_salary = Configuration::get_numeric_val_from_config(config_file, 'Base_Salary')
  
  #This says month but config has year?
  #Fiscal_Year_Start
  fiscal_month_start = get_fiscal_year_start(config_file)
  
  #avg raise = 2 config values - rename to avg_raise_percentage
  #config_file[['Average_Raise']] * config_file[['Base_Salary']]
  raise = if_else(!zeroGrowth, get_average_raise(config_file), 0)

  #TODO do this without lapply, just make datetime from years
  lapply(years, function(year, min_year, raise, base_salary, transactions, category_df) {
    
    bonus = categoryList %>% getBonus_Taxes(transactions, (baseSalary))
    raise = if_else(year > min_year, raise, 0)

    tibble('TimeAdj' = c(create_dateTime(year, fiscal_month_start), create_dateTime(year, fiscal_month_start + 1)),
           'Var' = c('BaseSalary', 'Total_Savings'), 
           'Amt' = c(raise, bonus), 
           'Type' = c('Credit', 'Credit')) %>% dplyr::filter(Amt != 0)
    
  },
  min_year = min(years),
  raise = raise,
  baseSalary = baseSalary, 
  transactions = transactions, 
  categoryList = categoryList) %>% 
  
  bind_rows()
}

#Need config_file passed into here for salary function
get_bonus_taxes = function(category_df, transactions, base_salary)
{
  tax_types = c('Federal Tax', 'State Tax')
  tax_refund = category_df %>% dplyr::filter(Category %in% tax_types) %>% pull(meanAnnualAcct) %>% sum()
  
  #TODO make bonus percentage an input
  historical_bonus = transactions %>% dplyr::filter(Category == 'Bonus') %>% pull(Amount)
  
  future_bonus = (historical_bonus / Configuration::get_numeric_val_from_config()) * base_salary
  
  return (taxRefund + bonus)
}