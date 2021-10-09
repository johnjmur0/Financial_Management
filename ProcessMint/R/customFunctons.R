getInvestments = function(categoryList, investment_Contribution_Roth, investment_Contribution_IRA)
{
  netIncome = getNetIncome(categoryList)
  taxRatio = netIncome / (getStartingSalary() / 12)
  grossIncome = netIncome * (1 + (1 - taxRatio))
  
  investmentTotal = (investment_Contribution_Roth * netIncome) + 
    (investment_Contribution_IRA * grossIncome)
}

getProjectedIncome = function(netIncome, baseSalary)
{
  taxRatio = (netIncome / (getStartingSalary() / 12))
  return ((baseSalary / 12) * taxRatio)
}

getBonus_Taxes = function(categoryList, transactions, baseSalary)
{
  taxRefund = categoryList %>% dplyr::filter(Category == "Federal Tax" | Category == "State Tax") %>% 
    pull(meanAnnualAcct) %>% sum()
  
  #TODO make bonus percentage an input
  bonus = ((transactions %>% dplyr::filter(Category == "Bonus") %>% 
              pull(Amount) / getStartingSalary()) * baseSalary)
  
  return (taxRefund + bonus)
}

createStructureProjections = function(category_df, transactions, years, zeroGrowth, config_file)
{
  base_salary = get_base_salary(config_file)
  fiscal_month_start = get_fiscal_year_start(config_file)
  
  raise = if_else(!zeroGrowth, get_average_raise(config_file), 0)

  lapply(years, function(year, min_year, raise, base_salary, transactions, category_df) {
    
    bonus = categoryList %>% getBonus_Taxes(transactions, (baseSalary))
    raise = if_else(year > min_year, raise, 0)

    list(tibble("TimeAdj" = createDateTime(year, fiscal_month_start), "Var" = "BaseSalary", "Amt" = raise, "Type"="Credit"),
         tibble("TimeAdj" = createDateTime(year, fiscal_month_start + 1), "Var" = "Total_Savings", "Amt" = bonus, "Type" = "Credit")) %>% 
      
      bind_rows() %>% dplyr::filter(Amt != 0)
    
  },min_year=min(years),
  raise=raise,
  baseSalary=baseSalary, 
  transactions=transactions, 
  categoryList=categoryList) %>% bind_rows()
}

get_account_balances = function(account_df, config_file, forecast_date_range)
{
  total_df = account_df %>% group_by(Account_Class) %>% summarise(Sum = sum(Account_Balance))
  
  #TODO figure out how to handle multiple loans
  currentBalances = tibble("Timestamp" = forecast_date_range %>% first(),
                           "Total_Savings" = total_df %>% dplyr::filter(Account_Class == "bank") %>% pull(Sum),
                           "Investments" = total_df %>% dplyr::filter(Account_Class == "investment") %>% pull(Sum),
                           "Public_Loans" = total_df %>% dplyr::filter(Account_Class == "loans") %>% pull(Sum),
                           "BaseSalary" = get_base_salary(config_file))
}

get_account_changes = function(account_df, transactions, config_file, forecast_date_range, growth_rate = 0)
{
  #TODO same as above, handle multiple loans
  interest_rate = account_df %>% dplyr::filter(Account_Class == "loan" & Interest_Rate > 0) %>% pull(Interest_Rate)
  loan_payment = transactions %>% dplyr::filter(str_detect(category, "loan")) %>% data.table::first() %>% pull(Amount)
  
  loanStructure = tibble("Timestamp" = forecast_date_range %>% first(),
                         "Public_Loan_Payment" = loan_payment,
                         "Public_Loan_Interest_Rate" = interest_rate,
                         
                         #Connect to fidelity api for this?
                         "401k_Total_Investment" = get_401k_contribution_annual(config_file),
                         "Investment_Return_Rate" = growth_rate)
}


get_manual_adjustments = function(category_df, transactions, config_file, years, zeroGrowth = FALSE)
{
  #additionalAdjustments = createStructureProjections(categoryList, transactions, years, zeroGrowth)
  
  manual_adj = lapply(config_file[["Manual_Adjustments"]], function(adj) {
    
    tibble(
      "TimeAdj" = create_datetime(adj[['Year']], adj[['Month']]), 
      "Account_Id"=adj[['Account_Id']], 
      "Amt"= adj[["Amount"]], 
      "Type"=adj[["Type"]])
    
  }) %>% bind_rows()
    
    #manual_adj %>% bind_rows(additionalAdjustments)
}