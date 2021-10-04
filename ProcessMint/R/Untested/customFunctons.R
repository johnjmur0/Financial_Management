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

createStructureProjections = function(categoryList, transactions, years, zeroGrowth, config_file)
{
  baseSalary = getStartingSalary()
  fiscal_month_start = get_fiscal_year_start(config_file)
  
  raise = if_else(!zeroGrowth, getAvgRaise(), 0)

  lapply(years, function(year, minYear, raise, baseSalary, transactions, categoryList) {
    
    bonus = categoryList %>% getBonus_Taxes(transactions, (baseSalary))
    raise = if_else(year > minYear, raise, 0)

    list(tibble("TimeAdj" = createDateTime(year, fiscal_month_start), "Var" = "BaseSalary", "Amt" = raise, "Type"="Credit"),
         tibble("TimeAdj" = createDateTime(year, fiscal_month_start + 1), "Var" = "Total_Savings", "Amt" = bonus, "Type" = "Credit")) %>% 
      
      bind_rows() %>% dplyr::filter(Amt != 0)
    
  },minYear=min(years),
  raise=raise,
  baseSalary=baseSalary, 
  transactions=transactions, 
  categoryList=categoryList) %>% bind_rows()
}

getCurrentBalances = function(account_df, config_file, forecast_time)
{
  total_df = account_df %>% group_by(Account_Class) %>% summarise(Sum = sum(Account_Balance))
  
  #TODO figure out how to handle multiple loans
  currentBalances = tibble("Timestamp" = forecast_time %>% first(),
                           "Total_Savings" = total_df %>% dplyr::filter(Account_Class == "bank") %>% pull(Sum),
                           "Investments" = total_df %>% dplyr::filter(Account_Class == "investment") %>% pull(Sum),
                           "Public_Loans" = total_df %>% dplyr::filter(Account_Class == "loans") %>% pull(Sum),
                           "BaseSalary" = get_base_salary(config_file))
}

getStrucutralData = function(account_df, transactions, config_file, forecast_time, growth_rate = 0)
{
  #TODO same as above, handle multiple loans
  interest_rate = account_df %>% dplyr::filter(Account_Class == "loan" & Interest_Rate > 0) %>% pull(Interest_Rate)
  loan_payment = transactions %>% dplyr::filter(str_detect(category, "loan")) %>% data.table::first() %>% pull(Amount)
  
  loanStructure = tibble("Timestamp" = forecast_time,
                         "Public_Loan_Payment" = loan_payment,
                         "Public_Loan_Interest_Rate" = interest_rate,
                         
                         #Connect to fidelity api for this?
                         "401k_Total_Investment" = get_401k_contribution_annual(config_file)
                         "Investment_Return_Rate" = growth_rate)
}

#Replace with config file
getTimeAdjustments = function(categoryList, transactions, years, zeroGrowth = FALSE)
{
  additionalAdjustments = createStructureProjections(categoryList, transactions, years, zeroGrowth)
  
  list(tibble("TimeAdj" = createDateTime(2021, 12), "Var"="Public_Loans", "Amt"= Inf, "Type"="Debit")) %>% 
    
    bind_rows(additionalAdjustments)
}