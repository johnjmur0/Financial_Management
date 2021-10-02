getStartingSalary = function() { 140000 }
getMinimumSavings_Months = function(months = 8) { months }
getAvgRaise = function() { 4200 }

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

createStructureProjections = function(categoryList, transactions, years, zeroGrowth)
{
  baseSalary = getStartingSalary()
  
  raise = if_else(!zeroGrowth, getAvgRaise(), 0)

  lapply(years, function(year, minYear, raise, baseSalary, transactions, categoryList) {
    
    bonus = categoryList %>% getBonus_Taxes(transactions, (baseSalary))
    raise = if_else(year > minYear, raise, 0)

    list(tibble("TimeAdj" = createDateTime(year, 2), "Var" = "BaseSalary", "Amt" = raise, "Type"="Credit"),
         tibble("TimeAdj" = createDateTime(year, 3), "Var" = "Total_Savings", "Amt" = bonus, "Type" = "Credit")) %>% 
      
      bind_rows() %>% dplyr::filter(Amt != 0)
    
  },minYear=min(years),
  raise=raise,
  baseSalary=baseSalary, 
  transactions=transactions, 
  categoryList=categoryList) %>% bind_rows()
}

getCurrentBalances = function(forecastTime)
{
  baseSalary = getStartingSalary()
  #idk how to download from Mint - easier to copy
  currentBalances = tibble("Timestamp" = forecastTime %>% first(),
                           "Total_Savings" = (45783.44 + 13835.92 + 60),
                           "Investments" = (49861 + 8016.5 + 1324.59),
                           "Public_Loans" = -20232.52,
                           "Private_Loans" = 0,
                           "BaseSalary" = baseSalary)
}

getStrucutralData = function(forecastTime, growthRate = 0)
{
  loanStructure = tibble("Timestamp" = forecastTime,
                         "Public_Loan_Payment" = 380.81,
                         "Public_Loan_Interest_Rate" = .054,
                         "Private_Loan_Payment" = 0,
                         "Private_Loan_Interest_Rate" = 0,
                         
                         "Investment_Contribution_IRA" = (.08 + .04),
                         "Investment_Contribution_Roth" = .059299,
                         "Investment_Return_Rate" = growthRate)
}

getTimeAdjustments = function(categoryList, transactions, years, zeroGrowth = FALSE)
{
  additionalAdjustments = createStructureProjections(categoryList, transactions, years, zeroGrowth)
  
  list(tibble("TimeAdj" = createDateTime(2021, 12), "Var"="Public_Loans", "Amt"= Inf, "Type"="Debit")) %>% 
    
    bind_rows(additionalAdjustments)
}