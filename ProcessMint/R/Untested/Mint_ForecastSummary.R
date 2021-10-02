createFinalDf = function(categoryList, transactions, currentBalances, loanStructure, forecastTime)
{
  monthlyAvgSpend = getMonthlyAvgSpend(categoryList)
  netIncome = getNetIncome(categoryList) %>% getProjectedIncome(currentBalances[["BaseSalary"]])
  
  finalDf = tibble("Timestamp" = forecastTime, 
                   "NetIncome" = netIncome, 
                   "AvgSpend" = monthlyAvgSpend, 
                   "NetWorth" = 0) %>% 
    
    left_join(currentBalances, by=c("Timestamp")) %>% left_join(loanStructure, by=c("Timestamp")) %>% 
    
    select(-Timestamp) %>% mutate_all(funs(if_else((is.na(.)), 0, .))) %>% 
    
    bind_cols(data.frame("Timestamp" = forecastTime)) %>% as_tibble() %>% 
    
    select(Timestamp, NetIncome, AvgSpend, NetWorth, Total_Savings, Investments, BaseSalary,            
           Public_Loans, Private_Loans, Public_Loan_Payment, Public_Loan_Interest_Rate,   
           Private_Loan_Payment, Private_Loan_Interest_Rate, Investment_Contribution_Roth, 
           Investment_Contribution_IRA, Investment_Return_Rate)
}

adjustTimeVariable = function(i, timeAdjustment, finalDf)
{
  variable = timeAdjustment[["Var"]]
  
  i = ifelse((variable == "BaseSalary" | variable == "Investment_Contribution"), i + 1, i)
  
  timeAdjustment[["Amt"]] = if_else(timeAdjustment[["Amt"]] == Inf, 
                                    finalDf[[variable]][[i - 1]] * -1,
                                    timeAdjustment[["Amt"]])
  
  finalDf[[variable]][[i - 1]] = timeAdjustment[["Amt"]] + finalDf[[variable]][[i - 1]]
  
  if (timeAdjustment[["Type"]] == "Debit") {
    finalDf[["Total_Savings"]][i - 1] = finalDf[["Total_Savings"]][i -1] - timeAdjustment[["Amt"]]
  }
  
  return (finalDf)
}

predictDf_Forward = function(finalDf, timeAdjustmentDf, categoryList, monthsOfSavings)
{
  for (i in 2:nrow(finalDf)) {
    
    print(i)
    
    finalDf[["BaseSalary"]][i] = finalDf[["BaseSalary"]][i - 1]
    finalDf[["Investment_Contribution"]][i] = finalDf[["Investment_Contribution"]][i - 1]
    
    timeAdjustment = timeAdjustmentDf %>% dplyr::filter(TimeAdj == finalDf[["Timestamp"]][i])
    
    if (nrow(timeAdjustment) > 0) {
      
      finalDf = i %>% adjustTimeVariable(timeAdjustment, finalDf)
    }
    
    publicLoans = finalDf[["Public_Loans"]]
    publicLoans_Interest_Rate = 1 + finalDf[["Public_Loan_Interest_Rate"]] / 12
    publicLoans_Payment = finalDf[["Public_Loan_Payment"]]
    
    privateLoans = finalDf[["Private_Loans"]]
    privateLoans_Interest_Rate = 1 + finalDf[["Private_Loan_Interest_Rate"]] / 12
    privateLoans_Payment = finalDf[["Private_Loan_Payment"]]
    
    investments = finalDf[["Investments"]]
    investment_Contribution_Roth = finalDf[["Investment_Contribution_Roth"]]
    investment_Contribution_IRA = finalDf[["Investment_Contribution_IRA"]]
    investment_Return_Rate = finalDf[["Investment_Return_Rate"]]
    
    netIncome = finalDf[["NetIncome"]]
    avgSpend = finalDf[["AvgSpend"]]
    totalSavings = finalDf[["Total_Savings"]]
    netWorth = finalDf[["NetWorth"]]
    baseSalary = finalDf[["BaseSalary"]]
    
    if (abs(publicLoans[i - 1]) < 10) {
      publicLoans_Payment[i] = 0
      publicLoans[i] = 0
    } else {
      finalDf[["Public_Loans"]][i] = (publicLoans[i - 1] * publicLoans_Interest_Rate[i]) + publicLoans_Payment[i]
    }
    
    if (abs(privateLoans[i - 1]) < 10) {
      privateLoans_Payment[i] = 0
      privateLoans[i] = 0
    } else {
      finalDf[["Private_Loans"]][i] = (privateLoans[i - 1] * privateLoans_Interest_Rate[i]) + privateLoans_Payment[i]
    }
    
    finalDf[["NetIncome"]][i] = getNetIncome(categoryList) %>% getProjectedIncome(baseSalary[i - 1])
    
    finalDf[["Total_Savings"]][i] = totalSavings[i-1] + finalDf[["NetIncome"]][i] + avgSpend[i] - 
      (privateLoans_Payment[i] + publicLoans_Payment[i])
    
    requiredSavings = abs(avgSpend[i] * getMinimumSavings_Months(monthsOfSavings))
    loans = abs(finalDf[["Private_Loans"]][i] + finalDf[["Public_Loans"]][i])
    
    additionalInvestment = if_else(finalDf[["Total_Savings"]][i] > (requiredSavings + loans),
                                   finalDf[["Total_Savings"]][i] - (requiredSavings + loans),
                                   0)
    
    investmentContribution = getInvestments(categoryList, investment_Contribution_Roth, investment_Contribution_IRA)
    
    finalDf[["Investments"]][i] = (1 + investment_Return_Rate[i] / 12) *
      (investments[i - 1] + investmentContribution + additionalInvestment)
    
    finalDf[["Total_Savings"]][i] = finalDf[["Total_Savings"]][i] - additionalInvestment
    
    finalDf[["NetWorth"]][i] = finalDf[["Total_Savings"]][i] + finalDf[["Investments"]][i] + 
      finalDf[["Private_Loans"]][i] + finalDf[["Public_Loans"]][i]
  }
  
  finalDf %>% mutate(Total_Loans = Private_Loans + Public_Loans,
                     Total_Assets = Total_Savings + Investments)
}