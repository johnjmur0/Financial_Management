get_projection_inputs = function(category_df, transactions, current_accounts, account_changes, forecast_time_series, historical_start_date)
{
  avg_spend_monthly = get_avg_spend_monthly(category_df)
  
  net_income = get_net_income(category_df, historical_start_date) %>% get_projected_income(current_accounts[["BaseSalary"]])
  
  tibble("Timestamp" = forecast_time_series, 
         "NetIncome" = net_income, 
         "AvgSpend" = avg_spend_monthly, 
         "NetWorth" = 0) %>% 
    
    left_join(current_accounts, by=c("Timestamp")) %>% left_join(account_changes, by=c("Timestamp")) %>% 
    
    select(-Timestamp) %>% mutate_all(funs(if_else((is.na(.)), 0, .))) %>% 
    
    bind_cols(data.frame("Timestamp" = forecast_time_series)) %>% as_tibble()
}

adjustTimeVariable = function(i, account_changes, projection_inputs)
{
  variable = account_changes[["Var"]]
  
  i = ifelse((variable == "BaseSalary" | variable == "Investment_Contribution"), i + 1, i)
  
  account_changes[["Amt"]] = if_else(account_changes[["Amt"]] == Inf, 
                                    projection_inputs[[variable]][[i - 1]] * -1,
                                    as.numeric(account_changes[["Amt"]]))
  
  projection_inputs[[variable]][[i - 1]] = account_changes[["Amt"]] + projection_inputs[[variable]][[i - 1]]
  
  if (account_changes[["Type"]] == "Debit") {
    projection_inputs[["Total_Savings"]][i - 1] = projection_inputs[["Total_Savings"]][i -1] - account_changes[["Amt"]]
  }
  
  return (projection_inputs)
}

creat_projection_df = function(projection_inputs, account_adjustments, category_df, fixed_payments, min_savings_months)
{
  for (i in 2:nrow(projection_inputs)) {
    
    print(i)
    
    projection_inputs[["BaseSalary"]][i] = projection_inputs[["BaseSalary"]][i - 1]
    projection_inputs[["Investment_Contribution"]][i] = projection_inputs[["Investment_Contribution"]][i - 1]
    
    account_adjustments = account_adjustments %>% dplyr::filter(TimeAdj == projection_inputs[["Timestamp"]][i])
    
    if (nrow(account_adjustments) > 0) {
      
      projection_inputs = i %>% adjustTimeVariable(account_adjustments, projection_inputs)
    }
    
    publicLoans = projection_inputs[["Public_Loans"]]
    publicLoans_Interest_Rate = 1 + projection_inputs[["Public_Loan_Interest_Rate"]] / 12
    publicLoans_Payment = projection_inputs[["Public_Loan_Payment"]]
    
    investments = projection_inputs[["Investments"]]
    investment_Contribution_Roth = projection_inputs[["Investment_Contribution_Roth"]]
    investment_Contribution_IRA = projection_inputs[["Investment_Contribution_IRA"]]
    investment_Return_Rate = projection_inputs[["Investment_Return_Rate"]]
    
    netIncome = projection_inputs[["NetIncome"]]
    avgSpend = projection_inputs[["AvgSpend"]]
    totalSavings = projection_inputs[["Total_Savings"]]
    netWorth = projection_inputs[["NetWorth"]]
    baseSalary = projection_inputs[["BaseSalary"]]
    
    if (abs(publicLoans[i - 1]) < 10) {
      publicLoans_Payment[i] = 0
      publicLoans[i] = 0
    } else {
      projection_inputs[["Public_Loans"]][i] = (publicLoans[i - 1] * publicLoans_Interest_Rate[i]) + publicLoans_Payment[i]
    }
    
    projection_inputs[["NetIncome"]][i] = get_net_income(category_df) %>% 
      get_projected_income(baseSalary[i - 1])
    
    projection_inputs[["Total_Savings"]][i] = totalSavings[i-1] + projection_inputs[["NetIncome"]][i] + avgSpend[i] - (publicLoans_Payment[i])
    
    requiredSavings = abs(avgSpend[i] * min_savings_months)
    loans = abs(projection_inputs[["Public_Loans"]][i])
    
    additionalInvestment = if_else(projection_inputs[["Total_Savings"]][i] > (requiredSavings + loans),
                                   projection_inputs[["Total_Savings"]][i] - (requiredSavings + loans),
                                   0)
    
    investmentContribution = fixed_payments[['Annual_401k_Contribution']] / 12
    
    projection_inputs[["Investments"]][i] = (1 + investment_Return_Rate[i] / 12) *
      (investments[i - 1] + investmentContribution + additionalInvestment)
    
    projection_inputs[["Total_Savings"]][i] = projection_inputs[["Total_Savings"]][i] - additionalInvestment
    
    projection_inputs[["NetWorth"]][i] = 
      projection_inputs[["Total_Savings"]][i] + projection_inputs[["Investments"]][i] + projection_inputs[["Public_Loans"]][i]
  }
  
  projection_inputs %>% mutate(Total_Loans = Public_Loans,
                               Total_Assets = Total_Savings + Investments)
}