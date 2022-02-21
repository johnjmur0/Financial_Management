historicalSpendAnalysis = function(categoryList)
{
  monthlySpend = getMonthlyAvgSpend(categoryList, returnDf = TRUE, removeIncome = FALSE, removeLoans = FALSE) %>% 
    rename(TotalSpend = totalSpend) %>% arrange(Year, Month, Category)
  
  dir = getRootDir(overviewDf)
  allTransactions = list.files(dir, full.names = TRUE)
  mostRecentFileDate = getRecentFileName(allTransactions)

  #WIP 
  aggSpending = aggregateCategories_Small(monthlySpend, mostRecentFileDate, dir)  
  
  aggSpending %>% barChart_Category() 
  
  metaSpending = aggSpending %>% aggregateCategories_Big()
  
  metaSpending %>% barChart_Category() 
  
  futureSpendingDf = metaSpending %>% finalSpendingSummary()
  futureSpendingDf %>% mutate(TotalValue = FutureExpenses) %>% getDistribution(c())
  futureSpendingDf %>% mutate(TotalValue = income) %>% getDistribution(c("Year"))
  
  #Next, take out alcohol from future, anything else not relevent (lunch food, etc)
  #Make forward looking budget
  #Figure out why older costs were lower, but so was profit despite income being comparable 
}

finalSpendingSummary = function(metaSpending) 
{
  #Something botched here - Mint doesn't have transactions, can't go that far back in BoA
  cleanSpending = metaSpending %>% dplyr::filter(!(Year == 2019 & Month == 1) &
                                   !(Year == 2018 & Month == 12)) %>% 
    
    #Remove current month b/c its almost always unfinished
    dplyr::filter(!(Year == Sys.time() %>% lubridate::year() & 
                      Month == Sys.time() %>% lubridate::month())) %>%
    
    #get other contributions added here
    dplyr::filter(Category != "investments") %>% ungroup()
  
  cleanSpending %>% mutate(TotalValue = abs(TotalValue)) %>% getDistribution(c("Category"))
  
  totalProfit = cleanSpending %>% addTimestamp() %>% spread(key=Category, value=TotalValue) %>% 
    mutate(Profit = discretionary + groceries + housing + loans + music + income) %>% 
    gather(key = Category, value=TotalValue, -Year, -Month, -Timestamp)
  
  totalProfit %>% dplyr::filter(!(Category %in% c("bonus", "income"))) %>%
    mutate(TotalValue = abs(TotalValue),
           Profit_Cost = if_else(Category == "Profit", "Revenue", "Cost")) %>%
    ggplot() + geom_line(aes(x=Timestamp, y=TotalValue, color=Category)) + 
    geom_hline(yintercept = 0) + geom_hline(yintercept=3000) + facet_wrap(~Profit_Cost)
  
  totalProfit %>% dplyr::filter(Category == "Profit") %>% getDistribution(c())
  
  totalProfit %>% spread(key=Category, value=TotalValue) %>% 
    mutate(FutureExpenses = discretionary + groceries + music)
}

getDistribution = function(spendingDf, groupVec)
{
  spendingDf %>% group_by(!!!syms(groupVec)) %>% 
    
    summarize(Min = min(TotalValue),
              p25 = quantile(TotalValue, probs = .25),
              p50 = quantile(TotalValue, probs = .5),
              Mean = mean(TotalValue), 
              p75 = quantile(TotalValue, probs = .75),
              Max = max(TotalValue),
              SD = sd(TotalValue))
}

addTimestamp = function(df)
{
  if (!(c("Month", "Year") %in% colnames(df))) {
    stop(str_c("need month and year in df, only has", colnames(df), sep = " "))
  }
  
  df %>% mutate(Timestamp = lubridate::parse_date_time(str_c(Month, 1, Year, sep = "/"), orders = "%m/%d/%Y"))
}

barChart_Category = function(spendingDf) 
{
  spendingDf %>% addTimestamp() %>% dplyr::filter(Category != "paycheck") %>%
    
    #group_by(Timestamp) %>% summarise(TotalValue = sum(TotalValue)) %>% mutate(Category = "Total Food") %>% 
    
    ggplot() + geom_col(aes(x=Timestamp, y=TotalValue)) + facet_wrap(~Category) + geom_hline(yintercept = 0)
}

aggregateCategories_Big = function(aggSpending)
{
  disrectionary = tibble("discretionary" = c("taxi", "gift", "drinking", 
                                             "personal", "shopping", "foodGroup", "uncategorized", "bills"))
  music = tibble("music" = c("hobbies", "coaching"))
  housing = tibble("housing" = c("rent"))
  groceries = tibble("groceries" = c("groceries"))
  loans = tibble("loans" = c("loans"))
  income = tibble("income" = c("taxes_insurance", "paycheck", "miscIncome"))
  bonus = tibble("bonus" = c("bonus"))
  
  #get other contributions included here
  investments = tibble("investments" = c("investments"))
                         
  metaCategoryVec = list(disrectionary, music, housing, groceries, income, investments, loans, bonus)
  
  metaSpending = metaCategoryVec %>% aggregateCategories(aggSpending, "TotalValue")
  
  return (metaSpending)
}

aggregateCategories_Small = function(monthlySpend, mostRecentFileDate, dir)
{
  taxes_insurance = tibble("taxes_insurance" = c("Federal Tax", "State Tax", "Taxes", 
                                                 c("Life Insurance", "Home Insurance", "Health Insurance")))
  
  hobbies = tibble("hobbies" = c("Books", "Music", "Movies & DVDs", "Hobbies", "Entertainment"))
  
  foodGroup = tibble("foodGroup" = c("Fast Food", "Restaurants", "Food & Dining", "Coffee Shops"))
  
  taxi = tibble("taxi" = c("Rental Car & Taxi", "Gas & Fuel", "Public Transportation"))
  
  gift = tibble("gift" = c("Charity", "Gift", "Gifts & Donations"))
  
  coaching = tibble("coaching" = c("Financial", "Financial Advisor"))
  
  investments = tibble("investments" = c("Investments", "Buy"))
  
  personal = tibble("personal" = c("Personal Care", "Health & Fitness", "Laundry", "Pharmacy", 
                                      "Clothing", "Travel", "Hair", "Misc Expenses", "Service & Parts", 
                                   
                                   c("Reimbursement", "Returned Purchase", "Transfer for Cash Spending"), 
                                   
                                   c("Home Supplies", "Home Improvement", "Furnishings", "Electronics & Software")))
  
  miscIncome = tibble("miscIncome" = c("Interest Income", "Income"))
  
  rent = tibble("rent" = c("Mortgage & Rent"))
  
  drinking = tibble("drinking" = c("Alcohol & Bars")) 
  
  bills_fees = tibble("bills" = c("Bills & Utilities",
                                  c("ATM Fee", "Bank Fee", "Cash & ATM", "Service Fee", "Fees & Charges", "Deposit"),
                                  c("Transfer"))) 
  
  groceries = tibble("groceries" = c("Groceries")) 
  
  shopping = tibble("shopping" = c("Shopping")) 
  
  ignore = tibble("ignore" = c("Vacation", "Office Supplies", "Business Services"))
  
  paycheck = tibble("paycheck" = c("Paycheck"))
  bonus = tibble("bonus" = c("Bonus"))
  
  loans = tibble("loans" = c("Education", "Student Loan"))
  
  allCategories = list(hobbies, foodGroup, taxi, gift, personal, coaching, investments, 
                       rent, drinking, bills_fees, groceries, shopping, taxes_insurance,
                       ignore, miscIncome, paycheck, loans, bonus)
  
  aggSpending = allCategories %>% aggregateCategories(monthlySpend, "TotalSpend") 
    
  aggSpending %>% write_csv(file.path(dirname(dir), "SpendingData", 
                                      str_c("WIP_Aggregate_Statement_", mostRecentFileDate, ".csv")))
  
  return (aggSpending)
}

aggregateCategories = function(categoryVec, spendDf, spendVar) 
{
  lapply(categoryVec, function(categoryVal, spendDf, spendVar) {
    
    print (categoryVal)
    
    spendDf %>% dplyr::filter(Category %in% (categoryVal %>% pull())) %>% 
      group_by(Year, Month) %>% summarise(TotalValue := sum(!!sym(spendVar))) %>% mutate(Category = categoryVal %>% colnames())
    
  },
  spendDf=spendDf,
  spendVar=spendVar) %>% bind_rows()
}
