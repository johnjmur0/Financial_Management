get_distribution = function(spend_df, group_vec) {
  
  spend_df %>% 
  
    group_by(!!!syms(group_vec)) %>% 
    
    summarize(Min = min(TotalValue),
              p25 = quantile(TotalValue, probs = .25),
              p50 = quantile(TotalValue, probs = .5),
              Mean = mean(TotalValue), 
              p75 = quantile(TotalValue, probs = .75),
              Max = max(TotalValue),
              SD = sd(TotalValue))
}

aggregate_categories = function(category_vec, spend_df, spend_var)  {
  
  lapply(category_vec, function(category_val, spend_df, spend_var) {
    
    spend_df %>% 
      filter(Category %in% (category_val %>% pull() %>% tolower())) %>% 
      group_by(Year, Month) %>% 
      summarise(total_value := sum(!!sym(spend_var))) %>% 
      mutate(Category = category_val %>% colnames())
  },
  spend_df = spend_df,
  spend_var = spend_var) %>% 
  bind_rows()
}

aggregate_categories_small = function(monthly_spend_df) {

  #TODO put these all in configs
  #### Aggregated categories ####
  taxes_insurance = tibble("taxes_insurance" = c("Federal Tax", "State Tax", "Taxes", 
                                                 c("Life Insurance", "Home Insurance", "Health Insurance")))
  
  hobbies = tibble("hobbies" = c("Books", "Music", "Movies & DVDs", "Hobbies", "Entertainment"))
  
  food = tibble("foodGroup" = c("Fast Food", "Restaurants", "Food & Dining", "Coffee Shops"))
  
  taxi = tibble("taxi" = c("Rental Car & Taxi", "Gas & Fuel", "Public Transportation"))
  
  gift = tibble("gift" = c("Charity", "Gift", "Gifts & Donations"))
  
  coaching = tibble("coaching" = c("Financial", "Financial Advisor"))
  
  investments = tibble("investments" = c("Investments", "Buy"))
  
  personal = tibble("personal" = c("Personal Care", "Health & Fitness", "Laundry", "Pharmacy", 
                                      "Clothing", "Travel", "Hair", "Misc Expenses", "Service & Parts", 
                                   
                                   c("Reimbursement", "Returned Purchase", "Transfer for Cash Spending"), 
                                   
                                   c("Home Supplies", "Home Improvement", "Furnishings", "Electronics & Software")))
  
  misc_income = tibble("miscIncome" = c("Interest Income", "Income"))
  
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
  
  all_categories = list(hobbies, food, taxi, gift, personal, coaching, investments, 
                       rent, drinking, bills_fees, groceries, shopping, taxes_insurance,
                       ignore, misc_income, paycheck, loans, bonus)
  
  agg_spend_df = all_categories %>% aggregate_categories(monthly_spend_df, "TotalSpend") 
  
  return(agg_spend_df)
}

aggregate_categories_big = function(agg_spend_df) {

  #TODO put into configs
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
                         
  meta_category_vec = list(disrectionary, music, housing, groceries, income, investments, loans, bonus)
  
  meta_spend_df = meta_category_vec %>% aggregate_categories(agg_spend_df, "total_value")
  
  return(meta_spend_df)
}

final_spend_summary = function(meta_spend_df) {

  #TODO Something botched here - Mint doesn't have transactions, can't go that far back in BoA
  clean_spend_df = meta_spend_df %>% 
  
    filter(!(Year == 2019 & Month == 1) & !(Year == 2018 & Month == 12)) %>% 
    
    #Remove current month b/c its almost always unfinished
    filter(!(Year == lubridate::year(Sys.time()) & Month == lubridate::month(Sys.time()))) %>% 
    
    #TODO get other contributions added here
    filter(Category != "investments") %>% 
    
    ungroup()

  total_profit_df = clean_spend_df %>% 
    mutate(Timestamp = lubridate::mdy(str_c(Month, 1, Year, sep = '/'))) %>%  
    spread(key = Category, value = total_value) %>% 
    #TODO expenses need to be negative, move this into config
    mutate(Profit = income + (discretionary + groceries + housing + loans + music)) %>% 
    gather(key = Category, value = total_value, -Year, -Month, -Timestamp)
  
  total_profit_df %>% 
  spread(key = Category, value = total_value) %>% 
  #TODO define this in config
  mutate(FutureExpenses = discretionary + groceries + music)
}

analyze_historical_spend = function(category_df) {

  monthly_spend_df = category_df %>% 
    
    get_avg_spend_monthly(return_df = TRUE, remove_income = FALSE, remove_loans = FALSE) %>% 
    
    rename(TotalSpend = Total_Spend,
           Category = category) %>% 
    
    arrange(Year, Month, Category)
   
  agg_spend_df = aggregate_categories_small(monthly_spend_df)  
  
  meta_spend_df = agg_spend_df %>% aggregate_categories_big()
  
  future_spend_df = meta_spend_df %>% final_spend_summary()
}