aggregate_categories = function(category_vec, spend_df, col_name, time_vec)  {
  
  lapply(category_vec, function(category_val, spend_df, col_name, time_vec) {
    
    category_definition = category_val %>% pull() %>% tolower()
    
    spend_df %>% 
      filter(category %in% category_definition) %>% 
      group_by(!!!syms(time_vec)) %>% 
      summarise(total := sum(!!sym(col_name))) %>% 
      mutate(category = category_val %>% colnames())
  },
  spend_df = spend_df,
  col_name = col_name,
  time_vec = time_vec) %>% 
  bind_rows()
}

aggregate_categories_small = function(category_df, time_vec) {

  #TODO put these all in configs
  ####### Aggregated categories ####
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
  
  all_categories %>% aggregate_categories(category_df, 'total', time_vec)
}

aggregate_categories_big = function(agg_spend_df, time_vec) {

  #TODO put into configs
  disrectionary = tibble("discretionary" = c("taxi", "gift", "drinking", 
                                             "personal", "shopping", "foodGroup", 
                                             "uncategorized", "bills"))
  music = tibble("music" = c("hobbies", "coaching"))
  housing = tibble("housing" = c("rent"))
  groceries = tibble("groceries" = c("groceries"))
  loans = tibble("loans" = c("loans"))
  income = tibble("income" = c("taxes_insurance", "paycheck", "miscIncome"))
  bonus = tibble("bonus" = c("bonus"))
  
  #get other contributions included here
  investments = tibble("investments" = c("investments"))
                         
  meta_category_vec = list(disrectionary, music, housing, groceries, income, investments, loans, bonus)
  
  meta_category_vec %>% aggregate_categories(agg_spend_df, 'total', time_vec)
}