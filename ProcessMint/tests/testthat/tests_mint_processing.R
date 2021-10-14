library(lubridate)

get_test_transactions = function()
{
  tibble("date" = c("1/1/2019", "2/1/2019", "3/1/2019", "4/1/2019"), 
         "category" = c("foo", "bar", "foo", "bar"),
         "amount" = c(10, 20, 30, 40),
         "transaction_type" = c("credit", "debit", "credit", "debit")) %>% 
    
    mutate(date = lubridate::mdy(date))
}

get_test_category_df = function()
{
  tibble("Year" = c(2019, 2019, 2019, 2019),
         "Month" = c(1, 2, 3, 4),
         "category" = c("foo", "bar", "foo", "bar"),
         "Monthly_Total" = c(10, -20, 30, -40),
         "Type" = c("Credit", "Debit", "Credit", "Debit"))
    
}

context("Test processing of Mint transactions")

test_that("Test summarize monthly transactions", {
  
  test_transactions = get_test_transactions()
  
  result = ProcessMint::get_monthly_summary(test_transactions)
  testthat::expect_equal(4, nrow(result))
  #These columns are sufficient conditions that the rest of columns are correct
  testthat::expect_true(all(c("Year", "Month", "Profit_Total_Monthly") %in% colnames(result)))
  
  all_df = result %>% summarise(Total_Expense = sum(Expense_Total_Monthly),
                                Total_Profit = sum(Profit_Total_Monthly))
  
  testthat::expect_equal(-60, all_df[['Total_Expense']])
  testthat::expect_equal(-20, all_df[['Total_Profit']])
})

test_that("Test summarize transactions by category", {
  
  test_config = c()
  test_config[["Outlier_Months"]] = list(list("Month" = 4, "Year" = 2019))
  
  test_transactions = get_test_transactions() %>% 
    rename(Date = date) %>% mutate(Year = lubridate::year(Date),
                                   Month = lubridate::month(Date),
                                   Amount = if_else(transaction_type == "debit", amount * -1, amount))
  
  result = ProcessMint::monthly_category_sum(test_transactions, 
                                             test_config, 
                                             min(test_transactions[["Date"]]),
                                             include_outlier = FALSE)
  
  foo = result %>% dplyr::filter(category == 'foo')
  bar = result %>% dplyr::filter(category == 'bar')
  
  testthat::expect_equal(unique(foo[['Type']]), 'Credit')
  testthat::expect_equal(unique(bar[['Type']]), 'Debit')
  
  testthat::expect_equal(30, foo %>% summarise(Total = sum(Monthly_Total)) %>% pull(Total))
  testthat::expect_equal(-20, bar %>% summarise(Total = sum(Monthly_Total)) %>% pull(Total))
  
  testthat::expect_equal(2, nrow(result))
  testthat::expect_equal(1, nrow(bar))
  #test start date filtering
  testthat::expect_true(min(result[['Month']]) == 2 & min(result[['Year']]) == 2019)
  
  #test outlier filtering
  testthat::expect_true(max(result[['Month']]) == 3 & max(result[['Year']]) == 2019)
})

test_that("Test Get Monthly Spend", {
  
  test_category_df = get_test_category_df()
  test_transactions = get_test_transactions()
  
  avg_spend_df = ProcessMint::get_avg_spend_monthly(test_category_df, 
                                                    test_transactions, 
                                                    return_df = FALSE, 
                                                    remove_income = TRUE, 
                                                    remove_loans = TRUE)
  
  testthat::expect_equal(-30, avg_spend_df)
})

#TODO add get_avg_spend_monthly with return_df

test_that("Test Get Monthly Income", {
  
  test_category_df = get_test_category_df()
  test_transactions = get_test_transactions()
  
  avg_spend_df = ProcessMint::get_avg_income(test_category_df)
  
  testthat::expect_equal(20, avg_spend_df)
})

