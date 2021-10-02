library(lubridate)

context("Testing processing of Mint transactions")

test_that("Transaction files are parsed correctly", {
  
  transaction_files = c("transactions_04-03-2021.csv", "transactions_04-10-2021.csv",
                       "transactions_04-26-2020.csv", "transactions_08_03_2021.csv",
                       "transactions_09-20-2019.csv", "transactions_10-06-2019.csv",
                       "transactions_11-21-2020.csv")
  
  expected = ("08-03-2021")
  result = ProcessMint::get_most_recent_file_date(transaction_files)
  testthat::expect_equal(expected, result)
})

test_that("Test reading file", {
  
  test_df = readr::read_csv("test_overview.csv")
})

test_that("Category stuff??", {
  
  test_transactions = readr::read_csv("test_transactions.csv") %>% 
    
    mutate(Date = parse_date_time(Date, orders = "%m/%d/%Y"),
           Amount = if_else(`Transaction Type` == "debit", Amount * -1, Amount))
  
  result = ProcessMint::get_monthly_summary(test_transactions)
  
  print ("foo")
})