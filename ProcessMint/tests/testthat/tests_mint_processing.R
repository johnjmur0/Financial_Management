library(lubridate)

context("Test processing of Mint transactions")

test_that("Transaction files are parsed correctly", {
  
  transaction_files = c("transactions_04-03-2021.csv", "transactions_04-10-2021.csv",
                       "transactions_04-26-2020.csv", "transactions_08_03_2021.csv",
                       "transactions_09-20-2019.csv", "transactions_10-06-2019.csv",
                       "transactions_11-21-2020.csv")
  
  expected = ("08-03-2021")
  result = ProcessMint::get_most_recent_file_date(transaction_files)
  testthat::expect_equal(expected, result)
})

test_that("Test summarize monthly transactions", {
  
  test_transactions = tibble("Date"=c("1/1/2019", "2/1/2019", "3/1/2019", "4/1/2019"), 
                             "Description"=c("foo", "foo", "foo", "foo"),
                             "Amount"=c(10, -20, 30, -40)) %>% 
    
    mutate(Date = mdy(Date))
  
  result = ProcessMint::get_monthly_summary(test_transactions)
  testthat::expect_equal(4, nrow(result))
  #These columns are sufficient conditions that the rest of columns are correct
  testthat::expect_true(all(c("Year_Month", "Profit_Total_Monthly") %in% colnames(result)))
})