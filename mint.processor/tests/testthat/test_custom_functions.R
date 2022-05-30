testthat::context('test custom/config related functions')

library(dplyr)

test_transactions_df = readr::read_csv('test_transactions.csv')

testthat::test_that('test get_manual_adjustments', {

  test_config = c()
  test_config[["Manual_Adjustments"]] = list(list('Month' = 4, 
                                                  'Year' = 2019,
                                                  'Var' = 'Investments',
                                                  'Type' = 'Credit',
                                                  'Amount' = 1000))

  adjustments = mint.processor::get_manual_adjustments(test_config)

  testthat::expect_equal(lubridate::ymd(str_c(2019, 4, 1, sep = "/")), adjustments[["TimeAdj"]])
  testthat::expect_equal(1000, adjustments[["Amt"]])
  testthat::expect_equal('Credit', adjustments[["Type"]])
  testthat::expect_equal('Investments', adjustments[["Var"]])
})

testthat::test_that('test get_account_balances', {
  
  skip('Calling function that is not being used. Replace or delete')
  
  test_config = jsonlite::toJSON(tibble('Base_Salary' = 1000000))

  test_series = seq(lubridate::ymd(str_c(2022, 1, 1, sep = "/")), 
                    lubridate::ymd(str_c(2022, 2, 1, sep = "/")), by = 'month')
  
  test_account_df = config.handler::get_account_df(test_config)
  
  account_balances = mint.processor::get_account_balances(test_account_df, test_config, test_series)
  
  testthat::expect_equal()
})

#rest of functions are just data transformations, test later