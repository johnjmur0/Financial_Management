testthat::context('test custom/config related functions')

test_transactions_df = readr::read_csv('test_transactions.csv')

testthat::test_that('test get_manual_adjustments', {

  test_config = c()
  test_config[["Manual_Adjustments"]] = list(list('Month' = 4, 
                                                  'Year' = 2019,
                                                  'Var' = 'Investments',
                                                  'Type' ='Credit',
                                                  'Amount' = 1000))

  adjustments = Process_Mint::get_manual_adjustments(test_config)

  testthat::expect_equal(lubridate::ymd(str_c(2019, 4, 1, sep = "/")), adjustments[["TimeAdj"]])
  testthat::expect_equal(1000, adjustments[["Amt"]])
  testthat::expect_equal('Credit', adjustments[["Type"]])
  testthat::expect_equal('Investments', adjustments[["Var"]])
})

testthat::test_that('test get_account_balances', {
  
  test_config = c()
  test_config[["Base_Salary"]] = 1000000
  test_series = seq(lubridate::ymd(str_c(2022, 1, 1, sep = "/")), 
                    lubridate::ymd(str_c(2022, 2, 1, sep = "/")), by='month')
  
  Process_Mint::get_account_df()
  
  account_balances = Process_Mint::get_account_balances(test_account_df, test_config, test_series)
  
  testthat::expect_equal()
})

#rest of functions are just data transformations, test later