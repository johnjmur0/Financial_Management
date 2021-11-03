testthat::context('test custom/config related functions')

testthat::test_that('test get_manual_adjustments', {

  test_config = c()
  test_config[["Manual_Adjustments"]] = list(list("Month" = 4, "Year" = 2019,
                                                  'Var' = 'Investments','Type' ='Credit',
                                                  'Amount' = 1000))

  adjustments = ProcessMint::get_manual_adjustments(test_config)

  testthat::expect_equal(lubridate::ymd(str_c(2019, 4, 1, sep = "/")), adjustments[["TimeAdj"]])
  testthat::expect_equal(1000, adjustments[["Amt"]])
  testthat::expect_equal('Credit', adjustments[["Type"]])
  testthat::expect_equal('Investments', adjustments[["Var"]])
})

#rest of functions are just data transformations, test later