testthat::context('test custom/config related functions')

#test_accounts_json = rjson::fromJSON(file = 'test_accounts.json')

testthat::test_that('test get_config_field_generic valid', {
  
  test_config = c()
  test_config[['Foo']] = 'bar'
  
  result = get_config_field_generic(test_config, 'Foo')
  testthat::expect_equal('bar', result)
})

testthat::test_that('test get_config_field_generic invalid', {
  
  test_config = c()
  
  testthat::expect_error(get_config_field_generic(test_config, 'Foo'), 
                         'Getting Foo from config failed')
})

testthat::test_that('test get_numeric_field valid', {
  
  test_config = c()
  test_config[['Base_Salary']] = '100'
  
  salary = Configuration::get_numeric_val_from_config(test_config, field_name = 'Base_Salary')
  
  testthat::expect_equal(100, salary)
})

testthat::test_that('test get_numeric_field invalid', {
  
  test_config = c()
  test_config[['Base_Salary']] = 'Foo'
  
  testthat::expect_error(Configuration::get_numeric_val_from_config(test_config, 'Base_Salary'),
                         'Base_Salary Foo from config is not numeric')
})