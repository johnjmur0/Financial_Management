context('test custom/config related functions')

test_that('get_config_field_generic valid', {
  
  test_config = c()
  test_config[['Foo']] = 'bar'
  
  result = get_config_field_generic(test_config, 'Foo')
  expect_equal('bar', result)
})

test_that('get_config_field_generic invalid', {
  
  test_config = c()
  
  expect_error(get_config_field_generic(test_config, 'Foo'), 'Getting Foo from config failed')
})

test_that('get_numeric_field valid', {
  
  test_config = c()
  test_config[['Base_Salary']] = '100'
  
  salary = config.handler::get_numeric_val_from_config(test_config, field_name = 'Base_Salary')
  
  expect_equal(100, salary)
})

test_that('get_numeric_field invalid', {
  
  test_config = c()
  test_config[['Base_Salary']] = 'Foo'
  
  expect_error(config.handler::get_numeric_val_from_config(test_config, 'Base_Salary'),
                         'Base_Salary Foo from config is not numeric')
})