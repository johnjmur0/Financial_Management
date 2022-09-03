testthat::context('test mint util functions')

test_user_name = 'jjm'

test_that('test get_mint_module', {

    testthat::expect_error(mint_module_memoised())
})

test_that('test get_mint_investments', {

    skip('Need to update get_mint_data to reflect 2.0 updates from mintapi')
    
    investments_df = get_mint_investments(test_user_name, read_cache = FALSE, write_cache = FALSE)

    testthat::expect_true(investments_df %>% nrow() > 100)
    testthat::expect_equal(investments_df %>% colnames(), c('foo', 'bar'))
})

test_that('test get_mint_accounts', {

    accounts_df = get_mint_accounts(test_user_name, read_cache = FALSE, write_cache = FALSE)

    expected_cols = c('name', 'type', 'systemStatus', 'currentBalance', 'availableBalance')
    testthat::expect_true(accounts_df %>% nrow() > 10)
    testthat::expect_equal(accounts_df %>% colnames(), expected_cols)
})

test_that('test get_mint_transactions', {

    transactions_df = get_mint_transactions(test_user_name, read_cache = FALSE, write_cache = FALSE)

    expected_cols = c('date', 'description', 'amount', 'type', 'category', 'accountId')
    testthat::expect_true(transactions_df %>% nrow() > 4000)
    testthat::expect_equal(transactions_df %>% colnames(), expected_cols)
})