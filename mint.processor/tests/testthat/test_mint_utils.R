testthat::context('test mint util functions')

test_that('test aggregate_categories', {

    date_vec = c(lubridate::mdy('1/1/2022'), lubridate::mdy('1/1/2022'), 
            lubridate::mdy('1/2/2022'), lubridate::mdy('1/3/2022'))

    test_category_df = tibble::tibble(
    'date' = date_vec,
    'year' = c(2022, 2022, 2022, 2022),
    'month' = c(1, 1, 1, 1),
    'day' = c(1, 1, 2, 2),
    'total' = c(1, 2, 3, 4),
    'category' = c('A', 'B', 'C', 'D')  
    )
    
    agg_vec = list('first' = c('A', 'B'), 'second' = c('C', 'D'))
    time_vec = c('year', 'month', 'day')
    sum_col = 'total'

    agg_df = test_category_df %>% aggregate_categories(agg_vec, sum_col, time_vec)

    expect_equal(agg_df %>% nrow(), 2)
    expect_equal(agg_df %>% colnames(), c(time_vec, sum_col, 'category'))

    expect_equal(agg_df[['category']] %>% unique(), c('first', 'second'))
    expect_equal(agg_df %>% filter(category == 'first') %>% pull(total) %>% sum(), 3)
    expect_equal(agg_df %>% filter(category == 'second' & day == 2) %>% pull(total) %>% sum(), 7)
})

test_that('test clean_transactions_df', {

    test_raw_transactions = tibble::tibble(
        'date' = c('2022-01-01', '2022-01-02', '2022-01-03'),
        'description' = c('foo', 'foo', 'bar'),
        'amount' = list(c(10), c(-20), c(30)),
        'type' = c('CashAndCreditTransaction', 'CashAndCreditTransaction', 'CashAndCreditTransaction'),
        'category' = list(list('name' = 'a', 'id' = 1), list('name' = 'a', 'id' = 2), list('name' = 'b', 'id' = 1)),
        'accountId' = c(1, 1, 1)
    )

    clean_transactions_df = test_raw_transactions %>% clean_transactions_df()

    expect_equal(clean_transactions_df %>% nrow(), 3)
    
    expected_cols = c(test_raw_transactions %>% colnames(), 'id', 'year', 'month', 'day') %>% sort()
    expect_equal(clean_transactions_df %>% colnames() %>% sort(), expected_cols)
    expect_equal(clean_transactions_df['amount'] %>% sum(), 20)
})

test_that('test clean_accounts_df', {

    test_raw_account = tibble::tibble(
        'name' = c('Foo', 'Bar', 'Bat'),
        'type' = c('BankAccount', 'InvestmentAccount', 'CreditAccount'),
        'systemStatus' = c('ACTIVE', 'ACTIVE', 'DEAD'),
        'currentBalance' = list(list(100), list(50), list(0)),
        'availableBalance' = list(list(100), list(50), list(0)),
    )

    clean_accounts_df = test_raw_account %>% clean_accounts_df()

    expect_equal(clean_accounts_df %>% nrow(), 2)
    expect_equal(clean_accounts_df %>% colnames() %>% sort(), c('account_type', 'total'))

    expect_equal(clean_accounts_df['account_type'] %>% distinct() %>% pull(), c('bank', 'investment'))
    expect_equal(clean_accounts_df['total'] %>% sum(), 150)
    expect_equal(clean_accounts_df %>% filter(account_type == 'bank') %>% select(total) %>% pull(), 100)
})


test_that('test summarise_categories with outlier', {

    test_clean_transactions = tibble::tibble(
        'date' = c(lubridate::mdy('1/1/2022'), lubridate::mdy('1/1/2022'), lubridate::mdy('1/1/2022')),
        'year' = c(2022, 2022, 2022),
        'month' = c(1, 1, 1),
        'day' = c(1, 1, 1),
        'amount' = c(10, -20, 30),
        'category' = c('A', 'A', 'B'),
    )

    test_config = list()
    test_config[['transactions_params']][['ignore_categories']] = c('foo', 'bar')
    agg_vec = c('year', 'month', 'day')

    summed_transactions = test_clean_transactions %>% 
        summarise_categories(test_config, lubridate::mdy('1/1/2022'), agg_vec, include_outlier = TRUE)

    expected_cols = c('year', 'month', 'day', 'total', 'category')
    expect_equal(summed_transactions %>% nrow(), 2)
    expect_equal(summed_transactions %>% colnames() %>% sort(), expected_cols %>% sort())
    expect_equal(summed_transactions %>% filter(category == 'A') %>% pull(total), -10)
    expect_equal(summed_transactions %>% filter(category == 'B') %>% pull(total), 30)
})