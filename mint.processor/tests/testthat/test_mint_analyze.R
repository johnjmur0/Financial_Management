testthat::context('test main mint functions')

test_agg_transactions_df = tibble::tibble(
    'year' = c(2022, 2022, 2022, 2022),
    'month' = c(1, 1, 2, 2),
    'day' = c(3, 4, 3, 4),
    'total' = c(-10, 20, -20, 40),
    'category' = c('discretionary', 'income', 'discretionary', 'income'),
)


testthat::test_that('test get_manual_adjustments', {

})

testthat::test_that('test get_projections', {

    
})