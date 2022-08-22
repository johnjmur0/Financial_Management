testthat::context('test generic utilities related functions')

library(dplyr)

testthat::test_that('test basic function', {

    test_df = tibble(
        'foo' = c(1, 2, 3),
        'bar' = c(4, 5, 6)
    )

    list_df = test_df %>% utilities::df_to_list()

    testthat::expect_equal(list_df[[1]], tibble('foo' = 1, 'bar' = 4))
})