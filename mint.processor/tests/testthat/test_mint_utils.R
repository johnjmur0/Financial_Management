testthat::context('test mint util functions')

date_vec = c(lubridate::mdy('1/1/2022'), lubridate::mdy('1/1/2022'), 
            lubridate::mdy('1/2/2022'), lubridate::mdy('1/3/2022'))

test_category_df = tibble(
  'date' = date_vec,
  'year' = c(2022, 2022, 2022, 2022),
  'month' = c(1, 1, 1, 1),
  'day' = c(1, 1, 2, 2),
  'total' = c(1, 2, 3, 4),
  'category' = c('A', 'B', 'C', 'D')  
)

test_that('test aggregate_categories', {

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
