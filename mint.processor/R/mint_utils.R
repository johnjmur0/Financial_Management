aggregate_categories <- function(spend_df, category_list, col_name, time_vec) {
  lapply(seq_along(category_list), function(i, spend_df, col_name, time_vec) {
    category_name <- names(category_list)[i]
    category_definition <- category_list[i][[1]] %>% tolower()

    spend_df %>%
      dplyr::filter(tolower(category) %in% category_definition) %>%
      group_by(!!!syms(time_vec)) %>%
      summarise(total := sum(!!sym(col_name))) %>%
      mutate(category = category_name)
  },
  spend_df = spend_df,
  col_name = col_name,
  time_vec = time_vec
  ) %>%
    bind_rows()
}

#' clean raw transactions df
#'
#' @param transactions_df raw mint transactions df
#'
#' @export
#'
clean_transactions_df <- function(transactions_df) {
  transactions_df %>%
    janitor::clean_names() %>%
    mutate(date = lubridate::with_tz(lubridate::mdy(date), "UTC")) %>%
    dplyr::filter(date <= Sys.time()) %>%
    mutate(
      amount = unlist(amount),
      year = lubridate::year(date),
      month = lubridate::month(date),
      day = as.numeric(lubridate::day(date))
    )
}

#' clean raw accounts df
#'
#' @param accounts_df raw mint accounts df
#'
#' @export
#'
clean_accounts_df <- function(accounts_df) {
  accounts_df %>%
    dplyr::filter(systemStatus == "ACTIVE") %>%
    rename(account_type = type, total = currentBalance) %>%
    select(account_type, total) %>%
    mutate(account_type = case_when(
      account_type %in% c("CreditAccount", "BankAccount", "CashAccount") ~ "bank",
      account_type == "InvestmentAccount" ~ "investment",
      account_type == "LoanAccount" ~ "debt",
      TRUE ~ account_type
    )) %>%
    group_by(account_type) %>%
    summarise(total = sum(total)) %>%
    dplyr::filter(total != 0)
}

summarise_categories <- function(transactions_df, config_list, start_date, agg_vec, include_outlier = TRUE) {
  lapply(agg_vec, function(val) {
    utilities::df_col_has_value(transactions_df, val, "numeric")
  })

  utilities::df_col_has_value(transactions_df, "category", "character")
  utilities::df_col_has_value(transactions_df, "amount", "numeric")

  # TODO handle outlier categories, plus category + amount combo
  if (!include_outlier) {
    transactions_df <- lapply(config_list[["outlier_months"]], function(outlier, transactions_df) {
      exclude_month <- lubridate::ymd(str_c(outlier[["year"]], outlier[["month"]], 1, sep = "-"))
      transactions_df %>%
        mutate(Year_Month = lubridate::ymd(str_c(Year, Month, 1, sep = "-"))) %>%
        dplyr::filter(Year_Month != exclude_month)
    },
    transactions_df = transactions_df
    ) %>%
      bind_rows() %>%
      distinct()
  }

  ignore_categories <- config_list[["transactions_params"]][["ignore_categories"]]

  transactions_df %>%
    dplyr::filter(date >= start_date & !(category %in% ignore_categories)) %>%
    group_by(!!!syms(agg_vec), category) %>%
    summarise(total = sum(amount)) %>%
    dplyr::filter(total != 0) %>%
    ungroup()
}