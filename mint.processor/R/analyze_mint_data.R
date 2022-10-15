#' Get mint historical data aggregated
#'
#' @param transactions_df raw transactions
#' @param time_vec vector to aggregate by
#' @param config_list user config list
#'
#' @export
#'
summarize_transactions <- function(transactions_df, time_vec, config_list) {
  year <- NULL

  clean_transactions_df <- transactions_df %>% clean_transactions_df()

  historical_start_year <- clean_transactions_df %>%
    pull(year) %>%
    min()
    
  historical_start_date <- utilities::create_datetime(historical_start_year, 1)

  clean_transactions_df %>%
    summarise_categories(config_list, historical_start_date, time_vec, include_outlier = TRUE) %>%
    aggregate_categories(config_list[["aggregate_categories"]], "total", time_vec) %>%
    aggregate_categories(config_list[["meta_categories"]], "total", time_vec)
}

#' Create current financial account projections
#'
#' @param historical_transactions_df aggregated and cleaned transactions
#' @param accounts_df aggregated and cleaned accounts
#' @param config_list user config data
#' @param forecast_date_range tuple of start and end date
#'
#'
#' @export
#'
create_projections_df <- function(historical_transactions_df,
                                  accounts_df,
                                  config_list,
                                  forecast_date_range) {
  projection_df <- tibble("timestamp" = seq(min(forecast_date_range),
    max(forecast_date_range),
    by = "month"
  ))

  starting_accounts_df <- accounts_df %>%
    mutate("timestamp" = min(forecast_date_range)) %>%
    dplyr::filter(total != 0)

  historical_avg_spend <- historical_transactions_df %>%
    dplyr::filter(category %in% c("discretionary", "groceries")) %>%
    group_by(year, month) %>%
    summarise(total = sum(total)) %>%
    ungroup() %>%
    summarise(total = mean(total)) %>%
    pull()

  financial_config <- config_list[["current_financial_params"]]

  annual_income <- financial_config[["current_salary"]] * (1 - financial_config[["tax_rate"]])
  net_income <- annual_income - financial_config[["contribution_401k"]] - financial_config[["insurance"]]

  # TODO get bills from historical, need to add as category
  projection_df %>%
    mutate(
      housing_bills = financial_config[["housing"]] + financial_config[["bills"]],
      spend = historical_avg_spend + housing_bills,
      income = net_income / 12,
      additional_savings = cumsum(spend + income),
      starting_savings = starting_accounts_df %>% dplyr::filter(account_type == "bank") %>% pull(total),
      starting_investment = starting_accounts_df %>% dplyr::filter(account_type == "investment") %>% pull(total),
      total_savings = starting_savings + additional_savings,
      monthly_contribution = (financial_config[["contribution_401k"]] +
        financial_config[["contribution_ira"]]) / 12,
      monthly_contribution = monthly_contribution + ((financial_config[["current_salary"]] *
        financial_config[["employer_match"]]) / 12),
      investment_growth = (1 + (financial_config[["annual_investment_growth"]] / 12))^row_number(),
      total_investment = (starting_investment + cumsum(monthly_contribution))^investment_growth,
      net_worth = total_savings + total_investment,
    )
}