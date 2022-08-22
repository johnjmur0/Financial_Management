#' Lapply but re-attach named list entries
#'
#' @param list list
#' @param fun function
#'
#' @import dplyr
#' @export
#'
lapply_preserve_names = function(list, fun) {
  
  lapply(seq_along(list), function(i) {
    obj = list[i]
    names(obj) = names(list)[i]
    fun(obj)
  })
}

#' split dataframe row wise for iteration
#'
#' @param df data.frame or tibble
#'
#' @return list
#' @import dplyr
#' @export
#'
df_to_list = function(df) {
  
  . <- NULL
  return(df %>% split(seq_len(nrow(.))))
}

#' create datetime from year month
#'
#' @param year numeric
#' @param month numeric 
#'
#' @return lubridate datetime object
#' @export
#'
create_datetime = function(year, month) {
  lubridate::ymd(paste(year, month, 1, sep = "-"))
}

#' check that dataframe column has data of specified type
#'
#' @param expected_type expected class of data in column
#' @param df data.frame
#' @param column column name to inspect
#'
#' @return NULL
#' @export
#'
df_col_has_value = function(df, column, expected_type) {
  
  if (is.null(df[[column]]) || length(df[[column]]) == 0 || is.na(df[[column]])) {
    stop(stringr::str_c("provided data frame column", column, "was null, na, or empty", sep = " "))
  }
  
  if (class(df[[column]]) != expected_type) {
    stop(stringr::str_c("provided data frame column did not equal expected class. Provided", 
               class(df[[column]]), "but expected", expected_type, sep = " "))
  }
}