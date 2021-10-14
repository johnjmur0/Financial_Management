#TODO put this into Utils package

#' split dataframe row wise for iteration
#'
#' @param df data.frame or tibble
#'
#' @return list
#' @export
#'
#' @examples
df_to_list = function(df)
{
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
#' @examples
create_datetime = function(year, month)
{
  lubridate::ymd(paste(year, month, 1, sep = "-"))
}


#' check that dataframe column has data of specified type
#'
#' @param expected_type expected class of data in column
#' @param df data.frame
#' @param column column name to inspect
#'
#' @return
#' @export
#'
#' @examples
df_col_has_value = function(df, column, expected_type)
{
  if(is.null(df[[column]]) || length(df[[column]]) == 0  || is.na(df[[column]])) {
    stop("provided data frame column was null, na, or empty")
  }
  
  if (class(df[[column]]) != expected_type) {
    stop(str_c("provided data frame column did not equal expected class. Provided", 
               class(df[[column]]), "but expected", expected_type, sep=" "))
  }
}