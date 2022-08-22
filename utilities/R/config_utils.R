#' obtain user config_file
#'
#' @param user_name user name indicating config file
#'
#' @return list
#' @import rjson 
#' @import stringr
#' @export
#'
get_user_config = function(user_name) {
  config_path = file.path('./config_files', stringr::str_c(user_name, '_config.json'))
  config_file = rjson::fromJSON(file = config_path)
}