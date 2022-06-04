#TODO It'd be nice to have this be a class, cut down on dupe code
get_mint_module = function() {
    
    dir = here::here()
    #TODO This always fails first time with rpytools error, but always works second time. Figure out
    reticulate::import_from_path(path = file.path(dirname(dir), 'mint_getter/src'), module = 'call_mint_API')
}

get_user_config = function(mint_module, user_name) {
    print(user_name)
    mint_module[['get_user_config']](user_name)
}

get_mint_connection = function(mint_module, user_config) {

    mint_module[['get_mint_conn']](user_config)
}

close_mint_connection = function() {

    mint_module = mint_module_memoised()
    mint_conn = mint_conn_memoised(mint_module)
    mint_module[['close_mint_conn']](mint_conn)
}

mint_module_memoised = memoise::memoise(get_mint_module)
mint_conn_memoised = memoise::memoise(get_mint_connection)
get_user_config_memoised = memoise::memoise(get_user_config)

check_cache = function(file_name) {

    result = tryCatch({
            
        return(readr::read_csv(file.path('./temp_cache', file_name)))
        
    }, error = function(e) {
        
        print('Reading from cache failed, reading from mint.')
        return(dplyr::tibble())      
    })
}

get_mint_data_generic = function(function_name, user_name, read_cache, write_cache) {

    file_name = stringr::str_c(stringr::str_remove(function_name, 'get_'), '.csv')
    
    if (read_cache) {

        cache_df = check_cache(file_name)
        
        if (length(cache_df) > 0) {
            return(cache_df)
        }
    }
    
    mint_module = mint_module_memoised()
    print(mint_module)
    user_config = get_user_config_memoised(mint_module, user_name)
    mint_conn = mint_conn_memoised(mint_module, user_config)
    
    ret_df = mint_module[[function_name]](mint_conn) %>% as_tibble()

    if (write_cache) {
        ret_df %>% readr::write_csv(file.path('./temp_cache', file_name))
    }

    return(ret_df)
}

get_mint_transactions = function(user_name, read_cache = FALSE, write_cache = FALSE) {
    
    func_name = 'get_transactions_df'

    return(get_mint_data_generic(func_name, user_name, read_cache, write_cache))
}

get_mint_accounts = function(user_name, read_cache = FALSE, write_cache = FALSE) {

    func_name = 'get_accounts_df'

    return(get_mint_data_generic(func_name, user_name, read_cache, write_cache))
}

get_mint_investments = function(user_name, read_cache = FALSE, write_cache = FALSE) {

    func_name = 'get_investments_df'

    return(get_mint_data_generic(func_name, user_name, read_cache, write_cache))
}

get_mint_data_by_type = function(data_name, user_name, read_cache, write_cache) {

    switch(data_name,

        transactions = {
            return(get_mint_transactions(user_name, read_cache, write_cache))
        },
        accounts = {
            return(get_mint_accounts(user_name, read_cache, write_cache)) 
        },
        investments = {
            return(get_mint_investments(user_name, read_cache, write_cache))
        },
        
        stop(str_c('Provided data_name', data_name, 'not supported.', sep = ' '))
    )
}

#' Get mint data by type memoised
#'
#' @param data_name name of data to get in switch statement 
#' @param user_name user name for config file 
#' @param read_cache bool 
#' @param write_cache bool
#'
#' @export
#'
get_mint_data_by_type_memoised = memoise::memoise(get_mint_data_by_type)