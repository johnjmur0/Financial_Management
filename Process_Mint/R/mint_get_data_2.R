#TODO It'd be nice to have this be a class, cut down on dupe code
get_mint_module = function() {
    
    #TODO This always fails first time with rpytools error, but always works second time. Figure out
    reticulate::import_from_path(path = './Get_Mint', module = 'call_mint_API')
}

get_mint_connection = function(mint_module) {

    mint_module[['get_mint_conn']]()
}

close_mint_connection = function() {

    mint_module = mint_module_memoised()
    mint_conn = mint_conn_memoised(mint_module)
    mint_module[['close_mint_conn']](mint_conn)
}

mint_module_memoised = memoise::memoise(get_mint_module)
mint_conn_memoised = memoise::memoise(get_mint_connection)

check_cache = function(file_name) {

    result = tryCatch({ 
        
        return(read_csv(file.path('./temp_cache', file_name)))
        
    }, error = function(e) {
        
        print('Reading from cache failed, reading from mint.')
        return(tibble())      
    })
}

get_mint_data_generic = function(file_name, function_name, read_cache, write_cache) {

    if (read_cache) {

        cache_df = check_cache(file_name)
        
        if (length(cache_df) > 0) {
            return(cache_df)
        }
    }
    
    mint_module = mint_module_memoised()
    mint_conn = mint_conn_memoised(mint_module)
    
    ret_df = mint_module[[function_name]](mint_conn) %>% as_tibble()

    if (write_cache) {
        ret_df %>% write_csv(file.path('./temp_cache', file_name))
    }

    return(ret_df)
}

get_mint_transactions = function(read_cache = FALSE, write_cache = FALSE) {
    
    file_name = 'transactions_df.csv'
    fun_name = 'get_transactions_df'

    return(get_mint_data_generic(file_name, fun_name, read_cache, write_cache))
}

get_mint_accounts = function(read_cache = FALSE, write_cache = FALSE) {

    file_name = 'accounts_df.csv'
    fun_name = 'get_account_df'

    return(get_mint_data_generic(file_name, fun_name, read_cache, write_cache))
}

get_mint_investments = function(read_cache = FALSE, write_cache = FALSE) {

    file_name = 'investments_df.csv'
    fun_name = 'get_investments_df'

    return(get_mint_data_generic(file_name, fun_name, read_cache, write_cache))
}