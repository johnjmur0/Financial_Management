
get_all_mint_data = function()
{    
    #TODO This always fails first time with rpytools error, but always works second time. Figure out
    mint_module = reticulate::import_from_path(path = './Get_Mint', module = 'call_mint_API')

    mint_conn = mint_module[['get_mint_conn']]()
    
    transactions = mint_module[['get_transactions_df']](mint_conn)
    accounts = mint_module[['get_account_df']](mint_conn)
    investments = mint_module[['get_investments_df']](mint_conn)


}