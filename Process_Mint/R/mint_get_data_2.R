
get_all_mint_data = function()
{    
    mint_module = reticulate::import_from_path(path = './Get_Mint', module = 'call_mint_API')

    transactions = mint_module$get_transactions_df()
    accounts = mint_module$get_account_df()
    investments = mint_module$get_investments_df()
}