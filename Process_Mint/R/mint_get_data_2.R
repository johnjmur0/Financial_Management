
get_all_mint_data = function()
{    
    mint_module = reticulate::import_from_path(path = './Get_Mint', module = 'call_mint_API')
}