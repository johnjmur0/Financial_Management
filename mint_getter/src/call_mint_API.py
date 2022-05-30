import os
from unicodedata import name
import pandas as pd
import mintapi
import json

def get_config(user_name: str):

    config_dir = os.path('../../config_files')
    config_file = os.path.join(config_dir, + user_name + '_config_.json')

    json_file = ''
    try:
        json_file = open(config_file)
    except FileNotFoundError:
        raise FileNotFoundError(f'User config for user name {config_file}. Does not exist. Please create it')

    user_config_data = json.load(json_file)
    return user_config_data

#https://github.com/mintapi/mintapi
def get_mint_conn(user_config: dict):
    
    return mintapi.Mint(
        user_config['mint_login']['login_email'],
        user_config['mint_login']['password'],
        mfa_method='soft-token',
        mfa_token = user_config['mint_login']['mfa_token'],
        headless=False)

def close_mint_conn(mint_conn):
    mint_conn.close()

def get_accounts_df(mint_conn):
    
    accounts = mint_conn.get_accounts()

    account_df = pd.DataFrame()

    for account in accounts:
        account_df = pd.concat([account_df, pd.DataFrame.from_dict(account, orient = "index").T])
    
    account_df = account_df[['accountName', 'accountType', 'accountSystemStatus', 'value', 'interestRate']].reset_index(drop=True)
    return account_df

def get_transactions_df(mint_conn):
    
    transactions = mint_conn.get_transactions()
    return transactions

def get_investments_df(mint_conn):
    
    investments = mint_conn.get_invests_json()

    investment_obj = json.loads(investments) 
    return pd.DataFrame.from_dict(investment_obj)

if __name__ == "__main__":

    user_config = get_config('jjm')
    mint_conn = get_mint_conn(user_config)
    get_accounts_df(mint_conn)