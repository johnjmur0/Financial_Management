import pandas as pd
import mintapi
import json

#https://github.com/mintapi/mintapi
def get_mint_conn():
    return mintapi.Mint(
        #TODO secure these
        "johnjmur0@gmail.com",
        "w3Sa40pT$6XV",
        mfa_method='soft-token',
        mfa_token ='E6LDN5UVDKBQFODPBOTACIGHTMPGA6BH',
        headless=True)

def close_mint_conn(mint_conn):
    mint_conn.close()

def get_account_df(mint_conn):
    
    #mint = get_mint_conn()
    accounts = mint_conn.get_accounts(True)    
    #mint.close()

    account_df = pd.DataFrame()

    for account in accounts:
        account_df = pd.concat([account_df, pd.DataFrame.from_dict(account, orient = "index").T])
    
    account_df = account_df[['accountName', 'accountType', 'accountSystemStatus', 'value', 'interestRate']].reset_index(drop=True)
    return account_df

def get_transactions_df(mint_conn):
    
    #mint = get_mint_conn()
    transactions = mint_conn.get_transactions()
    #mint.close()
    return transactions

def get_investments_df(mint_conn):
    
    #mint = get_mint_conn()
    investments = mint_conn.get_invests_json()
    mint.close()

    investment_obj = json.loads(investments) 
    return pd.DataFrame.from_dict(investment_obj)