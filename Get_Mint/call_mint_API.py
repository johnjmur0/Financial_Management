import gspread
import df2gspread as d2g
import json
import pandas as pd
import mintapi
from oauth2client.service_account import ServiceAccountCredentials

#https://github.com/mintapi/mintapi
mint = mintapi.Mint(
    #TODO secure these
    "johnjmur0@gmail.com",
    "w3Sa40pT$6XV",
    mfa_method='soft-token',
    mfa_token ='E6LDN5UVDKBQFODPBOTACIGHTMPGA6BH',
    headless=True
)

def get_account_df():
    accounts = mint.get_accounts(True)    

    account_df = pd.DataFrame()

    for account in accounts:
        account_df = pd.concat([account_df, pd.DataFrame.from_dict(account, orient = "index").T])

    return account_df

def get_transactions_df():
    transactions = mint.get_transactions()

    return transactions

def get_investments_df():

    investments = mint.get_invests_json()

    investment_obj = json.loads(investments) 
    return pd.DataFrame.from_dict(investment_obj)