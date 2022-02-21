import json
import pandas as pd
import mintapi
import gspread
import df2gspread as d2g
from oauth2client.service_account import ServiceAccountCredentials

mint = mintapi.Mint(
    "johnjmur0@gmail.com",  # Email used to log in to Mint
    "w3Sa40pT$6XV",  # Your password used to log in to mint
 
    # Optional parameters
    mfa_method='soft-token',  # Can be 'sms' (default), 'email', or 'soft-token'.
                       # if mintapi detects an MFA request, it will trigger the requested method
                       # and prompt on the command line.
    mfa_token ='Q5ISNFBVWXM5FEJ5KKUI2WUXRSHZT5PS',
    headless=True,  # Whether the chromedriver should work without opening a
                     # visible window (useful for server-side deployments)
    mfa_input_callback=None,  # A callback accepting a single argument (the prompt)
                              # which returns the user-inputted 2FA code. By default
                              # the default Python `input` function is used.
    session_path=None, # Directory that the Chrome persistent session will be written/read from.
                       # To avoid the 2FA code being asked for multiple times, you can either set
                       # this parameter or log in by hand in Chrome under the same user this runs
                       # as.
    imap_account=None, # account name used to log in to your IMAP server
    imap_password=None, # account password used to log in to your IMAP server
    imap_server=None,  # IMAP server host name
    imap_folder='INBOX',  # IMAP folder that receives MFA email
    wait_for_sync=False,  # do not wait for accounts to sync
    wait_for_sync_timeout=30000,  # number of seconds to wait for sync
    use_chromedriver_on_path=False
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

mint.close()

account_df = get_account_df()
transactions_df = get_transactions_df()
investments_df = get_investments_df()