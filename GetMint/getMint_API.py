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

accounts = mint.get_accounts(True)
investments = mint.get_invests_json()
transactions = mint.get_transactions()

mint.close()

account_df = pd.DataFrame()

for account in accounts:
    account_df = pd.concat([account_df, pd.DataFrame.from_dict(account, orient = "index").T])

account_df.to_csv("C:/Users/JackMurphy/Downloads/Mint/accounts_10-03-2021.csv")

transactions.to_csv("C:/Users/JackMurphy/Downloads/Mint/transactions_10-03-2021.csv")

investment_obj = json.loads(investments)
pd.DataFrame.from_dict(investment_obj).to_csv("C:/Users/JackMurphy/Downloads/Mint/investments_10-03-2021.csv")
