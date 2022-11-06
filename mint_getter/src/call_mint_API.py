import os
from unicodedata import name
import pandas as pd
import mintapi
import json

from webdriver_manager.firefox import GeckoDriverManager
from seleniumrequests import Firefox


def get_user_config(user_name: str):

    # TODO find cleaner way to do this
    config_dir = os.path.join(
        os.path.dirname(os.path.dirname(os.path.dirname(__file__))), "config_files"
    )
    config_file = os.path.join(config_dir, user_name + "_config.json")

    json_file = ""
    try:
        json_file = open(config_file)
    except FileNotFoundError:
        raise FileNotFoundError(
            f"User config for user name {config_file}. Does not exist. Please create it"
        )

    user_config_data = json.load(json_file)
    return user_config_data


# https://github.com/mintapi/mintapi
def get_mint_conn(user_config: dict):

    driver = Firefox(executable_path=GeckoDriverManager().install())

    return mintapi.Mint(
        email=user_config["mint_login"]["login_email"],
        password=user_config["mint_login"]["password"],
        mfa_method="soft-token",
        mfa_token=user_config["mint_login"]["mfa_token"],
        headless=False,
        use_chromedriver_on_path=False,
        wait_for_sync_timeout=300,
        driver=driver,
    )


def close_mint_conn(mint_conn):
    mint_conn.close()


def get_accounts_df(mint_conn):

    accounts = mint_conn.get_account_data()

    account_df = pd.DataFrame()

    for account in accounts:
        account_df = pd.concat(
            [account_df, pd.DataFrame.from_dict(account, orient="index").T]
        )

    ret_cols = ["name", "type", "systemStatus", "currentBalance", "availableBalance"]
    account_df = account_df[ret_cols].reset_index(drop=True)
    return account_df


def get_transactions_df(mint_conn):

    transactions = mint_conn.get_transaction_data(limit = 1000000)

    transactions_df = pd.DataFrame()

    for transaction in transactions:
        transactions_df = pd.concat(
            [transactions_df, pd.DataFrame.from_dict(transaction, orient="index").T]
        )

    ret_cols = ["date", "description", "amount", "type", "category", "accountId"]
    transactions_df = transactions_df[ret_cols].reset_index(drop=True)
    return transactions_df


def get_investments_df(mint_conn):

    investments = mint_conn.get_investment_data()

    investment_obj = json.loads(investments)
    return pd.DataFrame.from_dict(investment_obj)
