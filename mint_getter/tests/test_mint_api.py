import pytest

from src import call_mint_API

class Test_Mint_API():

    def get_config_test(self):

        test_config = call_mint_API.get_config('jjm')
        