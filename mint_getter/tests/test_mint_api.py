from src.call_mint_API import *


class Test_Mint_API:
    def test_get_config(self):

        test_config = get_user_config("jjm")

        assert len(test_config) > 0
