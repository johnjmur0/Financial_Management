process_accounts = function(accounts_file_path)
{
  accounts = rjson::fromJSON(file = accounts_file_path)
  
  lapply(accounts, function(account) {
    
    #Anything else to save here?
    tibble("Account_Name" = account[["accountName"]],
           "Account_Balance" = account[["value"]],
           "Account_Class" = account[["accountType"]],
           "Interest_Rate" = if_else(is.null(account[["interestRate"]]), 0,
                                             account[["interestRate"]]))
  
    }) %>% bind_rows()
}