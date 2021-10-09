#Example command that worked for reference
#"python C:/source/mintapi/mintapi/api.py --keyring --headless --session-path=None --mfa-method=soft-token --mfa-token=Q5ISNFBVWXM5FEJ5KKUI2WUXRSHZT5PS  johnjmur0@gmail.com --accounts --filename=C:/Users/JackMurphy/Downloads/Mint/accounts_10-03-2021_2.json"

get_mint_datasets = function(config_file)
{
  #TODO Have program manage this
  api_filePath = "C:/source/mintapi/mintapi/api.py"
  login_params = "--keyring --headless --session-path=None"
  mfa_params = str_c("--mfa-method=soft-token --mfa-token=", config_file[["MFA_Token"]])
  login_email = config_file[["Login_Email"]]
  
  date = format(as.Date(Sys.time(), tz = Sys.timezone()), format="%m-%d-%Y")
  #Delete files once they're read
  dir = "C:/temp"
  data_filePaths = tibble("Param" = c("--accounts", "--investments", "--transactions"),
                          "FileName" = c(file.path(dir, str_c("accounts_", date, ".json")), 
                                         file.path(dir, str_c("investments_", date, ".json")), 
                                         file.path(dir, str_c("transactions_", date, ".csv"))))
  
  shell_cmd = str_c("python", api_filePath, login_params, mfa_params, login_email, sep = " ")
  
  ret_data = lapply(data_filePaths %>% df_to_list(), function(data_request, shell_cmd) {
    
    print (str_c("Processing command", shell_cmd, sep = " "))
    shell_cmd = str_c(shell_cmd, data_request[["Param"]], str_c("--filename=", data_request[["FileName"]]), sep = " ")
    
    result = shell(shell_cmd)
    
    if (result != 0)
    {
      stop(str_c("Getting", data_request[["Param"]], "from mint api failed. Not sure how to get error message yet"))
    }
    
    if (str_detect(data_request[['FileName']], ".json")) {
      ret = rjson::fromJSON(file = data_request[['FileName']])
    }
    else if (str_detect(data_request[['FileName']], ".csv")) {
      ret = readr::read_csv(data_request[['FileName']])
    }
    else {
      stop(str_c("file format not expected"), data_request[['FileName']], sep = " ")
    }
  },
  shell_cmd=shell_cmd)
  
  return (ret_data)
}
