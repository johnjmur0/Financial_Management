#Get this from prompt?
userName = 'jjm'
overviewDf = Configuration::read_table(userName, "Overview")
actionsDf = Configuration::read_table(userName, "Actions")
adjustmentsDf = Configuration::read_table(userName, "Adjustments")
expensesDf = Configuration::read_table(userName, "Expenses")

