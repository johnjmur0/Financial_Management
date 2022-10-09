source("./.Rprofile")

plumber::pr_run(plumber::pr("./financial_management_api.R"), port = 8000)