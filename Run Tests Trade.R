# Run Test Trade

rm(list = ls())


source("run_all_rmd.R")


# Run Sim

run_all_rmd("Gen Trade Test Data.Rmd")

source("IsDB Trade Finance DM.R")

file.copy("Inputs/isdb_test_trade.csv", "Inputs/isdb_test_trade_sim.csv" , overwrite = T )  
file.copy("Outputs/model_trade_output.rda" , "Outputs/model_trade_output_sim.rda",  overwrite = T)
file.copy("Outputs/full_trade_disb_profile.rda" , "Outputs/full_trade_disb_profile_sim.rda", overwrite = T)



# Run Comp
source("run_all_rmd.R")
run_all_rmd("Gen Trade Test Comp Data.Rmd")

source("IsDB Trade Finance DM.R")

file.copy("Inputs/isdb_test_trade.csv", "Inputs/isdb_test_trade_comp.csv" , overwrite = T )  
file.copy("Outputs/model_trade_output.rda" , "Outputs/model_trade_output_comp.rda",  overwrite = T)
file.copy("Outputs/full_trade_disb_profile.rda" , "Outputs/full_trade_disb_profile_comp.rda", overwrite = T)


# Run Check 

source("run_all_rmd.R")
run_all_rmd("Gen Trade Test Check Data.Rmd")

source("IsDB Trade Finance DM.R")

file.copy("Inputs/isdb_test_trade.csv", "Inputs/isdb_test_trade_check.csv" , overwrite = T) 
file.copy("Outputs/model_trade_output.rda" , "Outputs/model_trade_output_check.rda" , overwrite = T)
file.copy("Outputs/full_trade_disb_profile.rda" , "Outputs/full_trade_disb_profile_check.rda" , overwrite = T)