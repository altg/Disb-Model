# Run All
#library(profvis)


rm(list = ls())


source("run_all_rmd.R")


# Run Sim

run_all_rmd("Gen Test Data.Rmd")

source("IsDB Projects & LoF DM.R")


file.copy("Inputs/isdb_test_prjs.csv", "Inputs/isdb_test_prjs_sim.csv" , overwrite = T )  
file.copy("Outputs/model_output.rda" , "Outputs/model_output_sim.rda",  overwrite = T)
file.copy("Outputs/full_disb_profile.rda" , "Outputs/full_disb_profile_sim.rda", overwrite = T)


# Run Comp
source("run_all_rmd.R")

run_all_rmd("Gen Test Comp Data.Rmd")

source("IsDB Projects & LoF DM.R")

file.copy("Inputs/isdb_test_prjs.csv", "Inputs/isdb_test_prjs_comp.csv" , overwrite = T) 
file.copy("Outputs/model_output.rda" , "Outputs/model_output_comp.rda" , overwrite = T)
file.copy("Outputs/full_disb_profile.rda" , "Outputs/full_disb_profile_comp.rda" , overwrite = T)

file.info("Outputs/model_output_comp.rda")$mtime


# Run Check 

source("run_all_rmd.R")

run_all_rmd("Gen Test Check Data.Rmd")

source("IsDB Projects & LoF DM.R")

file.copy("Inputs/isdb_test_prjs.csv", "Inputs/isdb_test_prjs_check.csv" , overwrite = T) 
file.copy("Outputs/model_output.rda" , "Outputs/model_output_check.rda" , overwrite = T)
file.copy("Outputs/full_disb_profile.rda" , "Outputs/full_disb_profile_check.rda" , overwrite = T)

# Profiling

# profvis(
#   {
#     source("IsDB Projects & LoF DM profiling.R")
#   }
# )


# files <- c("Country DataPrep.Rmd" , 
#            "Operations DataPrep.Rmd",
#            "Disbursement DataPrep Base.Rmd", 
#            "Repayment DataPrep Base.Rmd",
#            "Disbursement DataPrep.Rmd",
#            "Repayment DataPrep.Rmd",
#            "Combined DataPrep.Rmd"
# )
# 
# lapply(files, run_all_rmd)
