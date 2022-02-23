# Run All
#library(profvis)


rm(list = ls())


run_all_rmd <- function(filename) {
  tempR <- tempfile(fileext = ".R")
  knitr::purl(filename, output=tempR)
  source(tempR , echo = TRUE)
  unlink(tempR)
}


# Run Sim

run_all_rmd("Gen Test Data.Rmd")




source("IsDB Projects & LoF DM.R")


file.copy("Inputs/isdb_test_prjs.csv", "Inputs/isdb_test_prjs_sim.csv" , overwrite = T )  
file.copy("Outputs/model_output.rda" , "Outputs/model_output_sim.rda",  overwrite = T)
file.copy("Outputs/full_disb_profile.rda" , "Outputs/full_disb_profile_sim.rda", overwrite = T)


# Run Comp

run_all_rmd("Gen Test Comp Data.Rmd")

source("IsDB Projects & LoF DM.R")

file.copy("Inputs/isdb_test_prjs.csv", "Inputs/isdb_test_prjs_comp.csv" , overwrite = T) 
file.copy("Outputs/model_output.rda" , "Outputs/model_output_comp.rda" , overwrite = T)
file.copy("Outputs/full_disb_profile.rda" , "Outputs/full_disb_profile_comp.rda" , overwrite = T)





# Profiling

profvis(
  {
    source("IsDB Projects & LoF DM profiling.R")
  }
)


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
