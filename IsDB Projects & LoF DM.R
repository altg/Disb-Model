# Disb Model - Project & LOF

{
  rm(list = ls())
  options(java.parameters = "-Xmx32g" )
  options(scipen = 999)
  setwd(dirname(rstudioapi::getSourceEditorContext()$path))
  
  library(data.table)   #Ignore the warning messages
  library(ggplot2)
  library(openxlsx)
  #library(xlsx)
  library(plyr)
  library(dplyr)
  library(readxl)
  library(svDialogs)      
  library(DescTools)      
  library(ExcelFunctionsR)
  
  theme_set(theme_bw(base_size = 12))
}

time_log <- Sys.time()



username <- "idb_test"


hs = list() #List in which plots are stored
hcount = 0

fd = list() #List in which Disbursement Profile Summary plot is stored
fdcount = 0

model_output <- {}      #To store Model Outputs of all projects
full_disb_profile <- {}     #To store Disbursement Profiles of all projects



### Setting directories

dir <- dirname(rstudioapi::getSourceEditorContext()$path)   #Updates the path to the directory where the R Code is present
input_dir <- paste0(dir, "/Inputs/")                        #Setting the path to the directory where the input files are present
output_dir <- paste0(dir, "/Outputs/")                      #Setting the path to the directory where the output files will be written
mappings_dir <- paste0(dir, "/Mappings/")                   #Setting the path to the directory where the mapping files are present



### Loading Model Input File

model_input <- fread(input = paste0(input_dir, "isdb_test_prjs.csv"),  stringsAsFactors = F)


#na.strings = "",

model_input$evaluation_date <- as.Date(model_input$evaluation_date, format = "%m/%d/%Y")
model_input$date_of_approval <- as.Date(model_input$date_of_approval, format = "%m/%d/%Y")
model_input$date_of_signature <- as.Date(model_input$date_of_signature, format = "%m/%d/%Y")
model_input$date_of_signature_override <- as.Date(model_input$date_of_signature_override, format = "%m/%d/%Y")
model_input$date_of_effectiveness <- as.Date(model_input$date_of_effectiveness, format = "%m/%d/%Y")
model_input$date_of_effectiveness_override <- as.Date(model_input$date_of_effectiveness_override, format = "%m/%d/%Y")
model_input$date_of_first_disbursement <- as.Date(model_input$date_of_first_disbursement, format = "%m/%d/%Y")
model_input$date_of_first_disbursement_override <- as.Date(model_input$date_of_first_disbursement_override, format = "%m/%d/%Y")
model_input$date_of_final_disbursement_override <- as.Date(model_input$date_of_final_disbursement_override, format = "%m/%d/%Y")



### Loading Regression Table & Mapping Files

app_sig_reg <- fread(input = paste0(mappings_dir, "Approval to Signature Regression Table.csv"), na.strings = "", stringsAsFactors = F)
sig_eff_reg <- fread(input = paste0(mappings_dir, "Signature to Effectiveness Regression Table.csv"), na.strings = "", stringsAsFactors = F)
eff_firstdisb_reg <- fread(input = paste0(mappings_dir, "Effectiveness to First Disbursement Regression Table.csv"), na.strings = "", stringsAsFactors = F)
first_finaldisb_reg <- fread(input = paste0(mappings_dir, "First to Final Disbursement Regression Table.csv"), na.strings = "", stringsAsFactors = F)

country_mapping <- fread(input = paste0(mappings_dir, "Country Mapping.csv"), na.strings = "", stringsAsFactors = F)
sector_mapping <- fread(input = paste0(mappings_dir, "Sector Mapping.csv"), na.strings = "", stringsAsFactors = F)
disb_profile_mapping <- fread(input = paste0(mappings_dir, "Disbursement Profile Mapping.csv"), na.strings = "", stringsAsFactors = F, header = T)




#### Disbursement Model ####

for (id in 1:nrow(model_input)) {     #One project at a time
  proj <- model_input[id,]
  
  print( paste0( id , ":" , proj$project_title) )
  
  date_of_evaluation <- proj$evaluation_date
  amount_disb_eval_date <- proj$amount_disbursed_at_evaluation_date_usd
  act_perc_disb_eval_date <- (amount_disb_eval_date/proj$approval_amount_usd) * 100
  date_of_approval <- proj$date_of_approval
  
  
  ### Approval to Signature Model
  
  if (is.na(proj$date_of_signature_override)) {     #Override check
    if (is.na(proj$date_of_signature)) {            #Date check
      intercept <- app_sig_reg[which(app_sig_reg$input == "Intercept"),]$value  #Intercept of the model
      
      # Calculating Coefficients
      
      country <- app_sig_reg[which(app_sig_reg$input == proj$country),]$value   
      profile <- app_sig_reg[which(app_sig_reg$input == proj$profile),]$value
      rating <- app_sig_reg[which(app_sig_reg$input == proj$rating),]$value
      
      approval_year <- ifelse(year(date_of_approval) <= 2020, app_sig_reg[which(app_sig_reg$input == year(date_of_approval)),]$value, 
                              (app_sig_reg[which(app_sig_reg$input == 2015),]$value +
                                 app_sig_reg[which(app_sig_reg$input == 2016),]$value +
                                 app_sig_reg[which(app_sig_reg$input == 2017),]$value +
                                 app_sig_reg[which(app_sig_reg$input == 2018),]$value)/4)
      
      reg_property <- proj$registering_property * app_sig_reg[which(app_sig_reg$input == "Registering Property"),]$value
      gov_effectiveness <- proj$government_effectiveness * app_sig_reg[which(app_sig_reg$input == "Government Effectiveness"),]$value
      
      xbeta <- intercept + country + profile + rating + approval_year + reg_property + gov_effectiveness    #Calculating xbeta
      
      sigest <- app_sig_reg[which(app_sig_reg$input == "Sigma"),]$value       #Sigma of the model
      ratio <- app_sig_reg[which(app_sig_reg$input == "Adjustment Ratio"),]$value     #Adjustment Ratio of the model
      
      time_after_event <- ifelse(as.numeric(date_of_evaluation - proj$date_of_approval)>0, as.numeric(date_of_evaluation - proj$date_of_approval), 0)    #Calculating time after event
      t <- time_after_event
      
      # Calculating remaining time
      
      remaining_time <- ((integrate(function(x) (1/
                                                   (1/
                                                      (
                                                        1 + ((t/exp(xbeta)) ^ (1/sigest))
                                                      )
                                                   )
      ) * 
        
        1/
        (
          1 + ((x/exp(xbeta)) ^ (1/sigest))
        ), lower = t, upper = Inf, subdivisions = 20000)$value)/(gamma(x = 1 + sigest) * gamma(x = 1 - sigest))) * ratio
      
      days_from_app_to_sig <- round(t + remaining_time,0)   #Calculating days
      
      
      if (!is.na(proj$days_from_approval_to_signature_cap)) {     #Applying cap, if required
        days_from_app_to_sig <- ifelse(proj$days_from_approval_to_signature_cap > days_from_app_to_sig, days_from_app_to_sig, proj$days_from_approval_to_signature_cap)
        
      }
      
      date_of_signature <- date_of_approval + days_from_app_to_sig       #Calculating date
      
    } else  {
      date_of_signature <- proj$date_of_signature
      days_from_app_to_sig <- as.numeric(date_of_signature - date_of_approval)
      
    }
    
  } else  {
    date_of_signature <- proj$date_of_signature_override
    days_from_app_to_sig <- as.numeric(date_of_signature - date_of_approval)
    
  }
  
  
  
  ### Signature to Effectiveness Model
  
  if (is.na(proj$date_of_effectiveness_override)) {     #Override check
    if (is.na(proj$date_of_effectiveness)) {            #Date check
      intercept <- sig_eff_reg[which(sig_eff_reg$input == "Intercept"),]$value  #Intercept of the model
      
      # Calculating Coefficients
      
      country <- sig_eff_reg[which(sig_eff_reg$input == proj$country),]$value   
      sub_mode_of_finance <- sig_eff_reg[which(sig_eff_reg$input == proj$sub_mode_of_finance),]$value
      rating <- sig_eff_reg[which(sig_eff_reg$input == proj$rating),]$value
      sector <- sig_eff_reg[which(sig_eff_reg$input == proj$sector),]$value
      income_classification <- sig_eff_reg[which(sig_eff_reg$input == proj$income_classification),]$value
      approval_amount_usd <- log(proj$approval_amount_usd) * sig_eff_reg[which(sig_eff_reg$input == "Approval Amount"),]$value
      
      xbeta <- intercept + country + sub_mode_of_finance + rating + sector + income_classification + approval_amount_usd    #Calculating xbeta
      
      sigest <- sig_eff_reg[which(sig_eff_reg$input == "Sigma"),]$value       #Sigma of the model
      ratio <- sig_eff_reg[which(sig_eff_reg$input == "Adjustment Ratio"),]$value     #Adjustment Ratio of the model
      
      time_after_event <- ifelse(is.na(proj$date_of_signature), 0, 
                                 ifelse(as.numeric(date_of_evaluation - proj$date_of_signature)>0, as.numeric(date_of_evaluation - proj$date_of_signature), 0))   #Calculating time after event
      
      t <- time_after_event
      
      # Calculating remaining time
      
      remaining_time <- ((integrate(function(x) (1/
                                                   (1/
                                                      (
                                                        1 + ((t/exp(xbeta)) ^ (1/sigest))
                                                      )
                                                   )
      ) * 
        
        1/
        (
          1 + ((x/exp(xbeta)) ^ (1/sigest))
        ), lower = t, upper = Inf, subdivisions = 20000)$value)/(gamma(x = 1 + sigest) * gamma(x = 1 - sigest))) * ratio
      
      days_from_sig_to_eff <- round(t + remaining_time,0)   #Calculating days
      
      
      if (!is.na(proj$days_from_signature_to_effectiveness_cap)) {     #Applying cap, if required
        days_from_sig_to_eff <- ifelse(proj$days_from_signature_to_effectiveness_cap > days_from_sig_to_eff, days_from_sig_to_eff, proj$days_from_signature_to_effectiveness_cap)
        
      }
      
      date_of_effectiveness <- date_of_signature + days_from_sig_to_eff       #Calculating date
      
    } else  {
      date_of_effectiveness <- proj$date_of_effectiveness
      days_from_sig_to_eff <- as.numeric(date_of_effectiveness - date_of_signature)
      
    }
    
  } else  {
    date_of_effectiveness <- proj$date_of_effectiveness_override
    days_from_sig_to_eff <- as.numeric(date_of_effectiveness - date_of_signature)
    
  }
  
  
  
  ### Effectiveness to First Disbursement Model
  
  if (is.na(proj$date_of_first_disbursement_override)) {     #Override check
    if (is.na(proj$date_of_first_disbursement)) {            #Date check
      intercept <- eff_firstdisb_reg[which(eff_firstdisb_reg$input == "Intercept"),]$value  #Intercept of the model
      
      # Calculating Coefficients
      
      country <- eff_firstdisb_reg[which(eff_firstdisb_reg$input == proj$country),]$value
      profile <- ifelse(proj$profile == "Line of Finance", eff_firstdisb_reg[which(eff_firstdisb_reg$input == "Profile Line of Finance"),]$value,
                        eff_firstdisb_reg[which(eff_firstdisb_reg$input == proj$profile),]$value)
      sub_mode_of_finance <- ifelse(proj$sub_mode_of_finance == "Line of Finance", eff_firstdisb_reg[which(eff_firstdisb_reg$input == "Sub Mode of Finance Line of Finance"),]$value,
                                    eff_firstdisb_reg[which(eff_firstdisb_reg$input == proj$sub_mode_of_finance),]$value)
      rating <- eff_firstdisb_reg[which(eff_firstdisb_reg$input == proj$rating),]$value
      sector <- eff_firstdisb_reg[which(eff_firstdisb_reg$input == proj$sector),]$value
      
      effectiveness_year <- ifelse(year(date_of_effectiveness) <= 2020, eff_firstdisb_reg[which(eff_firstdisb_reg$input == year(date_of_effectiveness)),]$value, 
                                   (eff_firstdisb_reg[which(eff_firstdisb_reg$input == 2015),]$value +
                                      eff_firstdisb_reg[which(eff_firstdisb_reg$input == 2016),]$value +
                                      eff_firstdisb_reg[which(eff_firstdisb_reg$input == 2017),]$value +
                                      eff_firstdisb_reg[which(eff_firstdisb_reg$input == 2018),]$value)/4)
      
      approval_amount_usd <- log(proj$approval_amount_usd) * eff_firstdisb_reg[which(eff_firstdisb_reg$input == "Approval Amount"),]$value
      days_from_signature_to_effectiveness <- log(days_from_sig_to_eff) * eff_firstdisb_reg[which(eff_firstdisb_reg$input == "Days from Signature to Effectiveness"),]$value
      
      xbeta <- intercept + country + profile + sub_mode_of_finance + rating + sector + effectiveness_year + approval_amount_usd + days_from_signature_to_effectiveness    #Calculating xbeta
      
      sigest <- eff_firstdisb_reg[which(eff_firstdisb_reg$input == "Sigma"),]$value       #Sigma of the model
      ratio <- eff_firstdisb_reg[which(eff_firstdisb_reg$input == "Adjustment Ratio"),]$value     #Adjustment Ratio of the model
      
      time_after_event <- ifelse(is.na(proj$date_of_effectiveness), 0, 
                                 ifelse(as.numeric(date_of_evaluation - proj$date_of_effectiveness)>0, as.numeric(date_of_evaluation - proj$date_of_effectiveness), 0))   #Calculating time after event
      
      t <- time_after_event
      
      # Calculating remaining time
      
      remaining_time <- ((integrate(function(x) (1/
                                                   (1/
                                                      (
                                                        1 + ((t/exp(xbeta)) ^ (1/sigest))
                                                      )
                                                   )
      ) * 
        
        1/
        (
          1 + ((x/exp(xbeta)) ^ (1/sigest))
        ), lower = t, upper = Inf, subdivisions = 20000)$value)/(gamma(x = 1 + sigest) * gamma(x = 1 - sigest))) * ratio
      
      days_from_eff_to_firstdisb <- round(t + remaining_time,0)   #Calculating days
      
      
      if (!is.na(proj$days_from_effectiveness_to_first_disbursement_cap)) {     #Applying cap, if required
        days_from_eff_to_firstdisb <- ifelse(proj$days_from_effectiveness_to_first_disbursement_cap > days_from_eff_to_firstdisb, days_from_eff_to_firstdisb, proj$days_from_effectiveness_to_first_disbursement_cap)
        
      }
      
      date_of_first_disbursement <- date_of_effectiveness + days_from_eff_to_firstdisb       #Calculating date
      
    } else  {
      date_of_first_disbursement <- proj$date_of_first_disbursement
      days_from_eff_to_firstdisb <- as.numeric(date_of_first_disbursement - date_of_effectiveness)
      
    }
    
  } else  {
    date_of_first_disbursement <- proj$date_of_first_disbursement_override
    days_from_eff_to_firstdisb <- as.numeric(date_of_first_disbursement - date_of_effectiveness)
    
  }
  
  
  days_from_app_to_firstdisb <- as.numeric(date_of_first_disbursement - date_of_approval)     #Days from Approval to First Disbursement
  
  
  
  ### First to Final Disbursement Model
  
  if (is.na(proj$date_of_final_disbursement_override)) {     #Override check
    intercept <- first_finaldisb_reg[which(first_finaldisb_reg$input == "Intercept"),]$value  #Intercept of the model
    
    # Calculating Coefficients
    
    country <- first_finaldisb_reg[which(first_finaldisb_reg$input == proj$country),]$value
    profile <- ifelse(proj$profile == "Line of Finance", first_finaldisb_reg[which(first_finaldisb_reg$input == "Profile Line of Finance"),]$value,
                      first_finaldisb_reg[which(first_finaldisb_reg$input == proj$profile),]$value)
    sub_mode_of_finance <- ifelse(proj$sub_mode_of_finance == "Line of Finance", first_finaldisb_reg[which(first_finaldisb_reg$input == "Sub Mode of Finance Line of Finance"),]$value,
                                  first_finaldisb_reg[which(first_finaldisb_reg$input == proj$sub_mode_of_finance),]$value)
    sector <- first_finaldisb_reg[which(first_finaldisb_reg$input == proj$sector),]$value
    
    first_disb_year <- ifelse(year(date_of_first_disbursement) <= 2020, first_finaldisb_reg[which(first_finaldisb_reg$input == year(date_of_first_disbursement)),]$value, 
                              (first_finaldisb_reg[which(first_finaldisb_reg$input == 2015),]$value +
                                 first_finaldisb_reg[which(first_finaldisb_reg$input == 2016),]$value +
                                 first_finaldisb_reg[which(first_finaldisb_reg$input == 2017),]$value +
                                 first_finaldisb_reg[which(first_finaldisb_reg$input == 2018),]$value)/4)
    
    approval_amount_usd <- log(proj$approval_amount_usd) * first_finaldisb_reg[which(first_finaldisb_reg$input == "Approval Amount"),]$value
    days_from_approval_to_signature <- log(days_from_app_to_sig) * first_finaldisb_reg[which(first_finaldisb_reg$input == "Days from Approval to Signature"),]$value
    
    xbeta <- intercept + country + profile + sub_mode_of_finance + sector + first_disb_year + approval_amount_usd + days_from_approval_to_signature    #Calculating xbeta
    
    ratio <- first_finaldisb_reg[which(first_finaldisb_reg$input == "Adjustment Ratio"),]$value     #Adjustment Ratio of the model
    
    days_from_first_to_finaldisb <- round(exp(xbeta) * ratio,0)   #Calculating days
    
    
    time_after_event <- ifelse(is.na(proj$date_of_first_disbursement), 0, 
                               ifelse(as.numeric(date_of_evaluation - proj$date_of_first_disbursement)>0, as.numeric(date_of_evaluation - date_of_first_disbursement),
                                      0))   #Calculating time after event
    
    reg_group <- country_mapping$region_group[match(proj$country, country_mapping$country)]     #Region group
    sec_group <- sector_mapping$sector_group[match(proj$sector, sector_mapping$sector)]     #Sector group
    coun_ldmc <- country_mapping$country_ldmc[match(proj$country, country_mapping$country)]     #Country LDMC
    profile <- paste0(reg_group, ", ", sec_group)     #Profile
    
    catchup_shift <- ifelse(tolower(proj$country) == "others", dlg_input(message = "Catch Up/Shift", default = "Catch Up", gui = .GUI)$res,
                            ifelse(time_after_event > days_from_first_to_finaldisb, "Shift", 
                                   ifelse(tolower(coun_ldmc) == "ldmc", "Shift", "Catch Up")))     #Catch Up/Shift
    
    applicability <- ifelse(is.na(proj$date_of_first_disbursement), "No",
                            ifelse(time_after_event != 0, "Yes",
                                   ifelse(is.na(amount_disb_eval_date), "No", "Yes")))        #Catch up/Shift applicable
    
  } else  {
    date_of_final_disbursement <- proj$date_of_final_disbursement_override      #Date of Final Disbursement
    days_from_first_to_finaldisb <- as.numeric(date_of_final_disbursement - date_of_first_disbursement)
    
    time_after_event <- ifelse(is.na(proj$date_of_first_disbursement), 0, 
                               ifelse(as.numeric(date_of_evaluation - proj$date_of_first_disbursement)>0, as.numeric(date_of_evaluation - date_of_first_disbursement),
                                      0))   #Calculating time after event
    
    reg_group <- country_mapping$region_group[match(proj$country, country_mapping$country)]     #Region group
    sec_group <- sector_mapping$sector_group[match(proj$sector, sector_mapping$sector)]     #Sector group
    coun_ldmc <- country_mapping$country_ldmc[match(proj$country, country_mapping$country)]     #Country LDMC
    profile <- paste0(reg_group, ", ", sec_group)     #Profile
    
    catchup_shift <- ifelse(time_after_event > days_from_first_to_finaldisb, "Shift", "Catch Up")     #Since there is an override, Shift shouldn't be applicable
    
    applicability <- ifelse(is.na(proj$date_of_first_disbursement), "No",
                            ifelse(time_after_event != 0, "Yes",
                                   ifelse(is.na(amount_disb_eval_date), "No", "Yes")))        #Catch up/Shift applicable
    
  }
  
  
  # Disbursement Profile
  
  if (applicability == "No") {        #If Catch up/Shift isn't applicable
    disb_profile <- t(disb_profile_mapping[which(disb_profile_mapping$Profile == profile), 2:ncol(disb_profile_mapping)])      #Disbursement Profile according to Profile
    disb_profile <- as.data.frame(disb_profile, stringsAsFactors = F)
    disb_profile$std_tenor <- as.numeric(row.names(disb_profile))
    disb_profile <- disb_profile[, c(2,1)]
    colnames(disb_profile)[2] <- "perc_disbursed"
    disb_profile$perc_disbursed <- as.numeric(disb_profile$perc_disbursed)
    
    
    disb_profile$days_from_first_disb <- round(disb_profile$std_tenor * days_from_first_to_finaldisb,0)     #Days from First Disbursement
    disb_profile$date_of_disbursement <- disb_profile$days_from_first_disb + date_of_first_disbursement     #Date of Disbursement
    disb_profile$cum_amount_disbursed <- (disb_profile$perc_disbursed/100) * proj$approval_amount_usd       #Cumulative Amount Disbursed
    
    for (i in 1:nrow(disb_profile)) {       #Amount Disbursed between Tenors
      if (i == 1) {
        disb_profile$amt_disb_btw_tenors[i] <- disb_profile$cum_amount_disbursed[i]
        
      } else  {
        disb_profile$amt_disb_btw_tenors[i] <- disb_profile$cum_amount_disbursed[i] - disb_profile$cum_amount_disbursed[i-1]
        
      }
      
    }
    
    disb_profile$project_id <- proj$project_id
    
    disb_profile <- disb_profile[, c(7, 1, 3:4, 2, 5:6)]     #Final Disbursement Profile
    
    date_of_final_disbursement <- date_of_first_disbursement + days_from_first_to_finaldisb      #Date of Final Disbursement
    
    
  } else if (catchup_shift == "Catch Up") {     #If Catch up is applicable
    disb_profile <- t(disb_profile_mapping[which(disb_profile_mapping$Profile == profile), 2:ncol(disb_profile_mapping)])      #Disbursement Profile according to Profile
    disb_profile <- as.data.frame(disb_profile, stringsAsFactors = F)
    disb_profile$std_tenor <- as.numeric(row.names(disb_profile))
    disb_profile <- disb_profile[, c(2,1)]
    colnames(disb_profile)[2] <- "perc_disbursed"
    disb_profile$perc_disbursed <- as.numeric(disb_profile$perc_disbursed)
    
    tenor_eval_date <- round(time_after_event/days_from_first_to_finaldisb, 2)      #Tenor corresponding to Evaluation Date
    perc_disb_prof_tenor <- disb_profile$perc_disbursed[match(tenor_eval_date, disb_profile$std_tenor)]     #Percentage Disbursed at Tenor according to Profile
    extra_perc_disb <- (perc_disb_prof_tenor - act_perc_disb_eval_date)/((1 - tenor_eval_date) * 100)     #Extra Percentage Disbursed to be added
    
    for (i in 1:nrow(disb_profile)) {       #Catch up Disbursement Profile
      disb_profile$perc_disb_catchup[i] <- ifelse(disb_profile$std_tenor[i] <= tenor_eval_date, act_perc_disb_eval_date,      
                                                  disb_profile$perc_disb_catchup[i-1] +(disb_profile$perc_disbursed[i] - disb_profile$perc_disbursed[i-1] + extra_perc_disb))
      
    }
    
    disb_profile <- disb_profile[, c(1,3)]
    colnames(disb_profile)[2] <- "perc_disbursed"
    
    disb_profile$days_from_first_disb <- round(disb_profile$std_tenor * days_from_first_to_finaldisb,0)     #Days from First Disbursement
    disb_profile$date_of_disbursement <- disb_profile$days_from_first_disb + date_of_first_disbursement     #Date of Disbursement
    disb_profile$cum_amount_disbursed <- (disb_profile$perc_disbursed/100) * proj$approval_amount_usd       #Cumulative Amount Disbursed
    
    for (i in 1:nrow(disb_profile)) {       #Amount Disbursed between Tenors
      if (i == 1) {
        disb_profile$amt_disb_btw_tenors[i] <- disb_profile$cum_amount_disbursed[i]
        
      } else  {
        disb_profile$amt_disb_btw_tenors[i] <- disb_profile$cum_amount_disbursed[i] - disb_profile$cum_amount_disbursed[i-1]
        
      }
      
    }
    
    disb_profile$project_id <- proj$project_id
    
    disb_profile <- disb_profile[, c(7, 1, 3:4, 2, 5:6)]     #Final Disbursement Profile
    
    date_of_final_disbursement <- date_of_first_disbursement + days_from_first_to_finaldisb      #Date of Final Disbursement
    
    
  } else  {     #If Shift is applicable
    disb_profile <- t(disb_profile_mapping[which(disb_profile_mapping$Profile == profile), 2:ncol(disb_profile_mapping)])      #Disbursement Profile according to Profile
    disb_profile <- as.data.frame(disb_profile, stringsAsFactors = F)
    disb_profile$std_tenor <- as.numeric(row.names(disb_profile))
    disb_profile <- disb_profile[, c(2,1)]
    colnames(disb_profile)[2] <- "perc_disbursed"
    disb_profile$perc_disbursed <- as.numeric(disb_profile$perc_disbursed)
    
    disb_profile$profile_days <- round(disb_profile$std_tenor * days_from_first_to_finaldisb,0)     #Days from First Disbursement according to Profile
    
    prof_days_act_disb <- IFNA(disb_profile$profile_days[match(act_perc_disb_eval_date, disb_profile$perc_disbursed)], 
                               ifelse(disb_profile$perc_disbursed[1] > act_perc_disb_eval_date, 0, 
                                      max(disb_profile[which(disb_profile$perc_disbursed < act_perc_disb_eval_date),]$profile_days) +
                                        round(0.01 * days_from_first_to_finaldisb)))    #Profile Days corresponding to Actual Percentage Disbursed
    
    tenor_prof_days <- disb_profile$std_tenor[match(prof_days_act_disb, disb_profile$profile_days)]     #Tenor corresponding to Profile Days
    days_lag <- time_after_event - prof_days_act_disb
    
    disb_profile$days_from_first_disb <- ifelse((disb_profile$profile_days + days_lag) < 0, 0,
                                                disb_profile$profile_days + days_lag)     #Actual Days from First Disbursement
    
    disb_profile$perc_disbursed_shift <- ifelse(disb_profile$std_tenor <= tenor_prof_days, act_perc_disb_eval_date, disb_profile$perc_disbursed)
    
    disb_profile <- disb_profile[, c(1, 4:5)]
    colnames(disb_profile)[3] <- "perc_disbursed"
    
    disb_profile$date_of_disbursement <- disb_profile$days_from_first_disb + date_of_first_disbursement     #Date of Disbursement
    disb_profile$cum_amount_disbursed <- (disb_profile$perc_disbursed/100) * proj$approval_amount_usd       #Cumulative Amount Disbursed
    
    for (i in 1:nrow(disb_profile)) {       #Amount Disbursed between Tenors
      if (i == 1) {
        disb_profile$amt_disb_btw_tenors[i] <- disb_profile$cum_amount_disbursed[i]
        
      } else  {
        disb_profile$amt_disb_btw_tenors[i] <- disb_profile$cum_amount_disbursed[i] - disb_profile$cum_amount_disbursed[i-1]
        
      }
      
    }
    
    disb_profile$project_id <- proj$project_id
    
    disb_profile <- disb_profile[, c(7, 1:2, 4, 3, 5:6)]     #Final Disbursement Profile
    
    days_from_first_to_finaldisb <- days_from_first_to_finaldisb + days_lag       #Days from First to Final Disbursement due to Shift
    date_of_final_disbursement <- date_of_first_disbursement + days_from_first_to_finaldisb      #Date of Final Disbursement
    
    
  }
  
  
  # Disbursement Profile Graphs
  
  # setDT(disb_profile)
  
  disb_profile_plot <- ggplot(disb_profile, aes(days_from_first_disb, perc_disbursed)) +
    geom_line(color = "salmon1", size = 0.8) +
    labs(x = "Days from First Disbursement",y = "Percentage Disbursed") +
    ggtitle("Projects and Line of Finance", subtitle = paste0("Disbursement Profile | Project ID: ", proj$project_id)) 
  
  print(disb_profile_plot)
  
  hcount = hcount + 1
  hs[[hcount]] = recordPlot() # write plot to list
  
  
  full_disb_profile <- rbind(full_disb_profile, disb_profile)     #Storing Disbursement Profile of each project
  
  # Storing Model Output
  
  model_output <- rbind(model_output, data.frame(proj$project_id, proj$project_title, proj$approval_amount_usd, date_of_evaluation, amount_disb_eval_date,
                                                 act_perc_disb_eval_date, date_of_approval, proj$days_from_approval_to_signature_cap, days_from_app_to_sig,
                                                 proj$date_of_signature_override, date_of_signature, proj$days_from_signature_to_effectiveness_cap, days_from_sig_to_eff,
                                                 proj$date_of_effectiveness_override, date_of_effectiveness, proj$days_from_effectiveness_to_first_disbursement_cap, 
                                                 days_from_eff_to_firstdisb, proj$date_of_first_disbursement_override, date_of_first_disbursement, days_from_app_to_firstdisb,
                                                 profile, catchup_shift, applicability, days_from_first_to_finaldisb, proj$date_of_final_disbursement_override, 
                                                 date_of_final_disbursement))
  
}



### Disbursement Profile Summary

time_gap <- 90        #Time Interval Duration for Disbursement Summary

summ_time_points <- seq(0, RoundTo(x = as.numeric(max(model_output$date_of_final_disbursement) - model_input$evaluation_date[1]), multiple = time_gap, FUN = ceiling), time_gap)

disb_summ <- {}

for (z in 1 : (length(summ_time_points) - 1)) {
  lb <- ifelse(z == 1, summ_time_points[z], summ_time_points[z] + 1)
  lb_date <- model_input$evaluation_date[1] + lb
  
  ub <- summ_time_points[z] + time_gap
  ub_date <- model_input$evaluation_date[1] + ub
  
  interval <- paste0(lb, "-", ub)
  
  amount <- sum(full_disb_profile[which(full_disb_profile$date_of_disbursement >= lb_date & full_disb_profile$date_of_disbursement <= ub_date),]$amt_disb_btw_tenors)
  
  disb_summ <- rbind(disb_summ, data.frame(model_input$evaluation_date[1], interval, amount))
  
}


disb_summ$interval <- factor(disb_summ$interval, levels = disb_summ$interval)


disb_profile_plot <- ggplot(disb_summ, aes(interval, amount)) +
  geom_col(fill = "skyblue4") +
  labs(x = "Days from Evaluation Date",y = "Total Amount Disbursed (USD)") +
  ggtitle("Projects and Line of Finance", subtitle = paste0("Disbursement Profile Summary | Evaluation Date: ", model_input$evaluation_date[1])) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 6))

print(disb_profile_plot)

fdcount = fdcount + 1
fd[[fdcount]] = recordPlot() # write plot to list



colnames(disb_summ) <- c("Evaluation Date", "Days from Evaluation Date", "Total Amount Disbursed (USD) in Time Interval")

colnames(full_disb_profile) <- c("Project ID", "Standard Tenor", "Days from First Disbursement", "Date of Disbursement", "Percentage Disbursed", 
                                 "Cumulative Amount Disbursed", "Amount Disbursed between Tenors")

colnames(model_output) <- c("Project ID", "Project Title", "Approval Amount (USD)", "Date of Evaluation", "Amount Disbursed at Evaluation Date (USD)", 
                            "Actual Percentage Disbursed at Evaluation Date", "Date of Approval", "Days from Approval to Signature Cap", "Days from Approval to Signature",
                            "Date of Signature Override", "Date of Signature", "Days from Signature to Effectiveness Cap", "Days from Signature to Effectiveness",
                            "Date of Effectiveness Override", "Date of Effectiveness", "Days from Effectiveness to First Disbursement Cap", 
                            "Days from Effectiveness to First Disbursement", "Date of First Disbursement Override", "Date of First Disbursement", "Days from Approval to First Disbursement",
                            "Profile", "Catch Up/Shift", "Catch Up/Shift Applicable", "Days from First to Final Disbursement", "Date of Final Disbursement Override",
                            "Date of Final Disbursement")




### Writing Output Files

# Model Outputs

write.csv(x = model_output, file = paste0(output_dir,"IsDB Projects & LoF Disbursement Modelling Outputs", " ", username, " ",format(time_log, "%d-%b-%Y %H.%M.%S"), ".csv"),
          na ="", row.names = F)


# Disbursement Profiles

write.csv(x = full_disb_profile, file = paste0(output_dir,"IsDB Projects & LoF Disbursement Modelling Profiles", " ", username, " ",format(time_log, "%d-%b-%Y %H.%M.%S"), ".csv"),
          na ="", row.names = F)


# Disbursement Summary

write.csv(x = disb_summ, file = paste0(output_dir,"IsDB Projects & LoF Disbursement Modelling Summary", " ", username, " ",format(time_log, "%d-%b-%Y %H.%M.%S"), ".csv"),
          na ="", row.names = F)


# Disbursement Profile Graphs

pdf(paste0(output_dir,"IsDB Projects & LoF Disbursement Modelling Graphs", " ", username, " ",format(time_log, "%d-%b-%Y %H.%M.%S"), ".pdf"), height = 11, width = 14) # pdf file to be written to
for (i in seq(length(fd)))
  print(replayPlot(fd[[i]])) # loop over plots and write to pdf
for (i in seq(length(hs)))
  print(replayPlot(hs[[i]])) # loop over plots and write to pdf
graphics.off()
