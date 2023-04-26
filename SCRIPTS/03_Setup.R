#_____________________________________________________________________________
### Survey level analysis setup ----
#_____________________________________________________________________________

#.............................................................................
#### Prepare analysis data ----
#.............................................................................


# Define which variables to keep for analysis to save memory - and vars to transform to labels
label_var_vec <- c("SEX","GOVTOF","ILODEFR","BENFTS","INDS07M",
<<<<<<< HEAD
                   "lev_quals","RELIG11")
=======
                   "RELIG11")
>>>>>>> make_output
analysis_var_vec <- c("parent","fam_id","AGE","adult1664","weight_val",
                      "HSERIALP","employed","london_resident","manchester_resident","birmingham_resident",
                      "inactive","unemployed","disability",
                      "age_group","famtype","wfh_d","child_age","ethnicity","pt_d","num_children")

# Replace variables with their value labels, then remove all value labels from the datasets to allow easy mutation of variables
dataset_list_adj <- lapply(dataset_list,align_vars) %>% 
  lapply(convert_to_label,var_vec=label_var_vec) %>% 
  lapply(haven::zap_labels) %>% 
  lapply(recode_dta) %>% 
  lapply(select,c(analysis_var_vec,paste(label_var_vec,"_label",sep="")))

# To save space, remove original dataset list
#rm(dataset_list)

#.............................................................................
#### Build survey design ----
#.............................................................................

# Set up survey design within list
## The id variable is household, where clustering happens in survey
## Strata is lowest possible geography, for EUL data is region
## nest set to TRUE to detect if primary sampling units appear in multiple strate (they should not)
survey_design_full <- lapply(X=dataset_list_adj,
                             FUN= function(dta,id,strata,weights,nest) 
                               svydesign(data=dta,id=id,strata=strata,weights=weights,nest=nest),
                             id   = ~HSERIALP, #clustered at household, NOT family unit
                             strata = ~GOVTOF_label,#lowest possible geo 
                             weights = ~weight_val, 
                             nest  = TRUE)

# Subset to only adults (aged 16+) - do within survey package to maintain error calculation
survey_design_adults <- lapply(survey_design_full, subset, adult1664 == 1)

# Remove full survey design for memory savings
rm(survey_design_full)
