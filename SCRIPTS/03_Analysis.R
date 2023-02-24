#_____________________________________________________________________________
### Survey level analysis ----
#_____________________________________________________________________________

#.............................................................................
#### Prepare analysis data ----
#.............................................................................


# Define which variables to keep for analysis to save memory - and vars to transform to labels
label_var_vec <- c("SEX","GOVTOF","ILODEFR","ETHUKEUL","FUTYPE6")
analysis_var_vec <- c("parent","fam_id","AGE","adult1664","weight_val","HSERIALP","employed","london_resident")

# Replace variables with their value labels, then remove all value labels from the datasets to allow easy mutation of variables
dataset_list_adj <- lapply(dataset_list,convert_to_label,var_vec=label_var_vec) %>% 
  lapply(haven::zap_labels) %>% 
  lapply(recode_dta) %>% 
  lapply(select,c(analysis_var_vec,label_var_vec,paste(label_var_vec,"_label",sep="")))


#.............................................................................
#### Summary statistics across characteristics ----
#.............................................................................



lfs_sum_list <- lapply(dataset_list_adj,collapse_func,demog_var="SEX_label")

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
survey_design_adults <- lapply(survey_design_full,subset,subset=adult1664==1)



employ_rates_list <- list()

perm_byvars <- c("parent","london_resident")
analysis_byvars <- list("all"=c(),
                        "ethnicity"=c("ETHUKEUL_label"),
                        "famtype"=c("FUTYPE6_label"),
                        "sex"=c("SEX_label"))

# The loop below calculates share of people employed by characteristic
## The parameters set the permanent demographic vars (parentage and residency)
## followed by optional other variables, with appropriate names
## The output is then saved in a list of lists, with top levels named by var set
## and bottom levels named by the dataset name (i.e. year of dataset)
for(i in 1:length(analysis_byvars)) {
  
  # Extract the variables needed
  byvars_vec <- c(perm_byvars,analysis_byvars[[i]])
  
  # Construct a formula object
  fom <- formula_helper(formula_vars = byvars_vec)
  
  # Insert output into list, with name of var set
  employ_rates_list[[paste0(names(analysis_byvars)[[i]])]] <- lapply(X=survey_design_adults,
                                                               FUN= function(design,formula,by,com,keep.var) 
                                                                 svyby(design=design,formula=formula,by=by,FUN=com,keep.var=keep.var),
                                                               formula = ~employed, 
                                                               by    = fom, 
                                                               com   = svymean, 
                                                               keep.var = TRUE)
}

# To access results of any one year and var set, refer to the list level
## e.g. employ_rates_list[[2]][[4]] is same as employ_rates_list[["ethnicity"]][["lfsh_aj_21"]]

#.............................................................................
#### Output statistics ----
#.............................................................................

# Delist the results and combine all into handy data frame for export
analysis_byvars_vec <- unname(unlist(analysis_byvars)) # due to odd behaviour from across(), need to remove names

employ_rates_df <- bind_rows(lapply(employ_rates_list,bind_rows,.id="dataset"),.id="var_set") %>%  
  unite("byvar_characteristic",all_of(analysis_byvars_vec), sep='_',na.rm = TRUE,remove = FALSE) %>% 
  mutate(id = paste0(var_set,"_",dataset,"_",parent,"_",london_resident,"_",byvar_characteristic)) %>% 
  relocate(id)

# Export into workbook with formatted sheets
wbname <- "Parental employment rates.xlsx"
wb <- loadWorkbook(paste0(DATA_OUT,wbname))

data_sheets <- c("emp_rates_data")

# Check if sheets exists and delete data, otherwise create sheet
for (sht in data_sheets) {
  
  # Check if exists
  for (actual_sheets in names(wb)){
    sht_exists <- FALSE
    if (actual_sheets == sht) {
      sht_exists <- TRUE
    }
  }
  
  # If not exist
  if (sht_exists != TRUE) {
    addWorksheet(wb, sht, tabColour = "black")
  } else {
    deleteData(wb , sheet = sht,cols = 1:20, rows = 1:10000, gridExpand = TRUE)
  }
  
}

# Write to workbook
writeData(wb, sheet = "emp_rates_data",employ_rates_df, colNames = T)
saveWorkbook(wb,paste0(DATA_OUT,wbname),overwrite = T)
