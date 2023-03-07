#_____________________________________________________________________________
### Survey level analysis ----
#_____________________________________________________________________________

#.............................................................................
#### Prepare analysis data ----
#.............................................................................


# Define which variables to keep for analysis to save memory - and vars to transform to labels
label_var_vec <- c("SEX","GOVTOF","ILODEFR","ETHUKEUL","FUTYPE6","HOME")
analysis_var_vec <- c("parent","fam_id","AGE","adult1664","weight_val",
                      "HSERIALP","employed","london_resident","inactive","unemployed",
                      "age_group","famtype")

# Replace variables with their value labels, then remove all value labels from the datasets to allow easy mutation of variables
dataset_list_adj <- lapply(dataset_list,convert_to_label,var_vec=label_var_vec) %>% 
  lapply(haven::zap_labels) %>% 
  lapply(recode_dta) %>% 
  lapply(dup_dataset) %>% 
  lapply(select,c(analysis_var_vec,label_var_vec,paste(label_var_vec,"_label",sep="")))

# To save space, remove original dataset list
rm(dataset_list)

#.............................................................................
#### Summary statistics across characteristics ----
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

# Create subset for UK overall population rather than London vs. RoUK
#survey_design_adults_uk <- lapply(survey_design_full, subset, adult1664 == 1, london_resident %in% c(2))

# Remove full survey design for memory savings
rm(survey_design_full)

# Create empty lists for the rates and counts
employ_rates_list <- list()
inactive_rates_list <- list()
survey_count_list <- list()

perm_byvars <- c("parent","london_resident")
analysis_byvars <- list("all" = c(),
                        "ethnicity" = c("ETHUKEUL_label"),
                        "famtype_long" = c("FUTYPE6_label"),
                        "famtype" = c("famtype"),
                        "wfh" = c("HOME_label"),
                        "sex" = c("SEX_label"),
                        "sex.age" = c("SEX_label","age_group"))

# The loop below calculates share of people employed by characteristic
## The parameters set the permanent demographic vars (parentage and residency)
## followed by optional other variables, with appropriate names
## The output is then saved in a list of lists, with top levels named by var set
## and bottom levels named by the dataset name (i.e. year of dataset)
for(i in 1:length(analysis_byvars)) {
  
  # Extract the variables needed
  byvars_vec <- c(perm_byvars,analysis_byvars[[i]])
  
  # To find weighted and unweighted counts, use custom function and input the byvars
  survey_count_list[[paste0(names(analysis_byvars)[[i]])]] <- lapply(dataset_list_adj,
                            collapse_func,
                            demog_vec=analysis_byvars[[i]])
  
  
  
  # Construct a formula object
  fom <- formula_helper(formula_vars = byvars_vec)
  
  # Insert output for employment rates into list, with name of var set
  # employ_rates_list[[paste0(names(analysis_byvars)[[i]])]] <- lapply(X=survey_design_adults,
  #                                                              FUN= function(design,formula,by,com,keep.var,keep.names) 
  #                                                                svyby(design=design,formula=formula,by=by,FUN=com,keep.var=keep.var),
  #                                                              formula = ~employed, 
  #                                                              by    = fom, 
  #                                                              com   = svymean, 
  #                                                              keep.var = TRUE)
  # 
  employ_rates_list[[paste0(names(analysis_byvars)[[i]])]] <- lapply_svy_means(svy_list_nm = survey_design_adults,
                                                                               by_formula = fom,
                                                                               means_var = employed)
  
  # Insert output for inactivity rates into list, with name of var set
  # inactive_rates_list[[paste0(names(analysis_byvars)[[i]])]] <- lapply(X=survey_design_adults,
  #                                                                    FUN= function(design,formula,by,com,keep.var,keep.names) 
  #                                                                      svyby(design=design,formula=formula,by=by,FUN=com,keep.var=keep.var),
  #                                                                    formula = ~inactive, 
  #                                                                    by    = fom, 
  #                                                                    com   = svymean, 
  #                                                                    keep.var = TRUE)
  
  inactive_rates_list[[paste0(names(analysis_byvars)[[i]])]] <- lapply_svy_means(svy_list_nm = survey_design_adults,
                                                                               by_formula = fom,
                                                                               means_var = inactive)
}

# To access results of any one year and var set, refer to the list level
## e.g. employ_rates_list[[2]][[4]] is same as employ_rates_list[["ethnicity"]][["lfsh_aj_21"]]

#.............................................................................
####  Try simple regressions ----
#.............................................................................

# Regress employment on parenthood, sex, age etc.
## Note: the format X*Y means the formula includes X+Y+X:Y where the latter is interacted
reg_model_vars <- list("simple"=c("parent"),
                       "sex"=c("parent","SEX_label"),
                       "famtype"=c("parent","famtype"),
                       "sex full"=c("parent*SEX_label"),
                       "sex & age"=c("parent","SEX_label","age_group"),
                       "sex & age full"=c("parent*SEX_label*age_group"))

# Create empty list to store results
reg_emp_results <- list()

# Re-level some of the categorical variables to use as baseline in regressions
survey_2022_reg_data <- update(survey_design_adults[["lfsh_aj22"]], age_group = relevel(factor(age_group),"Aged 25-34"))

# We will only regress the 2022 data for simplicity
for(i in 1:length(reg_model_vars)) {
  
  # Extract the variables needed
  reg_model <- c(reg_model_vars[[i]])
  
  # Construct a formula object
  reg_fom <- formula_helper(outcome_var = "employed",
                        formula_vars = reg_model)
  
  
  reg_emp_results[[paste0(names(reg_model_vars)[[i]])]] <- svyglm(design=survey_2022_reg_data,
                                                                  formula = reg_fom)
  
}

# Export regression output
export_summs(reg_emp_results,
             to.file = "xlsx",
             file.name = paste0(DATA_OUT,"Regression output 2022.xlsx"))

#.............................................................................
#### Prepare output statistics ----
#.............................................................................

# Delist the results and combine all into handy data frame for export
analysis_byvars_vec <- unname(unique(unlist(analysis_byvars))) # due to odd behaviour from across(), need to remove names

employ_rates_df <- delist_results(list_nm = employ_rates_list,
                                  suffix = "empl")

# Same for inactivity
inactive_rates_df <- delist_results(list_nm = inactive_rates_list,
                                     suffix = "inac")

# Create a list where each element is the delisted dataframe of results
result_df_list <- list(employ_rates_df,
                       inactive_rates_df)

# Create a dataset with the counts, which should have the exact same number of rows as the rate estimates
survey_counts_df <- bind_rows(lapply(survey_count_list,bind_rows,.id="dataset"),.id="var_set")

means_fulldata_df <- result_df_list %>% 
  reduce(full_join, 
         by = c("dataset","var_set","byvar_characteristic","id",all_of(c(perm_byvars,analysis_byvars_vec)))) %>% 
  full_join(survey_counts_df,by=c("dataset","var_set",all_of(c(perm_byvars,analysis_byvars_vec))))

