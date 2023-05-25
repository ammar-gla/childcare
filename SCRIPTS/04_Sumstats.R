#_____________________________________________________________________________
### Summary tables ----
#_____________________________________________________________________________


#.............................................................................
#### Whole pop Summary statistics across characteristics ----
#.............................................................................

# These means using whole population as denominator, so suitable for employment and inactivity rates

# Set up the characteristics to loop over
## Always cut by parenthood
perm_byvars <- c("parent")

# Either use london_resident as cut for London, or do not use it at all
region_byvar <- c("london_resident","")

analysis_byvars <- list("all" = c(),
                        "ethnicity" = c("ethnicity"), #using the simple BAME categories
                        "famtype" = c("famtype"),
                        "sex" = c("SEX_label"),
                        "age" = c("age_group"),
                        "benefits"=c("BENFTS_label"),
                        "child_age"=c("child_age"),
                        #"sex.age" = c("SEX_label","age_group"),
                        #"sex.famtype" = c("SEX_label","famtype"),
                        "religion"=c("RELIG11_label"),
                        "disability"=c("disability"),
                        "num_children"=c("num_children"),
                        "lev_quals" =c("lev_quals_label"),
                        "sex.child_age" = c("SEX_label","child_age"))

# Find number of models to initialise list sizes
num_models <- length(analysis_byvars)
num_list_elements <- num_models * length(region_byvar) # since we need one for each geo subset

# Create empty lists for the rates and counts
employ_rates_list <- setNames(vector("list", num_list_elements),
                              rep(names(analysis_byvars),length(region_byvar)))
inactive_rates_list <- setNames(vector("list", num_list_elements),
                                rep(names(analysis_byvars),length(region_byvar)))
survey_adult_count_list <- setNames(vector("list", num_list_elements),
                                    rep(names(analysis_byvars),length(region_byvar)))

# The loop below calculates share of people employed by characteristic
## The parameters set the permanent demographic vars (parentage and residency)
## followed by optional other variables, with appropriate names
## The output is then saved in a list of lists, with top levels named by var set
## and bottom levels named by the dataset name (i.e. year of dataset)

## Also run separately for London vs. RoUK, and UK total, to combine later

display("#########################")
display(paste0("Employment and inactivity"))
display("#########################")


for(r in 1:length(region_byvar)) {
  
  display("______________________________")
  display(paste0("Region var:",region_byvar[[r]]))
  display("______________________________")
  
  for(i in 1:num_models) {
    
    display("----------------")
    display(paste0("Model vars:",analysis_byvars[[i]]))
    display("----------------")
    
    # The position within the lists are determined by model iteration and whether regions are used
    pos_i <- i + (r-1) * num_models
    
    # Extract the variables needed, but remove element if empty (i.e. no region var)
    byvars_vec <- c(perm_byvars,region_byvar[[r]],analysis_byvars[[i]]) %>% 
      magrittr::extract(nzchar(.))
    
    # To find weighted and unweighted counts, use custom function and input the byvars
    survey_adult_count_list[[pos_i]] <- lapply(dataset_list_adj,
                                               FUN = collapse_func,
                                               group_vec = c(perm_byvars,region_byvar[[r]]),
                                               demog_vec=analysis_byvars[[i]])
    
    # Construct a formula object
    fom <- formula_helper(formula_vars = byvars_vec)
    
    
    # Insert output for employment rates into list, with name of var set
    employ_rates_list[[pos_i]] <- map_svy_means(svy_list_nm = survey_design_adults,
                                                by_formula = fom,
                                                means_var = employed)
    
    
    # Insert output for inactivity rates into list, with name of var set
    inactive_rates_list[[pos_i]] <- map_svy_means(svy_list_nm = survey_design_adults,
                                                  by_formula = fom,
                                                  means_var = inactive)
  }
  
}


# To access results of any one year and var set, refer to the list level
## e.g. employ_rates_list[[2]][[4]] is same as employ_rates_list[["ethnicity"]][["lfsh_aj_21"]]

#.............................................................................
#### Employed pop Summary statistics across characteristics ----
#.............................................................................

# Subset the design to only employed people
survey_design_adults_emp <- lapply(survey_design_adults, subset, employed==1)

# Create empty lists for the rates and counts
empl_survey_count_list <- setNames(vector("list", num_list_elements),
                                   rep(names(analysis_byvars),length(region_byvar)))
wfh_rates_list <- setNames(vector("list", num_list_elements),
                           rep(names(analysis_byvars),length(region_byvar)))
pt_rates_list <- setNames(vector("list", num_list_elements),
                          rep(names(analysis_byvars),length(region_byvar)))

## Also run separately for London vs. RoUK, and UK total, to combine later
display("#########################")
display(paste0("Vars on employed population"))
display("#########################")

for(r in 1:length(region_byvar)) {
  
  display("______________________________")
  display(paste0("Region var:",region_byvar[[r]]))
  display("______________________________")
  
  for(i in 1:num_models) {
    
    display("----------------")
    display(paste0("Model vars:",analysis_byvars[[i]]))
    display("----------------")
    
    # The position within the lists are determined by model iteration and whether regions are used
    pos_i <- i + (r-1) * num_models
    
    # Extract the variables needed, but remove element if empty (i.e. no region var)
    byvars_vec <- c(perm_byvars,region_byvar[[r]],analysis_byvars[[i]]) %>% 
      magrittr::extract(nzchar(.))
    
    # To find weighted and unweighted counts, use custom function and input the byvars
    empl_survey_count_list[[pos_i]] <- lapply(dataset_list_adj,
                                              collapse_func,
                                              demog_vec=analysis_byvars[[i]])
    
    # Construct a formula object
    fom <- formula_helper(formula_vars = byvars_vec)
    
    
    # WFH rates (somewhat different from above)
    wfh_rates_list[[pos_i]] <- map_svy_means(svy_list_nm = survey_design_adults_emp,
                                             by_formula = fom,
                                             means_var = wfh_d)
    
    # FT/PT rates (somewhat different from above)
    pt_rates_list[[pos_i]] <- map_svy_means(svy_list_nm = survey_design_adults_emp,
                                            by_formula = fom,
                                            means_var = pt_d)
  }
}

#.............................................................................
#### Prepare output statistics ----
#.............................................................................

# Due to odd behaviour from across(), need to remove names
analysis_byvars_vec <- unname(unique(unlist(analysis_byvars))) 


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Employment and inactivity
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Delist the results and combine all into handy data frame for export
employ_rates_df <- delist_results(list_nm = employ_rates_list,
                                  suffix = "empl")

# Same for inactivity
inactive_rates_df <- delist_results(list_nm = inactive_rates_list,
                                    suffix = "inac")


# Create a list where each element is the delisted dataframe of results
result_df_list <- list(employ_rates_df,
                       inactive_rates_df)

# Create a dataset with the counts, which should have the exact same number of rows as the rate estimates
survey_counts_df <- bind_rows(lapply(survey_adult_count_list,bind_rows,.id="dataset"),.id="var_set")

# Create dataset with all results and counts merged together by same columns
join_vars <- c("dataset","var_set",c(perm_byvars,region_byvar,analysis_byvars_vec)) %>% 
  magrittr::extract(nzchar(.))

means_fulldata_df <- result_df_list %>% 
  reduce(full_join, by = c("byvar_characteristic","id",join_vars)) %>% 
  full_join(survey_counts_df,by=join_vars)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# WFH and other employment-only stats
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

wfh_rates_df <- delist_results(list_nm = wfh_rates_list,
                               suffix = "wfh")
pt_rates_df <- delist_results(list_nm = pt_rates_list,
                              suffix = "pt")

empl_result_df_list <- list(wfh_rates_df,pt_rates_df)

empl_survey_counts_df <- bind_rows(lapply(empl_survey_count_list,bind_rows,.id="dataset"),.id="var_set")


empl_stats_fulldata_df <- empl_result_df_list %>% 
  reduce(full_join, by = c("byvar_characteristic","id",join_vars)) %>% 
  full_join(survey_counts_df,by=join_vars)
