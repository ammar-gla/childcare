#_____________________________________________________________________________
### Regressions ----
#_____________________________________________________________________________


#.............................................................................
####  Try simple regressions ----
#.............................................................................

# Regress employment on parenthood, sex, age etc.
## Note: the format X*Y means the formula includes X+Y+X:Y where the latter is interacted

# ACTION: choose to regress on female-only data or female-parent-only data
fem_parent_only <- TRUE


if (fem_parent_only==TRUE) {
  
  reg_model_vars <- list("Main simple (**)"=c("london_resident"),
                         "** + age" =c("london_resident","age_group"),
                         "** + eth" =c("london_resident","ethnicity"),
                         "** + child age" =c("london_resident","child_age"),
                         "** + famtype" =c("london_resident","famtype"),
                         "** + religion" =c("london_resident","RELIG11_label"),
                         "** + disability" =c("london_resident","DISEA_label"),
                         "** + num_children" =c("london_resident","num_children"),
                         "full" =c("london_resident","age_group","ethnicity","famtype",
                                   "RELIG11_label","num_children","DISEA_label"))
  
  # Re-level some of the categorical variables to use as baseline in regressions
  # Restrict the data to only women & parents to make regressions simpler
  survey_2022_reg_data <- update(survey_design_adults[["lfsh_aj22"]],
                                 age_group = relevel(factor(age_group),"Aged 25-34"),
                                 ethnicity = relevel(factor(ethnicity),"White"),
                                 child_age = relevel(factor(child_age),"4-18 yrs"),
                                 DISEA_label = relevel(factor(DISEA_label),"Not Equality Act Disabled")) %>% 
    subset(SEX_label=="Female" & parent==1)
  
} else {
  
  reg_model_vars <- list("uk_wide"=c("parent"),
                         "London"=c("parent","london_resident"),
                         "Main simple (**)"=c("parent*london_resident"),
                         "regions"=c("parent","london_resident"),
                         "** + age" =c("parent*london_resident","age_group"),
                         "** + eth" =c("parent*london_resident","ethnicity"),
                         "** + eth interact" =c("parent*london_resident","ethnicity*parent"),
                         "** + child age" =c("parent*london_resident","child_age"),
                         "** + famtype" =c("parent*london_resident","famtype"),
                         "** + religion" =c("parent*london_resident","RELIG11_label"),
                         "** + religion interact" =c("parent*london_resident","RELIG11_label*parent"),
                         "** + disability" =c("parent*london_resident","DISEA_label"),
                         "** + num_children" =c("parent*london_resident","num_children"),
                         "full" =c("parent*london_resident","age_group","ethnicity","famtype",
                                   "RELIG11_label","num_children","DISEA_label"))
  
  # Restrict the data to only women to make regressions simpler
  survey_2022_reg_data <- update(survey_design_adults[["lfsh_aj22"]],
                                 age_group = relevel(factor(age_group),"Aged 25-34"),
                                 ethnicity = relevel(factor(ethnicity),"White"),
                                 child_age = relevel(factor(child_age),"4-18 yrs"),
                                 DISEA_label = relevel(factor(DISEA_label),"Not Equality Act Disabled")) %>% 
    subset(SEX_label=="Female")
  
}

# Note: religion is not available for N. Ireland

# Create empty list to store results
num_reg_models <- length(reg_model_vars)
reg_emp_results <- setNames(vector("list", num_reg_models),names(reg_model_vars))
reg_emp_results_region <- setNames(vector("list", num_reg_models),names(reg_model_vars))


# We will only regress the 2022 data for simplicity
reg_emp_results <- lapply(reg_model_vars,
                          svyglm_regress,
                          design=survey_2022_reg_data,
                          region_dummies=FALSE)

reg_emp_results_region <- lapply(reg_model_vars,
                                 svyglm_regress,
                                 design=survey_2022_reg_data,
                                 region_dummies=TRUE)

#.............................................................................
####  Export results ----
#.............................................................................

# Export regression output to Excel
export_summs(reg_emp_results,
             to.file = "xlsx",
             number_format = "%.4g",
             file.name = paste0(DATA_OUT,"Regression output 2022.xlsx"))

export_summs(reg_emp_results_region,
             to.file = "xlsx",
             number_format = "%.4g",
             file.name = paste0(DATA_OUT,"Regression output REGIONS 2022.xlsx"))


# Plot coefficients on model
## The models to include
reg_models_single <- list(reg_emp_results[["Main simple (**)"]],
                   reg_emp_results[["** + age"]],reg_emp_results[["** + eth"]],
                   reg_emp_results[["** + famtype"]],reg_emp_results[["** + disability"]],
                   reg_emp_results[["** + num_children"]],reg_emp_results[["** + religion"]],reg_emp_results[["full"]])

# reg_models_names_single <- c("1: London parents","2: [1] + age",
#                       "3: [1] + ethnicity", "4: [1] + child age","5: [1] + famtype",
#                       "6: [1] + disability","7: [1] + #children")

reg_models_names_single <- c("1: Baseline","2: [1] + age",
                             "3: [1] + ethnicity", "4: [1] + famtype",
                             "5: [1] + disability","6: [1] + #children",
                             "7: [1] + religion","8: Full model")

coef_plot_colors <- gla_pal(n=length(reg_models_names_single))

if (fem_parent_only==TRUE) {
  coef_vector_clean <- c("Londoner"="london_resident")
  
} else {
  coef_vector_clean <- c("Parent Londoner"="parent:london_resident")
}

coef_plot_single <- plot_summs(reg_models_single,
                             model.names = reg_models_names_single,
                             coefs=coef_vector_clean,
                             colors = coef_plot_colors)

save_GLA_plot("coef_plot_single")

