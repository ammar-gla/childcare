#_____________________________________________________________________________
### Regressions ----
#_____________________________________________________________________________


#.............................................................................
####  Try simple regressions ----
#.............................................................................

# Regress employment on parenthood, sex, age etc.
## Note: the format X*Y means the formula includes X+Y+X:Y where the latter is interacted
reg_model_vars <- list("simple"=c("parent","london_resident"),
                       "sex"=c("parent","london_resident","SEX_label"),
                       "famtype"=c("parent","london_resident","famtype"),
                       "sex full"=c("parent*SEX_label","london_resident"),
                       "sex & age"=c("parent","london_resident","SEX_label","age_group"),
                       "sex & age_child"=c("parent*SEX_label","london_resident","age_child"),
                       "sex, eth, age, & age_child"=c("parent*SEX_label","london_resident","ethnicity","age_group","age_child"),
                       "full model"=c("parent*SEX_label*ethnicity","london_resident","age_group","age_child"))

# Create empty list to store results
num_reg_models <- length(reg_model_vars)
reg_emp_results <- setNames(vector("list", num_reg_models),names(reg_model_vars))

# Re-level some of the categorical variables to use as baseline in regressions
survey_2022_reg_data <- update(survey_design_adults[["lfsh_aj22"]],
                               age_group = relevel(factor(age_group),"Aged 25-34"),
                               ethnicity = relevel(factor(ethnicity),"White"))

# We will only regress the 2022 data for simplicity
for(i in 1:num_reg_models) {
  
  # Extract the variables needed
  reg_model <- c(reg_model_vars[[i]])
  
  # Construct a formula object
  reg_fom <- formula_helper(outcome_var = "employed",
                            formula_vars = reg_model)
  
  
  reg_emp_results[[i]] <- svyglm(design=survey_2022_reg_data,
                                 formula = reg_fom)
  
}

# Export regression output
export_summs(reg_emp_results,
             to.file = "xlsx",
             file.name = paste0(DATA_OUT,"Regression output 2022.xlsx"))
