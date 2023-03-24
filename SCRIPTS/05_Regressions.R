#_____________________________________________________________________________
### Regressions ----
#_____________________________________________________________________________


#.............................................................................
####  Try simple regressions ----
#.............................................................................

# Regress employment on parenthood, sex, age etc.
## Note: the format X*Y means the formula includes X+Y+X:Y where the latter is interacted
reg_model_vars <- list("uk_wide"=c("parent"),
                       "simple"=c("parent","london_resident"),
                       "interacted simple"=c("parent*london_resident"),
                       "regions"=c("parent","london_resident","manchester_resident","birmingham_resident",
                               "SEX_label"),
                       "sex simple"=c("parent","london_resident",
                               "SEX_label"),
                       "sex * London" =c("parent*london_resident*SEX_label"),
                       "s*L + age" =c("parent*london_resident*SEX_label","age_group"),
                       "s*L + eth" =c("parent*london_resident*SEX_label","ethnicity"),
                       "s*L + a + e" =c("parent*london_resident*SEX_label","ethnicity","age_group"),
                       "** + child age"=c("parent*london_resident*SEX_label","ethnicity","age_group",
                                           "age_child"),
                       "** + famtype"=c("parent*london_resident*SEX_label","ethnicity","age_group",
                                        "age_child","famtype"),
                       "full model"=c("parent*SEX_label*ethnicity","london_resident",
                                      "age_group","age_child"))

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

rm(i)

#.............................................................................
####  Export results ----
#.............................................................................

# Export regression output to Excel
export_summs(reg_emp_results,
             to.file = "xlsx",
             file.name = paste0(DATA_OUT,"Regression output 2022.xlsx"))


# Plot coefficients on model
## The models to include
reg_models <- list(reg_emp_results[["sex * London"]],
                   reg_emp_results[["s*L + age"]],reg_emp_results[["s*L + eth"]],
                   reg_emp_results[["s*L + a + e"]],reg_emp_results[["** + child age"]],
                   reg_emp_results[["** + famtype"]])

reg_models_names <- c("1: London & Sex","2: L&S + age groups",
                      "3: L&S + ethnicity","4: L&S+a+e",
                      "5: L&S+a+e + child age", "6: L&S+a+e+ca + famtype ")

coef_vector_main <- c("Parent"="parent",
                 "Female parent Londoner"="parent:london_resident:SEX_labelFemale",
                 "Londoner"="london_resident","Female"="SEX_labelFemale",
                 "BAME"="ethnicityBAME", "Female parent"="parent:SEX_labelFemale",
                 "London parent"="parent:london_resident",
                 "Female Londoner"="london_resident:SEX_labelFemale")

coef_vector_clean <- c("Parent"="parent",
                      "Londoner"="london_resident",
                      "Female"="SEX_labelFemale",
                      "Female parent"="parent:SEX_labelFemale",
                      "Female parent Londoner"="parent:london_resident:SEX_labelFemale")


coef_plot_main <- plot_summs(reg_models,
                             model.names = reg_models_names,
                             coefs=coef_vector_main)

save_GLA_plot("coef_plot_main")

coef_plot_clean <- plot_summs(reg_models,
                             model.names = reg_models_names,
                             coefs=coef_vector_clean)

save_GLA_plot("coef_plot_clean")

# 
# survey_2022_reg_data2 <- with(survey_2022_reg_data,
#                               expand.grid(SEX_label=reg_emp_results[[2]]$xlevels$SEX_label,
#                                           parent=mean(parent,na.rm=TRUE)))
