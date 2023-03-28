#_____________________________________________________________________________
### Regressions ----
#_____________________________________________________________________________


#.............................................................................
####  Try simple regressions ----
#.............................................................................

# Regress employment on parenthood, sex, age etc.
## Note: the format X*Y means the formula includes X+Y+X:Y where the latter is interacted


# Simpler models for investigating female London parental gap, apply on data with female parents only

# reg_model_vars <- list("uk_wide"=c("parent"),
#                        "London"=c("parent","london_resident"),
#                        "Main simple (**)"=c("parent*london_resident"),
#                        "regions"=c("parent","london_resident"),
#                        "** + age" =c("parent*london_resident","age_group"),
#                        "** + eth" =c("parent*london_resident","ethnicity"),
#                        "** + child age" =c("parent*london_resident","child_age"),
#                        "** + famtype" =c("parent*london_resident","famtype"),
#                        "** + religion" =c("parent*london_resident","RELIG11_label"),
#                        "** + disability" =c("parent*london_resident","DISEA_label"),
#                        "** + num_children" =c("parent*london_resident","num_children"),
#                        "full" =c("parent*london_resident","age_group","ethnicity","famtype","RELIG11_label","num_children","DISEA_label"))

reg_model_vars <- list("Main simple (**)"=c("london_resident"),
                       "** + age" =c("london_resident","age_group"),
                       "** + eth" =c("london_resident","ethnicity"),
                       "** + child age" =c("london_resident","child_age"),
                       "** + famtype" =c("london_resident","famtype"),
                       "** + religion" =c("london_resident","RELIG11_label"),
                       "** + disability" =c("london_resident","DISEA_label"),
                       "** + num_children" =c("london_resident","num_children"),
                       "full" =c("london_resident","age_group","ethnicity","famtype","RELIG11_label","num_children","DISEA_label"))


# Create empty list to store results
num_reg_models <- length(reg_model_vars)
reg_emp_results <- setNames(vector("list", num_reg_models),names(reg_model_vars))
reg_emp_results_region <- setNames(vector("list", num_reg_models),names(reg_model_vars))


# Re-level some of the categorical variables to use as baseline in regressions
# Restrict the data to only women to make regressions simpler
survey_2022_reg_data <- update(survey_design_adults[["lfsh_aj22"]],
                               age_group = relevel(factor(age_group),"Aged 25-34"),
                               ethnicity = relevel(factor(ethnicity),"White"),
                               child_age = relevel(factor(child_age),"4-18 yrs"),
                               DISEA_label = relevel(factor(DISEA_label),"Not Equality Act Disabled")) %>% 
  subset(SEX_label=="Female" & parent==1)

# We will only regress the 2022 data for simplicity
for(i in 1:num_reg_models) {
  
  # Extract the variables needed
  reg_model <- c(reg_model_vars[[i]])
  
  # Construct a formula object
  reg_form <- formula_helper(outcome_var = "employed",
                            formula_vars = reg_model)
  
  
  reg_emp_results[[i]] <- svyglm(design=survey_2022_reg_data,
                                 formula = reg_form)
  
  # Do same regressions adding other regions for interest
  reg_form_region <- formula_helper(outcome_var = "employed",
                                    formula_vars = c(reg_model,"manchester_resident","birmingham_resident"))
  
  reg_emp_results_region[[i]] <- svyglm(design=survey_2022_reg_data,
                                 formula = reg_form_region)
  
}

rm(i,reg_model,reg_form)

#.............................................................................
####  Export results ----
#.............................................................................

# Export regression output to Excel
export_summs(reg_emp_results,
             to.file = "xlsx",
             file.name = paste0(DATA_OUT,"Regression output 2022.xlsx"))

export_summs(reg_emp_results_region,
             to.file = "xlsx",
             file.name = paste0(DATA_OUT,"Regression output REGIONS 2022.xlsx"))


# Plot coefficients on model
## The models to include
reg_models_single <- list(reg_emp_results[["Main simple (**)"]],
                   reg_emp_results[["** + age"]],reg_emp_results[["** + eth"]],
                   reg_emp_results[["** + child age"]],
                   reg_emp_results[["** + famtype"]],reg_emp_results[["** + disability"]],
                   reg_emp_results[["** + num_children"]])

reg_models_names_single <- c("1: London parents","2: [1] + age",
                      "3: [1] + ethnicity", "4: [1] + child age","5: [1] + famtype",
                      "6: [1] + disability","7: [1] + #children")

# coef_vector_main <- c("Parent"="parent",
#                  "Female parent Londoner"="parent:london_resident:SEX_labelFemale",
#                  "Londoner"="london_resident","Female"="SEX_labelFemale",
#                  "BAME"="ethnicityBAME", "Female parent"="parent:SEX_labelFemale",
#                  "London parent"="parent:london_resident",
#                  "Female Londoner"="london_resident:SEX_labelFemale")

coef_vector_clean <- c("Parent"="parent",
                      "Londoner"="london_resident",
                      "Parent Londoner"="parent:london_resident")

coef_plot_single <- plot_summs(reg_models_single,
                             model.names = reg_models_names_single,
                             coefs=coef_vector_clean)

save_GLA_plot("coef_plot_single")

# 
# survey_2022_reg_data2 <- with(survey_2022_reg_data,
#                               expand.grid(SEX_label=reg_emp_results[[2]]$xlevels$SEX_label,
#                                           parent=mean(parent,na.rm=TRUE)))
