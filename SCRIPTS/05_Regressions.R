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

# Vector of all variables to avoid any NAs
all_covars <- c("london_resident","age_group","ethnicity","child_age","famtype",
                "RELIG11_label","num_children","disability")

survey_2022_reg_data <- update(survey_design_adults[["lfsh_aj22"]],
                               age_group = relevel(factor(age_group),"Aged 25-34"),
                               ethnicity = relevel(factor(ethnicity),"White"),
                               child_age = relevel(factor(child_age),"5-18 yrs"),
                               disability = relevel(factor(disability),"Not disabled"),
                               num_children = relevel(factor(num_children),"1 child"),
                               lev_quals_label = relevel(factor(lev_quals_label),"No Qualifications")) %>% 
  subset(!is.na(RELIG11_label)) #removing N. Ireland

if (fem_parent_only==TRUE) {
  
  reg_model_vars <- list("Main simple (**)"=c("london_resident"),
                         "** + age" =c("london_resident","age_group"),
                         "** + eth" =c("london_resident","ethnicity"),
                         "** + child age" =c("london_resident","child_age"),
                         "** + famtype" =c("london_resident","famtype"),
                         "** + religion" =c("london_resident","RELIG11_label"),
                         "** + disability" =c("london_resident","disability"),
                         "** + num_children" =c("london_resident","num_children"),
                         "** + age + child age"=c("london_resident","age_group","child_age"),
                         "** + eth + religion"=c("london_resident","ethnicity","RELIG11_label"),
                         "** + lev_quals" =c("london_resident","lev_quals_label"),
                         "full" =c("london_resident","age_group","ethnicity","famtype",
                                   "RELIG11_label","num_children","disability","lev_quals_label"))
  
  # Re-level some of the categorical variables to use as baseline in regressions
  # Restrict the data to only women & parents to make regressions simpler
  survey_2022_reg_data <- survey_2022_reg_data %>% 
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
                         "** + disability" =c("parent*london_resident","disability"),
                         "** + num_children" =c("parent*london_resident","num_children"),
                         "** + lev_quals" =c("london_resident","lev_quals_label"),
                         "full" =c("parent*london_resident","age_group","ethnicity","famtype",
                                   "RELIG11_label","num_children","disability","lev_quals_label"))
  
  # Restrict the data to only women to make regressions simpler
  survey_2022_reg_data <- survey_2022_reg_data %>% 
    subset(SEX_label=="Female")
  
}

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

# Export regression on London resident output to Excel
export_summs(reg_emp_results,
             to.file = "xlsx",
             error_format = "[{conf.low}, {conf.high}]",
             ci_level = 0.90,
             number_format = "%.2g",
             file.name = paste0(DATA_OUT,"Regression output 2022.xlsx"))

export_summs(reg_emp_results_region,
             to.file = "xlsx",
             error_format = "[{conf.low}, {conf.high}]",
             ci_level = 0.90,
             number_format = "%.2g",
             file.name = paste0(DATA_OUT,"Regression output REGIONS 2022.xlsx"))
#...............................................................................

# Plot coefficients on London resident on model
## The models to include
reg_models_single <- list(reg_emp_results[["Main simple (**)"]],
                   reg_emp_results[["** + age"]],reg_emp_results[["** + eth"]],
                   reg_emp_results[["** + famtype"]],reg_emp_results[["** + disability"]],
                   reg_emp_results[["** + num_children"]],reg_emp_results[["** + religion"]],
                   reg_emp_results[["** + lev_quals"]],reg_emp_results[["** + eth + religion"]],
                   reg_emp_results[["full"]])

# The names we use to descibe models in chart
reg_models_names_single <- c("1: Baseline","2: [1] + age",
                             "3: [1] + ethnicity", "4: [1] + famtype",
                             "5: [1] + disability","6: [1] + #children",
                             "7: [1] + religion","8: [1] + qualification",
                             "9: [1] + eth + religion","10: Full model")

#GLA colors
coef_plot_colors <- gla_pal(n=length(reg_models_names_single))

if (fem_parent_only==TRUE) {
  coef_vector_clean <- c("Londoner"="london_resident")
  
} else {
  coef_vector_clean <- c("Parent Londoner"="parent:london_resident")
}

# Produce the chart with only coefficient on London residents
coef_plot_single <- plot_summs(reg_models_single,
                               ci_level = 0.90,
                               model.names = reg_models_names_single,
                               coefs=coef_vector_clean,
                               colors = coef_plot_colors)

save_GLA_plot("coef_plot_single",w=6,h=4)

# Plot some coefficients of interest from full model
reg_models_interest <- list(reg_emp_results[["full"]])

# The names we use to descibe models in chart
reg_models_names_interest <- c("10: Full model")

coef_vector_interest <- c("Aged 18-24"="age_groupAged 18-24",
                          "BAME"="ethnicityBAME",
                          "Couple"="famtypeCouple",
                          "Muslim"="RELIG11_labelMuslim",
                          "Disabled"="disabilityDisabled",
                          "Child <2yrs"="child_age2 yrs or less",
                          "Child 3-4yrs"="child_age3-4 yrs",
                          "3+ children"="num_children3+ children")

coef_plot_colors <- last(gla_pal(n=length(reg_models_names_single))) #to align with full model colors

coef_plot_interest <- plot_summs(reg_models_interest,
                                 ci_level = 0.90,
                                 model.names = reg_models_names_interest,
                                 coefs=coef_vector_interest,
                                 colors = coef_plot_colors)

save_GLA_plot("coef_plot_interest",w=6,h=4)

# Plot some coefficients of interest from full model
reg_models_interest <- list(reg_emp_results[["full"]])

# The names we use to descibe models in chart
reg_models_names_interest <- c("10: Full model")

coef_vector_interest <- c("Aged 18-24"="age_groupAged 18-24",
                          "BAME"="ethnicityBAME",
                          "Couple"="famtypeCouple",
                          "Muslim"="RELIG11_labelMuslim",
                          "Disabled"="disabilityDisabled",
                          "Child <2yrs"="child_age2 yrs or less",
                          "Child 3-4yrs"="child_age3-4 yrs",
                          "3+ children"="num_children3+ children")

coef_plot_colors <- last(gla_pal(n=length(reg_models_names_single))) #to align with full model colors

coef_plot_interest <- plot_summs(reg_models_interest,
                               ci_level = 0.90,
                               model.names = reg_models_names_interest,
                               coefs=coef_vector_interest,
                               colors = coef_plot_colors)

save_GLA_plot("coef_plot_interest",w=6,h=4)
