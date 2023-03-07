# Testing various aspect of survey package

library(survey)
library(tidyverse)


# Create sample dataset
n_obs = 250000

test <- data.frame(employ=rbinom(n=n_obs, size=1, prob=0.5),1000,
                   wfh=rbinom(n=n_obs, size=1, prob=0.5),
                   region=sample(0:20,1000,rep=TRUE),
                   weight=rnorm(n_obs,500,200),
                   parent = rbinom(n=n_obs, size=1, prob=0.3)) %>% 
  mutate(wfh = case_when(employ ==0 ~ NA_integer_,
                         TRUE ~ wfh),
         id = row_number())

# Survey design full
test_design <- svydesign(data=test, 
                         id=~id, 
                         strata= ~region, 
                         weights = ~weight,
                         nest=TRUE)

# Sub sample of design with only employed people
test_design_emp <- test_design %>% subset(employ==1)

# Try ratio on full design
ratio_res <- svyratio(~wfh, ~employ, 
                      design=test_design,
                      na.rm = TRUE,
                      keep.var = TRUE)

# Or mean on subset
mean_res <- svymean(~wfh, 
                    design=test_design_emp,
                    keep.var = TRUE)

# Now by parenthood

ratioby_res <- svyby(formula = ~wfh, 
                     by    = ~parent, 
                     denominator = ~employ,
                     design  = test_design, 
                     na.rm = TRUE,
                     FUN   = svyratio, 
                     keep.var = TRUE)

meanby_res <- svyby(formula = ~wfh, 
                     by    = ~parent, 
                     design  = test_design_emp, 
                     FUN   = svymean, 
                     keep.var = TRUE)
