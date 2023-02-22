#_____________________________________________________________________________
### Dataload and data processing ----
#_____________________________________________________________________________

# Clear out existing lists if taking memory
dataset_list <- list()
dataset_list_adj <- list()

# Turns out we need APS datasets, not LFS
lfs_dataset_ext_names <- c("lfsh_aj18_eul","lfsh_aj19_eul","lfsh_aj20_eul_phhwt22","lfsh_aj21_eul_phhwt22","lfsh_aj22_eul_phhwt22")
aps_dataset_ext_names <- c("apsh_jd17_eul","apsh_jd18_eul","apsh_jd19_eul","apsh_jd20_eul_phhwta22","apsh_jd21_eul_phhwta22")


# NB: SET THIS TO LFS OR APS!!!
ons_data_used <- "lfs"

#.............................................................................
#### Load LFS stats, automatically clean and save ----
#.............................................................................

dataset_nm <- c()
dataset_years <- c()

if (str_to_lower(ons_data_used)=="lfs") dataset_ext_names <- lfs_dataset_ext_names
if (str_to_lower(ons_data_used)=="aps") dataset_ext_names <- aps_dataset_ext_names

for (y in 1:length(dataset_ext_names)) {
  
  # Produce a list with dataframe, name and year
  temp_list <- import_save_dta(dta_num = y,
                  loadRDS = TRUE,
                  sav_dat =  TRUE,
                  aps_lfs = ons_data_used,
                  dataset_nm_vector = dataset_ext_names)
  
  # Create dataframe with name and save its name
  assign(temp_list[["name"]],temp_list[["dta"]])
  dataset_nm <- c(dataset_nm,temp_list[["name"]])
  
  # Save years in vector
  dataset_years <- c(dataset_years,temp_list[["year"]])

  # Delete list
  rm(temp_list)
  
}

rm(y)

# Give years as names to the vector
names(dataset_nm) <- as.character(dataset_years)

#.............................................................................
#### Adjustments to data ----
#.............................................................................

# Create list with datasets, including their name

dataset_list <- setNames(lapply(dataset_nm, get),dataset_nm)



# Create data frame with labels of variables
l_df <- lapply(dataset_nm,output_labels)
df_labels <- bind_rows(l_df, .id = "dta_year") %>% 
  select(dta_year,order(colnames(.)))

# Export labels
#write.xlsx(df_labels,paste0(OTHERDATA,"\\apsh_labels.xlsx"))

# Create overview HTMLs of the datasets
sjPlot::view_df(lfsh_aj22,show.prc = T, file = paste0(OTHERDATA,"LFS_overview_labels_2022.html"))

# Remove individual datasets to save memory
rm(list=dataset_nm)

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

# Calculate the share of people who are employed, i.e. mean of employment binary
output <- lapply(X=survey_design_adults,
                 FUN= function(design,formula,by,com,keep.var) 
                   svyby(design=design,formula=formula,by=by,FUN=com,keep.var=keep.var),
                 formula = ~employed, 
                 by    = ~parent + london_resident, 
                 com   = svymean, 
                 keep.var = TRUE)

output_df <- bind_rows(output,.id="dataset")
