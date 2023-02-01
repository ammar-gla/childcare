#_____________________________________________________________________________
### Dataload and data processing ----
#_____________________________________________________________________________

# Clear out existing lists if taking memory
dataset_list <- list()
dataset_list_adj <- list()

# Turns out we need APS datasets, not LFS
lfs_dataset_ext_names <- c("lfsh_aj18_eul","lfsh_aj19_eul","lfsh_aj20_eul_phhwt22","lfsh_aj21_eul_phhwt22","lfsh_aj22_eul_phhwt22")
aps_dataset_ext_names <- c("apsh_jd17_eul","apsh_jd18_eul","apsh_jd19_eul","apsh_jd20_eul_phhwta22","apsh_jd21_eul_phhwta22")

#.............................................................................
#### Load LFS stats, automatically clean and save ----
#.............................................................................

dataset_nm <- c()
dataset_years <- c()

for (y in 1:length(aps_dataset_ext_names)) {
  
  # Produce a list with dataframe, name and year
  temp_list <- import_save_dta(dta_num = y,
                  loadRDS = FALSE,
                  sav_dat =  TRUE,
                  aps_lfs = "APS",
                  dataset_nm_vector = aps_dataset_ext_names)
  
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
sjPlot::view_df(apsh_jd18,show.prc = T)

# Remove individual datasets to save memory
rm(list=dataset_nm)

# Define which variables to keep for analysis to save memory - and vars to transform to labels
label_var_vec <- c("SEX","GOVTOF","ILODEFR","ETHUKEUL","FUTYPE6")
analysis_var_vec <- c("parent","fam_id","AGE","weight_val")

# Replace variables with their value labels, then remove all value labels from the datasets to allow easy mutation of variables
dataset_list_adj <- lapply(dataset_list,convert_to_label,var_vec=label_var_vec) %>% 
  lapply(haven::zap_labels) %>% 
  lapply(recode_dta) %>% 
  lapply(select,c(analysis_var_vec,label_var_vec,paste(label_var_vec,"_label",sep="")))


#.............................................................................
#### Summary statistics across characteristics ----
#.............................................................................


lfs_sum_list <- lapply(dataset_list_adj,collapse_func,demog_var="SEX_label")



# #.............................................................................
# #### Export tables to Excel ----
# #.............................................................................
# 
# 
# # Headline figures
# lfsh_aj_head <- lfsh_aj_full %>% 
#   filter(london_worker %in% c("London","Not London") & quarter_response=="Yes" & ILODEFR==1 &
#            industry_job==9999  & occ_job==9999  & occ_job_two==9999) %>% 
#   mutate(id=paste(dta_year,london_worker,nte_worker,sep = "_")) %>% 
#   select(id,dta_year,weight_var,london_worker,nte_worker,unwt_pop,wt_pop,share_unwt_pop,share_wt_pop)
# 
# 
# 
# # -- Open workbook, delete existing data, and save new
# wb <- loadWorkbook(paste0(DATA_OUT,"/London at Night data.xlsx"))
# 
# data_sheets <- c("nte_headline","nte_ind","nte_occ","nte_occ_two","nte_data","nte_ind_detail","nte_occ_detail",
#                  "nte_detail_headline","nte_combi_headline","nte_shft_headline","all_headline","all_ind","all_occ")
# 
# for (sht in data_sheets) {
#   deleteData(wb , sheet = sht,cols = 1:20, rows = 1:10000, gridExpand = TRUE)
# }
# 
# writeData(wb, sheet = "nte_headline",lfsh_aj_head, colNames = T)
# writeData(wb, sheet = "nte_ind",lfsh_aj_ind, colNames = T)
# writeData(wb, sheet = "nte_occ",lfsh_aj_occ, colNames = T)

# saveWorkbook(wb,paste0(DATA_OUT,"/London at Night data.xlsx"),overwrite = T)
