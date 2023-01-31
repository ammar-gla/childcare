#_____________________________________________________________________________
### Dataload and data processing ----
#_____________________________________________________________________________

# Clear out existing lists if taking memory
lfs_dataset_list <- list()
lfs_dataset_list_adj <- list()

# Load the LFS datatets
dataset_ext_names <- c("lfsh_aj18_eul","lfsh_aj19_eul","lfsh_aj20_eul_phhwt22","lfsh_aj21_eul_phhwt22","lfsh_aj22_eul_phhwt22")
dataset_years <- c(2018:2022)

#.............................................................................
#### Load LFS stats, automatically clean and save ----
#.............................................................................

lfs_dataset_nm <- c()

for (y in 1:length(dataset_ext_names)) {
  
  # Produce a list with dataframe, name and year
  temp_list <- import_save_dta(dta_num = y,
                  loadRDS = TRUE,
                  sav_dat =  TRUE,
                  years_vector = dataset_years)
  
  # Create dataframe with name and save its name
  assign(temp_list[["name"]],temp_list[["dta"]])
  lfs_dataset_nm <- c(lfs_dataset_nm,temp_list[["name"]])

  # Delete list
  rm(temp_list)
  
}

rm(y)

# Give years as names to the vector
names(lfs_dataset_nm) <- as.character(dataset_years)

#.............................................................................
#### Adjustments to data ----
#.............................................................................

# Create list with datasets, including their name

lfs_dataset_list <- setNames(lapply(lfs_dataset_nm, get),lfs_dataset_nm)


# Create data frame with labels of variables
l_df <- lapply(lfs_dataset_nm,output_labels)
df_labels <- bind_rows(l_df, .id = "dta_year") %>% 
  select(dta_year,order(colnames(.)))

# Export labels
#write.xlsx(df_labels,paste0(OTHERDATA,"\\lfsh_labels.xlsx"))

# Create overview HTMLs of the datasets
#sjPlot::view_df(lfsh_aj_2018,show.prc = T)

# Replace variables with their value labels, then remove all value labels from the datasets to allow easy mutation of variables
lfs_dataset_list_adj <- lapply(lfs_dataset_list,convert_to_label,var_vec=c("SEX","GOVTOF","ILODEFR")) %>% 
  lapply(zap_labels) %>% 
  lapply(recode_dta) 



#.............................................................................
#### Summary statistics across characteristics ----
#.............................................................................


lfs_sum_list <- lapply(lfs_dataset_list_adj,collapse_func,demog_var="SEX")







# # Using standard binary NTE variable
# 
# # Full empty dataset
# lfsh_aj_full <- data.frame(dta_year=numeric(0),london_worker=character(0),uprate_weight_ldn=numeric(0),
#                            nte_worker=character(0),wt_pop=numeric(0),unwt_pop=numeric(0),
#                            industry_job=numeric(0),occ_job=numeric(0))
# 
# 
# 
# for (dta_nm in lfs_dataset_nm) {
#   
#   # Extract year
#   dta_year <- as.numeric(names(lfs_dataset_nm)[lfs_dataset_nm==dta_nm])
# 
#   # First just extract the main summary data by NTE
#   temp_list <- join_weights(dta=lfs_dataset_list_adj[[dta_nm]],
#                             dta_year=dta_year,
#                             cons_method=FALSE,
#                             sum_group_vars=c(),
#                             nte_var="nte_worker", 
#                             agg_vars=c("industry_job","occ_job","occ_job_two")) 
#   
#   
#   #assign(paste0(dta_nm,"_wt"),temp_list[["lfsh_wt"]])
#   assign(paste0(dta_nm,"_sum"),temp_list[["lfsh_sum"]])
#   
#   # Add to full table
#   lfsh_aj_full <- lfsh_aj_full %>% 
#     bind_rows(temp_list[["lfsh_sum"]])
#   
#   rm(temp_list)
#   
#   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   
#   # Now extract using industries
#   temp_list <- join_weights(dta=lfs_dataset_list_adj[[dta_nm]],
#                             dta_year=dta_year,
#                             cons_method=FALSE,
#                             sum_group_vars=c("industry_job"),
#                             nte_var="nte_worker",
#                             agg_vars=c("occ_job","occ_job_two"))
#   
#   
#   #assign(paste0(dta_nm,"_wt_ind"),temp_list[["lfsh_wt"]])
#   assign(paste0(dta_nm,"_sum_ind"),temp_list[["lfsh_sum"]])
#   
#   # Add to full table
#   lfsh_aj_full <- lfsh_aj_full %>% 
#     bind_rows(temp_list[["lfsh_sum"]])
# 
#   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   
#   # Extract using occupations
#   temp_list <- join_weights(dta=lfs_dataset_list_adj[[dta_nm]],
#                             dta_year=dta_year,
#                             cons_method=FALSE,
#                             sum_group_vars=c("occ_job"),
#                             nte_var="nte_worker",
#                             agg_vars=c("industry_job","occ_job_two"))
#   
#   
#   #assign(paste0(dta_nm,"_wt_ind"),temp_list[["lfsh_wt"]])
#   assign(paste0(dta_nm,"_sum_occ"),temp_list[["lfsh_sum"]])
#   
#   # Add to full table
#   lfsh_aj_full <- lfsh_aj_full %>% 
#     bind_rows(temp_list[["lfsh_sum"]])
#   
#   #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   
#   # Extract using two digit occupations
#   temp_list <- join_weights(dta=lfs_dataset_list_adj[[dta_nm]],
#                             dta_year=dta_year,
#                             cons_method=FALSE,
#                             sum_group_vars=c("occ_job_two"),
#                             nte_var="nte_worker",
#                             agg_vars=c("industry_job","occ_job"))
#   
#   
#   # Add to full table
#   lfsh_aj_full <- lfsh_aj_full %>% 
#     bind_rows(temp_list[["lfsh_sum"]])
#   
# }
# 
# 
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
# writeData(wb, sheet = "nte_occ_two",lfsh_aj_occ_two, colNames = T)
# writeData(wb, sheet = "nte_data",lfsh_aj_full, colNames = T)
# writeData(wb, sheet = "nte_ind_detail",lfsh_aj_ind_detail, colNames = T)
# writeData(wb, sheet = "nte_occ_detail",lfsh_aj_occ_detail, colNames = T)
# writeData(wb, sheet = "nte_occ_two_detail",lfsh_aj_occ_two_detail, colNames = T)
# writeData(wb, sheet = "nte_detail_headline",lfsh_aj_head_detail, colNames = T)
# writeData(wb, sheet = "nte_combi_headline",lfsh_aj_head_combi, colNames = T)
# writeData(wb, sheet = "nte_shft_headline",lfsh_aj_head_shft, colNames = T)
# writeData(wb, sheet = "all_headline",lfsh_aj_head_all, colNames = T)
# writeData(wb, sheet = "all_ind",lfsh_aj_ind_all, colNames = T)
# writeData(wb, sheet = "all_occ",lfsh_aj_occ_all, colNames = T)
# saveWorkbook(wb,paste0(DATA_OUT,"/London at Night data.xlsx"),overwrite = T)
