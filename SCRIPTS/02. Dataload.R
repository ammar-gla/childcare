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

# Adjust data
lfs_dataset_list_adj <- lapply(lfs_dataset_list,recode_dta) 




#.............................................................................
#### Summary statistics across characteristics ----
#.............................................................................

# Using standard binary NTE variable

# Full empty dataset
lfsh_aj_full <- data.frame(dta_year=numeric(0),london_worker=character(0),uprate_weight_ldn=numeric(0),
                           nte_worker=character(0),wt_pop=numeric(0),unwt_pop=numeric(0),
                           industry_job=numeric(0),occ_job=numeric(0))



for (dta_nm in lfs_dataset_nm) {
  
  # Extract year
  dta_year <- as.numeric(names(lfs_dataset_nm)[lfs_dataset_nm==dta_nm])

  # First just extract the main summary data by NTE
  temp_list <- join_weights(dta=lfs_dataset_list_adj[[dta_nm]],
                            dta_year=dta_year,
                            cons_method=FALSE,
                            sum_group_vars=c(),
                            nte_var="nte_worker", 
                            agg_vars=c("industry_job","occ_job","occ_job_two")) 
  
  
  #assign(paste0(dta_nm,"_wt"),temp_list[["lfsh_wt"]])
  assign(paste0(dta_nm,"_sum"),temp_list[["lfsh_sum"]])
  
  # Add to full table
  lfsh_aj_full <- lfsh_aj_full %>% 
    bind_rows(temp_list[["lfsh_sum"]])
  
  rm(temp_list)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Now extract using industries
  temp_list <- join_weights(dta=lfs_dataset_list_adj[[dta_nm]],
                            dta_year=dta_year,
                            cons_method=FALSE,
                            sum_group_vars=c("industry_job"),
                            nte_var="nte_worker",
                            agg_vars=c("occ_job","occ_job_two"))
  
  
  #assign(paste0(dta_nm,"_wt_ind"),temp_list[["lfsh_wt"]])
  assign(paste0(dta_nm,"_sum_ind"),temp_list[["lfsh_sum"]])
  
  # Add to full table
  lfsh_aj_full <- lfsh_aj_full %>% 
    bind_rows(temp_list[["lfsh_sum"]])

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Extract using occupations
  temp_list <- join_weights(dta=lfs_dataset_list_adj[[dta_nm]],
                            dta_year=dta_year,
                            cons_method=FALSE,
                            sum_group_vars=c("occ_job"),
                            nte_var="nte_worker",
                            agg_vars=c("industry_job","occ_job_two"))
  
  
  #assign(paste0(dta_nm,"_wt_ind"),temp_list[["lfsh_wt"]])
  assign(paste0(dta_nm,"_sum_occ"),temp_list[["lfsh_sum"]])
  
  # Add to full table
  lfsh_aj_full <- lfsh_aj_full %>% 
    bind_rows(temp_list[["lfsh_sum"]])
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Extract using two digit occupations
  temp_list <- join_weights(dta=lfs_dataset_list_adj[[dta_nm]],
                            dta_year=dta_year,
                            cons_method=FALSE,
                            sum_group_vars=c("occ_job_two"),
                            nte_var="nte_worker",
                            agg_vars=c("industry_job","occ_job"))
  
  
  # Add to full table
  lfsh_aj_full <- lfsh_aj_full %>% 
    bind_rows(temp_list[["lfsh_sum"]])
  
}

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# Shift work breakdown by detailed NTE

# Full empty dataset
lfsh_aj_full_shft <- data.frame(dta_year=numeric(0),london_worker=character(0),uprate_weight_ldn=numeric(0),
                                nte_worker_detail=character(0),wt_pop=numeric(0),unwt_pop=numeric(0),
                           industry_job=numeric(0),occ_job=numeric(0),occ_job_two=numeric(0),SHFTWK99=numeric(0))



for (dta_nm in lfs_dataset_nm) {
  
  # Extract year
  dta_year <- as.numeric(names(lfs_dataset_nm)[lfs_dataset_nm==dta_nm])
  
  # First just extract the main summary data by NTE
  temp_list <- join_weights(dta=lfs_dataset_list_adj[[dta_nm]],
                            dta_year=dta_year,
                            cons_method=FALSE,
                            sum_group_vars=c("SHFTWK99"),
                            nte_var="nte_combi_worker", 
                            agg_vars=c("industry_job","occ_job","occ_job_two")) 
  
  
  #assign(paste0(dta_nm,"_wt"),temp_list[["lfsh_wt"]])
  assign(paste0(dta_nm,"_sum"),temp_list[["lfsh_sum"]])
  
  # Add to full table
  lfsh_aj_full_shft <- lfsh_aj_full_shft %>% 
    bind_rows(temp_list[["lfsh_sum"]])
  
  rm(temp_list)
  
}

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# Using detailed NTE breakdown variable

# Full empty dataset
lfsh_aj_full_detail <- data.frame(dta_year=numeric(0),london_worker=character(0),uprate_weight_ldn=numeric(0),
                                  nte_worker_detail=character(0),wt_pop=numeric(0),unwt_pop=numeric(0),
                           industry_job=numeric(0),occ_job=numeric(0))


for (dta_nm in lfs_dataset_nm) {
  
  # Extract year
  dta_year <- as.numeric(names(lfs_dataset_nm)[lfs_dataset_nm==dta_nm])
  
  # First just extract the main summary data by NTE
  temp_list <- join_weights(dta=lfs_dataset_list_adj[[dta_nm]],
                            dta_year=dta_year,
                            cons_method=FALSE,
                            sum_group_vars=c(),
                            nte_var="nte_worker_detail", 
                            agg_vars=c("industry_job","occ_job","occ_job_two")) 
  
  
  #assign(paste0(dta_nm,"_wt"),temp_list[["lfsh_wt"]])
  #assign(paste0(dta_nm,"_sum"),temp_list[["lfsh_sum"]])
  
  # Add to full table
  lfsh_aj_full_detail <- lfsh_aj_full_detail %>% 
    bind_rows(temp_list[["lfsh_sum"]])
  
  rm(temp_list)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Now extract using industries
  temp_list <- join_weights(dta=lfs_dataset_list_adj[[dta_nm]],
                            dta_year=dta_year,
                            cons_method=FALSE,
                            sum_group_vars=c("industry_job"),
                            nte_var="nte_worker_detail",
                            agg_vars=c("occ_job","occ_job_two"))
  
  
  # Add to full table
  lfsh_aj_full_detail <- lfsh_aj_full_detail %>% 
    bind_rows(temp_list[["lfsh_sum"]])
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Now extract using occupations
  temp_list <- join_weights(dta=lfs_dataset_list_adj[[dta_nm]],
                            dta_year=dta_year,
                            cons_method=FALSE,
                            sum_group_vars=c("occ_job"),
                            nte_var="nte_worker_detail",
                            agg_vars=c("industry_job","occ_job_two"))
  
  # Add to full table
  lfsh_aj_full_detail <- lfsh_aj_full_detail %>% 
    bind_rows(temp_list[["lfsh_sum"]])
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Extract using two digit occupations
  temp_list <- join_weights(dta=lfs_dataset_list_adj[[dta_nm]],
                            dta_year=dta_year,
                            cons_method=FALSE,
                            sum_group_vars=c("occ_job_two"),
                            nte_var="nte_worker_detail",
                            agg_vars=c("industry_job","occ_job"))
  
  
  # Add to full table
  lfsh_aj_full_detail <- lfsh_aj_full_detail %>% 
    bind_rows(temp_list[["lfsh_sum"]])
  

}

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# COMBI: Using permutations of usual work times variable

# Full empty dataset
lfsh_aj_full_combi <- data.frame(dta_year=numeric(0),london_worker=character(0),uprate_weight_ldn=numeric(0),
                                  nte_combi_worker=character(0),wt_pop=numeric(0),unwt_pop=numeric(0),
                                  industry_job=numeric(0),occ_job=numeric(0))


for (dta_nm in lfs_dataset_nm) {
  
  # Extract year
  dta_year <- as.numeric(names(lfs_dataset_nm)[lfs_dataset_nm==dta_nm])
  
  # First just extract the main summary data by NTE
  temp_list <- join_weights(dta=lfs_dataset_list_adj[[dta_nm]],
                            dta_year=dta_year,
                            cons_method=FALSE,
                            sum_group_vars=c(),
                            nte_var="nte_combi_worker", 
                            agg_vars=c("industry_job","occ_job","occ_job_two")) 
  
  
  # Add to full table
  lfsh_aj_full_combi <- lfsh_aj_full_combi %>% 
    bind_rows(temp_list[["lfsh_sum"]])
  
  rm(temp_list)
  
}

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# Not breaking down by night-time work

# Full empty dataset
lfsh_aj_full_all <- data.frame(dta_year=numeric(0),london_worker=character(0),uprate_weight_ldn=numeric(0),
                                  nte_helper=character(0),wt_pop=numeric(0),unwt_pop=numeric(0),
                                  industry_job=numeric(0),occ_job=numeric(0))


for (dta_nm in lfs_dataset_nm) {
  
  # Extract year
  dta_year <- as.numeric(names(lfs_dataset_nm)[lfs_dataset_nm==dta_nm])
  
  # First just extract the main summary data by NTE
  temp_list <- join_weights(dta=lfs_dataset_list_adj[[dta_nm]],
                            dta_year=dta_year,
                            cons_method=FALSE,
                            sum_group_vars=c(),
                            nte_var="nte_helper", 
                            agg_vars=c("industry_job","occ_job","occ_job_two")) 
  
  
  #assign(paste0(dta_nm,"_wt"),temp_list[["lfsh_wt"]])
  #assign(paste0(dta_nm,"_sum"),temp_list[["lfsh_sum"]])
  
  # Add to full table
  lfsh_aj_full_all <- lfsh_aj_full_all %>% 
    bind_rows(temp_list[["lfsh_sum"]])
  
  rm(temp_list)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Now extract using industries
  temp_list <- join_weights(dta=lfs_dataset_list_adj[[dta_nm]],
                            dta_year=dta_year,
                            cons_method=FALSE,
                            sum_group_vars=c("industry_job"),
                            nte_var="nte_helper",
                            agg_vars=c("occ_job","occ_job_two"))
  
  
  # Add to full table
  lfsh_aj_full_all <- lfsh_aj_full_all %>% 
    bind_rows(temp_list[["lfsh_sum"]])
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Now extract using occupations
  temp_list <- join_weights(dta=lfs_dataset_list_adj[[dta_nm]],
                            dta_year=dta_year,
                            cons_method=FALSE,
                            sum_group_vars=c("occ_job"),
                            nte_var="nte_helper",
                            agg_vars=c("industry_job","occ_job_two"))
  
  # Add to full table
  lfsh_aj_full_all <- lfsh_aj_full_all %>% 
    bind_rows(temp_list[["lfsh_sum"]])
}

#.............................................................................
#### Export tables to Excel ----
#.............................................................................


# Headline figures
lfsh_aj_head <- lfsh_aj_full %>% 
  filter(london_worker %in% c("London","Not London") & quarter_response=="Yes" & ILODEFR==1 &
           industry_job==9999  & occ_job==9999  & occ_job_two==9999) %>% 
  mutate(id=paste(dta_year,london_worker,nte_worker,sep = "_")) %>% 
  select(id,dta_year,weight_var,london_worker,nte_worker,unwt_pop,wt_pop,share_unwt_pop,share_wt_pop)



# Industry figures
lfsh_aj_ind <- lfsh_aj_full %>% 
  filter(london_worker %in% c("London","Not London") & quarter_response=="Yes" & ILODEFR==1 &
           industry_job!=9999  & occ_job==9999  & occ_job_two==9999) %>% 
  mutate(id=paste(dta_year,industry_job,london_worker,nte_worker,sep = "_")) %>% 
  select(id,dta_year,weight_var,industry_job,london_worker,nte_worker,unwt_pop,wt_pop,share_unwt_pop,share_wt_pop,share_wt_nte_across_pop)

# Occupation figures
lfsh_aj_occ <- lfsh_aj_full %>% 
  filter(london_worker %in% c("London","Not London") & quarter_response=="Yes" & ILODEFR==1 &
           industry_job==9999  & occ_job!=9999  & occ_job_two==9999) %>% 
  mutate(id=paste(dta_year,occ_job,london_worker,nte_worker,sep = "_")) %>% 
  select(id,dta_year,weight_var,occ_job,london_worker,nte_worker,unwt_pop,wt_pop,share_unwt_pop,share_wt_pop,share_wt_nte_across_pop)

lfsh_aj_occ_two <- lfsh_aj_full %>% 
  filter(london_worker %in% c("London","Not London") & quarter_response=="Yes" & ILODEFR==1 &
           industry_job==9999  & occ_job==9999  & occ_job_two!=9999) %>% 
  mutate(id=paste(dta_year,occ_job_two,london_worker,nte_worker,sep = "_")) %>% 
  select(id,dta_year,weight_var,occ_job_two,london_worker,nte_worker,unwt_pop,wt_pop,share_unwt_pop,share_wt_pop,share_wt_nte_across_pop)


# ~~~ Industry and Occupation using detailed NTE var
lfsh_aj_head_detail <- lfsh_aj_full_detail %>% 
  filter(london_worker %in% c("London","Not London") & quarter_response=="Yes" & ILODEFR==1 & 
           industry_job==9999  & occ_job==9999  & occ_job_two==9999) %>% 
  mutate(id=paste(dta_year,london_worker,nte_worker_detail,sep = "_")) %>% 
  select(id,dta_year,weight_var,london_worker,nte_worker_detail,unwt_pop,wt_pop,share_unwt_pop,share_wt_pop)


lfsh_aj_ind_detail <- lfsh_aj_full_detail %>% 
  filter(london_worker %in% c("London","Not London") & quarter_response=="Yes" & ILODEFR==1 &
           industry_job!=9999  & occ_job==9999  & occ_job_two==9999) %>% 
  mutate(id=paste(dta_year,industry_job,london_worker,nte_worker_detail,sep = "_")) %>% 
  select(id,dta_year,weight_var,industry_job,london_worker,nte_worker_detail,unwt_pop,wt_pop,share_unwt_pop,share_wt_pop,share_wt_nte_across_pop)

lfsh_aj_occ_detail <- lfsh_aj_full_detail %>% 
  filter(london_worker %in% c("London","Not London") & quarter_response=="Yes" & ILODEFR==1 &
           industry_job==9999  & occ_job!=9999  & occ_job_two==9999) %>% 
  mutate(id=paste(dta_year,occ_job,london_worker,nte_worker_detail,sep = "_")) %>% 
  select(id,dta_year,weight_var,occ_job,london_worker,nte_worker_detail,unwt_pop,wt_pop,share_unwt_pop,share_wt_pop,share_wt_nte_across_pop)

lfsh_aj_occ_two_detail <- lfsh_aj_full_detail %>% 
  filter(london_worker %in% c("London","Not London") & quarter_response=="Yes" & ILODEFR==1 &
           industry_job==9999  & occ_job==9999  & occ_job_two!=9999) %>% 
  mutate(id=paste(dta_year,occ_job_two,london_worker,nte_worker_detail,sep = "_")) %>% 
  select(id,dta_year,weight_var,occ_job_two,london_worker,nte_worker_detail,unwt_pop,wt_pop,share_unwt_pop,share_wt_pop,share_wt_nte_across_pop)


# ~~~ Permutations of NTE work
lfsh_aj_head_combi <- lfsh_aj_full_combi %>% 
  filter(london_worker %in% c("London","Not London") & quarter_response=="Yes" & ILODEFR==1 & 
           industry_job==9999  & occ_job==9999  & occ_job_two==9999) %>% 
  mutate(id=paste(dta_year,london_worker,nte_combi_worker,sep = "_")) %>% 
  select(id,dta_year,weight_var,london_worker,nte_combi_worker,unwt_pop,wt_pop,share_unwt_pop,share_wt_pop)

# ~~~ NTE and shift work
lfsh_aj_head_shft <- lfsh_aj_full_shft %>% 
  filter(london_worker %in% c("London","Not London") & quarter_response=="Yes" & ILODEFR==1 & 
           industry_job==9999  & occ_job==9999  & occ_job_two==9999) %>% 
  mutate(id=paste(dta_year,SHFTWK99,london_worker,nte_combi_worker,sep = "_")) %>% 
  select(id,dta_year,weight_var,SHFTWK99,london_worker,nte_combi_worker,unwt_pop,wt_pop,share_unwt_pop,share_wt_pop)


# ~~~ Industry and Occupation using overall 
lfsh_aj_head_all <- lfsh_aj_full_all %>% 
  filter(london_worker %in% c("London","Not London") & quarter_response=="Yes" & ILODEFR==1 & 
           industry_job==9999  & occ_job==9999  & occ_job_two==9999) %>% 
  mutate(id=paste(dta_year,london_worker,nte_helper,sep = "_")) %>% 
  select(id,dta_year,weight_var,london_worker,nte_helper,unwt_pop,wt_pop,share_unwt_pop,share_wt_pop)

lfsh_aj_ind_all <- lfsh_aj_full_all %>% 
  filter(london_worker %in% c("London","Not London") & quarter_response=="Yes" & ILODEFR==1 &
           industry_job!=9999  & occ_job==9999  & occ_job_two==9999) %>% 
  mutate(id=paste(dta_year,industry_job,london_worker,nte_helper,sep = "_")) %>% 
  select(id,dta_year,weight_var,industry_job,london_worker,nte_helper,unwt_pop,wt_pop,share_unwt_pop,share_wt_pop,share_wt_nte_across_pop)

lfsh_aj_occ_all <- lfsh_aj_full_all %>% 
  filter(london_worker %in% c("London","Not London") & quarter_response=="Yes" & ILODEFR==1 &
           industry_job==9999  & occ_job!=9999  & occ_job_two==9999) %>% 
  mutate(id=paste(dta_year,occ_job,london_worker,nte_helper,sep = "_")) %>% 
  select(id,dta_year,weight_var,occ_job,london_worker,nte_helper,unwt_pop,wt_pop,share_unwt_pop,share_wt_pop,share_wt_nte_across_pop)


# -- Open workbook, delete existing data, and save new
wb <- loadWorkbook(paste0(DATA_OUT,"/London at Night data.xlsx"))

data_sheets <- c("nte_headline","nte_ind","nte_occ","nte_occ_two","nte_data","nte_ind_detail","nte_occ_detail",
                 "nte_detail_headline","nte_combi_headline","nte_shft_headline","all_headline","all_ind","all_occ")

for (sht in data_sheets) {
  deleteData(wb , sheet = sht,cols = 1:20, rows = 1:10000, gridExpand = TRUE)
}

writeData(wb, sheet = "nte_headline",lfsh_aj_head, colNames = T)
writeData(wb, sheet = "nte_ind",lfsh_aj_ind, colNames = T)
writeData(wb, sheet = "nte_occ",lfsh_aj_occ, colNames = T)
writeData(wb, sheet = "nte_occ_two",lfsh_aj_occ_two, colNames = T)
writeData(wb, sheet = "nte_data",lfsh_aj_full, colNames = T)
writeData(wb, sheet = "nte_ind_detail",lfsh_aj_ind_detail, colNames = T)
writeData(wb, sheet = "nte_occ_detail",lfsh_aj_occ_detail, colNames = T)
writeData(wb, sheet = "nte_occ_two_detail",lfsh_aj_occ_two_detail, colNames = T)
writeData(wb, sheet = "nte_detail_headline",lfsh_aj_head_detail, colNames = T)
writeData(wb, sheet = "nte_combi_headline",lfsh_aj_head_combi, colNames = T)
writeData(wb, sheet = "nte_shft_headline",lfsh_aj_head_shft, colNames = T)
writeData(wb, sheet = "all_headline",lfsh_aj_head_all, colNames = T)
writeData(wb, sheet = "all_ind",lfsh_aj_ind_all, colNames = T)
writeData(wb, sheet = "all_occ",lfsh_aj_occ_all, colNames = T)
saveWorkbook(wb,paste0(DATA_OUT,"/London at Night data.xlsx"),overwrite = T)
