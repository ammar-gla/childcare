#_____________________________________________________________________________
### Functions ----
#_____________________________________________________________________________

#.............................................................................
#### Processing functions ----
#.............................................................................

# Function to import tab data and manipulate. Alternatively load R datafile
import_save_dta <- function(dta_num=NA,
                            loadRDS=FALSE,
                            old_dat17=FALSE,
                            years_vector=dataset_years) {
  
  # Relevant names
  temp_year <- years_vector[dta_num]
  temp_name <- paste0("lfsp_aj_",temp_year)
  
  if (loadRDS==FALSE) {
    temp_dta <- read.table(file = paste0(INPUT,"\\",dataset_ext_names[dta_num],".tab"),
                           header = TRUE) 
    
    # Save relevant weight in own column 
    if (temp_year %in% c(2010:2011)) {
      temp_dta <- temp_dta %>%
        mutate(weight_val = PWT14,
               weight_var = "PWT14")
    } else  if (temp_year %in% c(2012:2019)) {
      temp_dta <- temp_dta %>%
        mutate(weight_val = PWT18,
               weight_var = "PWT18")
    } else if  (temp_year %in% c(2020:2022)) {
      temp_dta <- temp_dta %>%
        mutate(weight_val = PWT22,
               weight_var = "PWT22")
    }
    
    # Save dataset year
    temp_dta <- temp_dta %>%
      mutate(dta_year = temp_year)
    
    # Save R data and allow loading
    saveRDS(temp_dta,
            file=paste0(RDATA,temp_name,".rds"))
    
  } else { #otherwise load dataset directly
    temp_dta <- readRDS(file=paste0(RDATA,temp_name,".rds"))
  }
 
  if (old_dat17==TRUE) {
    # # To check against previous work, load previous dataset
    if (temp_year==2017) {
      
      # Load the 2017 dataset from SPSS with PWT17
      lfsp_aj_2017_pwt17 <-  read_sav(paste0(INPUT,"\\","lfsp_aj17_eul",".sav"))
      
      temp_dta <- lfsp_aj_2017_pwt17 %>%
        mutate(weight_val = PWT17,
               weight_var = "PWT17",
               dta_year = temp_year)
    }
  }
  
  temp_list <- list("dta"=temp_dta,"name"=temp_name,"year"=temp_year)
  return(temp_list)
}

# Function to adjust the data for our use, inspired by previous SPS code
## Missing variables:
## NATOX, CRYOX7
recode_dta <- function(dta=NA) {
  
  # Check year and set SOC var accordingly
  dta_year_check <- dta %>% 
    group_by(dta_year) %>% 
    filter(row_number()==1) %>% 
    pull(dta_year)
  
  if (dta_year_check>2020) soc_var <- "SC20MMJ" else soc_var <- "SC10MMJ"
  if (dta_year_check>2020) soc_var_two <- "SOC20M" else soc_var_two <- "SOC10M"  # for detailed occ
  
  # Change data
  dta_adj <- dta %>% 
    mutate(london_worker = case_when(GORWKR==8 ~ "London",
                                     GORWKR==-8 ~ "No answer",
                                     GORWKR==-9 ~ "NA",
                                     is.na(GORWKR) ~ "NA",
                                     TRUE ~ "Not London"),
           london_resident = case_when(GOVTOF2==8 ~ "London",
                                       TRUE ~ "Not London"),
           age_band = case_when(AGE <16 ~ "<16",
                                inrange(AGE,16,34) ~ "16-34",
                                inrange(AGE,35,54) ~ "35-54",
                                AGE>54 ~ "55+"),
           ethnicity = case_when(ETHUKEUL==1 ~ "White",
                                 ETHUKEUL %in% c(2,3,4,5,6,7,8,9) ~ "BAME",
                                 ETHUKEUL==-8 ~ "No answer",
                                 ETHUKEUL==-9 ~ "NA"),
           quarter_response = case_when(IOUTCOME %in% c(1,2) ~ "Yes",
                                        IOUTCOME == 6 ~ "No",
                                        TRUE ~ "NA"),
           nte_worker = case_when(USUWRK2==1 | USUWRK3==1 ~ "Yes",
                                  USUWRK2== 2 & USUWRK3==2 ~ "No",
                                  USUWRK2== -8 & USUWRK3==-8 ~ "No answer",
                                  USUWRK2== -9 & USUWRK3==-9 ~ "NA",
                                  TRUE ~ "NA"),
           nte_worker_detail = case_when(USUWRK2==1 & USUWRK3==1 ~ "Both",
                                         USUWRK2==1 & USUWRK3!=1 ~ "Evening",
                                         USUWRK2!=1 & USUWRK3==1 ~ "Night",
                                         USUWRK2== 2 & USUWRK3==2 ~ "No",
                                         USUWRK2== -8 & USUWRK3==-8 ~ "No answer",
                                         USUWRK2== -9 & USUWRK3==-9 ~ "NA",
                                         TRUE ~ "NA"),
           nte_combi_worker = case_when(USUWRK1==1 & USUWRK2==1 & USUWRK3==1 ~ "D-E-N",
                                        USUWRK1==1 & USUWRK2==1 & USUWRK3!=1 ~ "D-E",
                                        USUWRK1==1 &  USUWRK2!=1 & USUWRK3==1 ~ "D-N",
                                        USUWRK1!=1 &  USUWRK2==1 & USUWRK3==1 ~ "E-N",
                                        USUWRK1==1 & USUWRK2!=1 & USUWRK3!=1 ~ "D",
                                        USUWRK1!=1 & USUWRK2==1 & USUWRK3!=1 ~ "E",
                                        USUWRK1!=1 & USUWRK2!=1 & USUWRK3==1 ~ "N",
                                        USUWRK1==2 &  USUWRK2== 2 & USUWRK3==2 ~ "None",
                                        USUWRK1==-8 &  USUWRK2== -8 & USUWRK3==-8 ~ "No answer",
                                        USUWRK1==-9 &  USUWRK2== -9 & USUWRK3==-9 ~ "NA",
                                         TRUE ~ "NA"),
           nte_helper = "Any", # for aggregating everything in data
           # eve_work = EVENG, #these variables ask whether person works at least half of time in evening/night
           # night_work = NIGHT,
           industry_job = case_when(INDS07M %in% c(18L,19L) ~ 18L, # group R arts and S other services, together
                                    TRUE ~ INDS07M), 
           occ_job = !!sym(soc_var),
           occ_job_two = floor(!!sym(soc_var_two)/100)) # forcing a two-digit SOC code 
  
  
  
  return(dta_adj)
  
}


# For adjusted weight find prop of people who were not in this quarter's survey, and uprate
## The reason we are using weights from employed people is because they are the only ones with a region of work!
new_weight <- function(dta=NA,
                       uk_tot=FALSE,
                       cons_method=FALSE) {
  
  # Old weighting method - London workers, and everyone else who works in other bucket
  ## Note: a defunct version also shown in old xlsx files also puts in people who do not work into the bin. That is not the case in published figures.
   if (cons_method==TRUE) {
     if (uk_tot==FALSE) {
       
       # Group together everyone else in not London, and do not group by employment status
       dta_new <- dta %>% 
         mutate(london_worker=case_when(london_worker=="London" ~ london_worker,
                                        TRUE ~ "Not London")) %>% 
         group_by(london_worker,ILODEFR,quarter_response) %>% # if not grouping by ILODEFR, would get the discarded higher weight of 1.2813518
         summarise(weight_val = sum(weight_val)) %>% 
         ungroup() %>% 
         pivot_wider(id_cols=c(london_worker,ILODEFR),values_from = weight_val,names_from = quarter_response,names_prefix = "quarter_") %>% 
         mutate(uprate_weight_ldn = (quarter_Yes+quarter_No)/quarter_Yes) %>% 
         select(london_worker,ILODEFR,uprate_weight_ldn)
       
     }  
     else if (uk_tot==TRUE) {
       
       dta_new <- dta %>% 
         mutate(london_worker=case_when(london_worker=="London" ~ london_worker,
                                        TRUE ~ "Not London")) %>% 
         group_by(ILODEFR,quarter_response) %>% 
         summarise(weight_val = sum(weight_val)) %>% 
         ungroup() %>% 
         pivot_wider(id_cols=c(ILODEFR),values_from = weight_val,names_from = quarter_response,names_prefix = "quarter_") %>% 
         mutate(uprate_weight_uk = (quarter_Yes+quarter_No)/quarter_Yes) %>% 
         select(ILODEFR,uprate_weight_uk)
     }
   }
  
  # More meaningful: separating out non-workers from both groups
  else {
    if (uk_tot==FALSE) {
      
      # First find uprating for London and non-London
      dta_new <- dta %>% 
        group_by(london_worker,ILODEFR,quarter_response) %>% 
        summarise(weight_val = sum(weight_val)) %>% 
        filter(ILODEFR==1) %>%
        ungroup() %>% 
        pivot_wider(id_cols=c(london_worker,ILODEFR),values_from = weight_val,names_from = quarter_response,names_prefix = "quarter_") %>% 
        mutate(uprate_weight_ldn = (quarter_Yes+quarter_No)/quarter_Yes) %>% 
        select(london_worker,ILODEFR,uprate_weight_ldn)
      
    }  
    else if (uk_tot==TRUE) {
      
      dta_new <- dta %>% 
        group_by(ILODEFR,quarter_response) %>% 
        summarise(weight_val = sum(weight_val)) %>% 
        filter(ILODEFR==1) %>% 
        ungroup() %>% 
        pivot_wider(id_cols=c(ILODEFR),values_from = weight_val,names_from = quarter_response,names_prefix = "quarter_") %>% 
        mutate(uprate_weight_uk = (quarter_Yes+quarter_No)/quarter_Yes) %>% 
        select(ILODEFR,uprate_weight_uk)
    }
  }
  
  
  return(dta_new)
}

join_weights <- function(dta=NA,
                         dta_year=NA,
                         cons_method=FALSE,
                         default_group_vars=c("ILODEFR","london_worker"),
                         sum_group_vars=c(), # additional grouping, e.g. industry and occupation
                         nte_var="nte_worker", # in case we want separate evening/night time
                         agg_vars=c("industry_job","occ_job","occ_job_two")) # turn the value into "Any" to allow binding
  { 
  
  
  london_wt_dta <- new_weight(dta,
                              cons_method=cons_method)
  
  uk_wt_dta <- new_weight(dta,
                          uk_tot = TRUE,
                          cons_method=cons_method)
  
  if (cons_method==TRUE) {
    # ALT - consistent with old method, merge all NA into non-London
    lfsp_wt <- lfs_dataset_list_adj[[dta_nm]] %>%
      mutate(london_worker=case_when(london_worker=="London" ~ london_worker,
                                     TRUE ~ "Not London"),
             across({{agg_vars}}, ~ 9999)) %>%
      left_join(london_wt_dta,by=c("london_worker","ILODEFR")) %>%
      left_join(uk_wt_dta,by=c("ILODEFR")) %>%
      mutate(weight_val_ldn = weight_val * uprate_weight_ldn,
             weight_val_uk = weight_val * uprate_weight_uk)
  }
  else {
    # Merge on the weights
    lfsp_wt <- dta %>%
      mutate(across({{agg_vars}}, ~ 9999)) %>%
      left_join(london_wt_dta,by=c("london_worker","ILODEFR")) %>%
      left_join(uk_wt_dta,by=c("ILODEFR")) %>%
      mutate(weight_val_ldn = weight_val * uprate_weight_ldn,
             weight_val_uk = weight_val * uprate_weight_uk)
  }
  
  # Note: to use a vector of strings as variables, need to use across(all_of(sum_group_vars) below
  
  
  # Only interested in quarter_response=="Yes" & ILODEFR==1, but keep all for data checking
  lfsp_sum <- lfsp_wt %>% 
    group_by(quarter_response,across(all_of(sum_group_vars)),across(all_of(default_group_vars)),across(all_of(nte_var)),across(all_of(agg_vars)),dta_year,uprate_weight_ldn,weight_var) %>% 
    summarise(wt_pop=sum(weight_val_ldn),
              unwt_pop=n()) %>% 
    group_by(quarter_response,across(all_of(sum_group_vars)),across(all_of(default_group_vars)),across(all_of(agg_vars)),dta_year,uprate_weight_ldn,weight_var) %>% 
    mutate(share_wt_pop = wt_pop/sum(wt_pop),
           share_unwt_pop = unwt_pop/sum(unwt_pop)) %>% 
    group_by(quarter_response,across(all_of(default_group_vars)),across(all_of(nte_var)),across(all_of(agg_vars)),dta_year,uprate_weight_ldn,weight_var) %>% 
    mutate(share_wt_nte_across_pop = wt_pop/sum(wt_pop)) %>% # to see how many of nte workers are in each sum_grouping
    ungroup()
  
  temp_list <- list("lfsp_wt"=lfsp_wt,"lfsp_sum"=lfsp_sum)
  return(temp_list)
  
}

# 
# test <- function(vars=c("london_worker","nte_worker")) {
#   
#   dta <- d %>% 
#     group_by( across(all_of(vars)) ) %>% 
#     summarise(n=n())
#   
#   return(dta)
# }
# 
# d <- tibble(a=c(10,10,10,10,10),london_worker=c(1,2,2,1,1),nte_worker=c(1,1,2,2,2))
# test()
