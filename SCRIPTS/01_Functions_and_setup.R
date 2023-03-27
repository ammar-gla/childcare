#_____________________________________________________________________________
### Functions ----
#_____________________________________________________________________________

#.............................................................................
#### Processing functions ----
#.............................................................................

# Function to import tab data and manipulate. Alternatively load R datafile
import_save_dta <- function(dta_num=NA,
                            aps_lfs=NA,
                            loadRDS=FALSE,
                            sav_dat=TRUE,
                            dataset_nm_vector=NULL) {
  
  
  checkmate::assert_choice(str_to_lower(aps_lfs), choices = c("aps","lfs"))
  
  
  # Relevant names
  temp_year <- paste0("20",gsub("(\\w{4}_\\w{2})(\\d{2})(.*)","\\2", dataset_nm_vector[dta_num]))
  temp_name <- regmatches(dataset_nm_vector[dta_num],regexpr("\\w{4}_\\w{2}\\d{2}",dataset_nm_vector[dta_num]))
  
  if (loadRDS==FALSE) {
    if (sav_dat==FALSE) { #if tabulated data
      temp_dta <- read.table(file = paste0(INPUT,"\\",dataset_nm_vector[dta_num],".tab"),
                             header = TRUE) 
    }
    else { #otherwise load SPSS datasets
      temp_dta <-  read_sav(paste0(INPUT,"\\",dataset_nm_vector[dta_num],".sav"))
    }
    
    # Save relevant weight in own column 
    if (str_to_lower(aps_lfs)=="aps") {
      if (temp_year %in% c(2012:2019)) {
        temp_dta <- temp_dta %>%
          mutate(weight_val = PHHWTA18,
                 weight_var = "PHHWTA18")
      } else if (temp_year %in% c(2020:2022)) {
        temp_dta <- temp_dta %>%
          mutate(weight_val = PHHWTA22,
                 weight_var = "PHHWTA22")
      }
    } else if (str_to_lower(aps_lfs)=="lfs") {
      if (temp_year %in% c(2010:2011)) {
        temp_dta <- temp_dta %>%
          mutate(weight_val = PHHWT14,
                 weight_var = "PHHWT14")
      } else  if (temp_year %in% c(2012:2019)) {
        temp_dta <- temp_dta %>%
          mutate(weight_val = PHHWT18,
                 weight_var = "PHHWT18")
      } else if  (temp_year %in% c(2020:2022)) {
        temp_dta <- temp_dta %>%
          mutate(weight_val = PHHWT22,
                 weight_var = "PHHWT22")
      }
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
  
  temp_list <- list("dta"=temp_dta,"name"=temp_name,"year"=temp_year)
  return(temp_list)
}

# Function to adjust the data for our use, inspired by previous SPS code
recode_dta <- function(dta=NA) {
  
  # Check year and set SOC var accordingly
  dta_year_check <- dta %>% 
    group_by(dta_year) %>% 
    filter(row_number()==1) %>% 
    pull(dta_year)

  if (dta_year_check>2020) soc_var <- "SOC20M" else soc_var <- "SOC10M"  # for detailed occ
  
  # if (dta_year_check>=2022) {
  #   quals_var <- LEVQUL22_label
  # } else if (between(dta_year_check,2015,2021)) {
  #   quals_var <- LEVQUL15_label
  # }
  
  # Change data
  dta_adj <- dta %>% 
    mutate(ethnicity = case_when(ETHUKEUL == 1 ~ "White",
                                 ETHUKEUL %in% c(2,3,4,5,6,7,8,9) ~ "BAME",
                                 ETHUKEUL == -8 ~ "No answer",
                                 ETHUKEUL == -9 ~ "NA"),
           quarter_response = case_when(IOUTCOME %in% c(1,2) ~ "Yes",
                                        IOUTCOME == 6 ~ "No",
                                        TRUE ~ "NA"),
           industry_job = case_when(INDS07M %in% c(18,19) ~ 18, # group R arts and S other services, together
                                    TRUE ~ INDS07M), 
           # occ_job = floor(!!sym(soc_var)/1000), # forcing a one-digit SOC code
           # occ_job_two = floor(!!sym(soc_var)/100), # forcing a two-digit SOC code 
           fam_id = as.numeric(HSERIALP) *100 + FAMUNIT, #unique identifier for each family unit, ONLY EXISTS IN LFS!
           parent = case_when(RELHFU %in% c(1,2) & FDPCH19>0 ~ 1,
                              TRUE ~ 0),
           employed=case_when(ILODEFR==1 ~ 1,
                              TRUE ~ 0),
           unemployed=case_when(ILODEFR==2 ~ 1,
                              TRUE ~ 0),
           inactive=case_when(ILODEFR==3 ~ 1,
                              TRUE ~ 0),
           econ_active=case_when(ILODEFR %in% c(1,2) ~ 1,
                                 TRUE ~ 0),
           adult = case_when(AGE>=16 ~ 1,
                             TRUE ~ 0),
           adult1664 = case_when(between(AGE,16,64) ~ 1,
                             TRUE ~ 0),
           london_resident = case_when(GOVTOF == 8 ~ 1,
                                       TRUE ~ 0),
           manchester_resident = case_when(GOVTOR == 3 ~ 1,
                                           TRUE ~ 0),
           birmingham_resident = case_when(GOVTOR == 10 ~ 1,
                                           TRUE ~ 0),
           age_group = case_when(between(AGE,16,17) ~ "Aged 16-17",
                                 between(AGE,18,24) ~ "Aged 18-24", 
                                 between(AGE,25,34) ~ "Aged 25-34",
                                 between(AGE,35,49) ~ "Aged 35-49",
                                 between(AGE,50,64) ~ "Aged 50-64",
                                 AGE>64 ~ "Age 65+"),
           # famtype = case_when(FUTYPE6 %in% c("1 person - male","1 person - female") ~ "1 person",
           #                     FUTYPE6 %in% c("Male lone parent with dep children","Female lone par with dep children") ~ "Lone parent with dep children",
           #                     FUTYPE6 %in% c("Married couple with dep children","Cohab couple with dep children",
           #                                    "Same sex cohab couple, dep children","Civil Partners with dep children") ~ "Couple with dep children",
           #                     FUTYPE6 %in% c("Married couple, non-dep children only","Cohab couple with non-dep children only",
           #                                    "Male lone par with non-dep chldren only","Female lone par, non-dep chldren only",
           #                                    "Same sex cohab couple, non-dep children","Civil Partners, non-dep children only") ~ "Lone parent or couple with non-dependent children only",
           #                     FUTYPE6 %in% c("Married couple with no children","Cohab couple with no children",
           #                                    "Same sex cohabiting couple, no children","Civil Partners with no children") ~ "Couple with no children",
           #                     TRUE ~ NA_character_)
           # famtype = case_when(FUTYPE6 %in% c(1,2) ~ "1 person",
           #                     FUTYPE6 %in% c(10,12) ~ "Lone parent with dep children",
           #                     FUTYPE6 %in% c(6,9,16,19) ~ "Couple with dep children",
           #                     FUTYPE6 %in% c(5,8,11,13,15,18) ~ "Lone parent or couple with non-dependent children only",
           #                     FUTYPE6 %in% c(4,7,14,17) ~ "Couple with no children",
           #                     TRUE ~ NA_character_),
           famtype = case_when(FUTYPE6 %in% c(1,2,10,11,12,13) ~ "1 person",
                               FUTYPE6 %in% c(6,9,16,19,4,7,14,17,5,8,15,18) ~ "Couple",
                               TRUE ~ NA_character_),
           wfh_d = case_when(HOME %in% c(1,2,3) ~ 1,
                             HOME == 4 ~ 0,
                             TRUE ~ NA_real_),
           pt_d = case_when(FTPT %in% c(1,3) ~ 0, #if working full-time
                            FTPT %in% c(2,4) ~ 1, #working part time
                            TRUE ~ NA_real_),
           age_child = case_when(AYFL19 <= 2 ~ "2 yrs or less", #age of youngest child
                                 between(AYFL19,3,4) ~ "3-4 yrs",
                                 AYFL19 > 4 ~ "More then 4 yrs"),
           num_children = FDPCH16,
           #levquals = !!sym(quals_var)
           ) 
  
  
  
  return(dta_adj)
  
}

# Duplicate the rows to have a UK total in addition to London and RoUK
dup_dataset <- function(dta=NA) {
  dup_dta <- dta %>% 
    uncount(2, .id="row_version") %>% 
    mutate(london_resident = case_when(row_version == 2 ~ 2,
                                       TRUE ~ london_resident))
  
  return(dup_dta)
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
    lfsh_wt <- dataset_list_adj[[dta_nm]] %>%
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
    lfsh_wt <- dta %>%
      mutate(across({{agg_vars}}, ~ 9999)) %>%
      left_join(london_wt_dta,by=c("london_worker","ILODEFR")) %>%
      left_join(uk_wt_dta,by=c("ILODEFR")) %>%
      mutate(weight_val_ldn = weight_val * uprate_weight_ldn,
             weight_val_uk = weight_val * uprate_weight_uk)
  }
  
  # Note: to use a vector of strings as variables, need to use across(all_of(sum_group_vars) below
  
  
  # Only interested in quarter_response=="Yes" & ILODEFR==1, but keep all for data checking
  lfsh_sum <- lfsh_wt %>% 
    group_by(quarter_response,across(all_of(sum_group_vars)),across(all_of(default_group_vars)),across(all_of(nte_var)),across(all_of(agg_vars)),dta_year,uprate_weight_ldn,weight_var) %>% 
    summarise(wt_pop=sum(weight_val_ldn),
              unwt_pop=n()) %>% 
    group_by(quarter_response,across(all_of(sum_group_vars)),across(all_of(default_group_vars)),across(all_of(agg_vars)),dta_year,uprate_weight_ldn,weight_var) %>% 
    mutate(share_wt_pop = wt_pop/sum(wt_pop),
           share_unwt_pop = unwt_pop/sum(unwt_pop)) %>% 
    group_by(quarter_response,across(all_of(default_group_vars)),across(all_of(nte_var)),across(all_of(agg_vars)),dta_year,uprate_weight_ldn,weight_var) %>% 
    mutate(share_wt_nte_across_pop = wt_pop/sum(wt_pop)) %>% # to see how many of nte workers are in each sum_grouping
    ungroup()
  
  temp_list <- list("lfsh_wt"=lfsh_wt,"lfsh_sum"=lfsh_sum)
  return(temp_list)
  
}

#.............................................................................
#### Utility functions ----
#.............................................................................

# Produce a dataframe with all variables and their labels across datasets

output_labels <- function(dta_nm=NULL) {
  
  # Extract year
  dta_year <- as.numeric(names(dataset_nm)[dataset_nm==dta_nm])
  
  # Remove user-made variables
  dta <- dataset_list[[dta_nm]] %>% select(-weight_var,-dta_year)
  
  # Create list with labels
  lablist <- lapply(dta, attr, "label")
  
  # Create dataframe of list
  labdf <- as.data.frame(lablist, stringsAsFactors = F) 
  
  return(labdf)
  
}

# Create new columns using value labels of existing ones
convert_to_label <- function(dta=NULL,
                             var_vec=c()) {
  dta_convert <- dta %>% 
    mutate(across(all_of(var_vec),sjlabelled::as_label,.names='{col}_label'))
  
  return(dta_convert)
}


# Collapse data to summarise by demography, using weights
collapse_func_ILO <- function(dta=NULL,
                          group_vec=c("GOVTOF_label","parent"),
                          demog_var=NULL) {
  
  dta_collapse <- dta %>% 
    filter(between(AGE,16,64)) %>% 
    group_by(!!sym(demog_var),across(all_of(group_vec)),ILODEFR) %>% 
    summarise(people = sum(weight_val)) %>% 
    pivot_wider(id_cols = all_of(c(group_vec,demog_var)),
                names_from = ILODEFR,
                values_from = people,
                names_prefix = "people_ILO_") %>% # pivot to have ILOs in columns
    mutate(people_tot = rowSums(across(starts_with("people_ILO"))),
           pct_employed = people_ILO_1/people_tot) # total % employed) 
  
  
  return(dta_collapse)
}

collapse_func <- function(dta=NULL,
                          group_vec=c("london_resident","parent"),
                          demog_vec=NULL,
                          rm.na = FALSE) {
  
  # Since some of the elements in the column name vectors may be emptry strings, particularly the lack of region,
  # ensure only nonempty vector elements are passed on
  by_cols <- c(group_vec,demog_vec) %>% 
    magrittr::extract(nzchar(.))
  
  dta_collapse <- dta %>% 
    filter(between(AGE,16,64) & !if_any(all_of(by_cols),is.na)) %>% # do not allow NA in variables
    group_by(across(all_of(by_cols))) %>% 
    summarise(people = sum(weight_val),
              obs_n = n(),
              .groups = "drop")
  
  return(dta_collapse)
}

# Function to lapply a survey means through a list of survey designs and produce element in list
lapply_svy_means <- function(svy_list_nm=NULL,
                             means_var=NULL,
                             by_formula=NULL) {
  
  means_var_form <- as.formula(paste0("~", deparse(substitute(means_var))))
  
  new_list_element <- lapply(X=svy_list_nm,
                             FUN= function(design,formula,by,com,keep.var,keep.names) 
                               svyby(design=design,formula=formula,by=by,FUN=com,keep.var=keep.var),
                             formula = means_var_form, 
                             by    = by_formula, 
                             com   = svymean, 
                             keep.var = TRUE)
  
  return(new_list_element)
   
}

# Do same function with map, supposedly less memory intensive
map_svy_means <- function(svy_list_nm=NULL,
                          means_var=NULL,
                          by_formula=NULL) {
  
  means_var_form <- as.formula(paste0("~", deparse(substitute(means_var))))
  
  new_list_element <- map(.x=svy_list_nm,
                          .f= svyby,
                          formula = means_var_form, 
                          by    = by_formula, 
                          FUN   = svymean,
                          na.rm = TRUE,
                          keep.var = TRUE)
  
  return(new_list_element)
  
}

# Function to delist the results, create ID column, and add suffix to unique vars (se)
delist_results <- function(list_nm = NULL,
                           suffix = NULL) {
  
  new_df <- bind_rows(lapply(list_nm,bind_rows,.id="dataset"),.id="var_set") %>%  
    remove_rownames() %>% 
    unite("byvar_characteristic",all_of(analysis_byvars_vec), sep='_',na.rm = TRUE,remove = FALSE) %>% 
    mutate(id = paste0(var_set,"_",dataset,"_",parent,"_",london_resident,"_",byvar_characteristic)) %>%
    rename_with(.fn = ~paste0(.,"_",suffix), .cols = c("se")) %>% 
    relocate(id)
  
  return(new_df)
}

