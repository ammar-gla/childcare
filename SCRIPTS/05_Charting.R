#_____________________________________________________________________________
### Charting ----
#_____________________________________________________________________________

#..............................................................................
#### Produce line charts with confidence intervals ----
#..............................................................................


for_charting <- empl_stats_fulldata_df %>% 
  mutate(date_day = ymd(paste0("20",substr(dataset,nchar("lfsh_aj18")-1,nchar("lfsh_aj18")),"-06-01")),
         parent = fct_recode(factor(parent), "Non-parents" = "0", "Parents" = "1"),
         london_resident = fct_recode(factor(london_resident,exclude = NULL), "London" = "1", "RoUK" = <NA>))
