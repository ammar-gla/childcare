#_____________________________________________________________________________
### Export into Excel ----
#_____________________________________________________________________________

# Export into workbook with formatted sheets
wbname <- "Parental labour market rates.xlsx"
wb <- loadWorkbook(paste0(DATA_OUT,wbname))

data_sheets <- c("lf_rates_data")

# Check if sheets exists and delete data, otherwise create sheet
for (sht in data_sheets) {
  
  # Check if exists
  for (actual_sheets in names(wb)){
    sht_exists <- FALSE
    if (actual_sheets == sht) {
      sht_exists <- TRUE
    }
  }
  
  # If not exist
  if (sht_exists != TRUE) {
    addWorksheet(wb, sht, tabColour = "black")
  } else {
    deleteData(wb , sheet = sht,cols = 1:20, rows = 1:10000, gridExpand = TRUE)
  }
  
}

# Write to workbook
writeData(wb, sheet = "lf_rates_data",means_fulldata_df, colNames = T)
saveWorkbook(wb,paste0(DATA_OUT,wbname),overwrite = T)

