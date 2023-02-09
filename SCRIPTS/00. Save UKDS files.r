## NOTE: this assumes that the folders contain SPSS .sav files to extract!
## ALSO requires that you already have the INPUT folder within your project folder

# Shorten the names of the folders
all_in_folder <- "C:\\Users\\ALjubijankic\\Downloads\\try" #<--- This depends on where your files are
files_names <- list.files(all_in_folder)
new_names <- gsub("(\\d{4})(spss.*)(.zip)","\\1\\3",files_names)
#rename
file.rename(from=c(paste0(all_in_folder,"/",files_names)), to=c(paste0(all_in_folder,"/",new_names)))

# Unzip and copy out the datasets
for (fold in new_names) {
  
  # First unzip
  zipF<- paste0(all_in_folder,"/",fold,".zip")
  unzip(zipF, 
        exdir=paste0(here::here(INPUT),"/",fold),
        overwrite=TRUE) # NOTE: This will copy the unzipped folder to the project for documentation purposes
  
  # Extract the data file
  data_path <- paste0(here::here(INPUT),"/",fold,"/UKDA-",fold,"-spss/","spss/","spss24/")
  files_path <- list.files(data_path,full.names = TRUE)
  
  file.copy(from=files_path, to=paste0(here::here(INPUT),"/","data"), 
            recursive = TRUE,
            overwrite = TRUE)
  
}



file.copy(from=paste0(here::here(INPUT),"/*.sav"), to=paste0(INPUT,"data"), 
          recursive = TRUE,
          overwrite = TRUE,
          copy.mode = TRUE)
