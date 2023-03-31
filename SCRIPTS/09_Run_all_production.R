#_______________________________________________________________________________
###  RUN ALL SCRIPTS ----
#_______________________________________________________________________________
  
  # Put in your Nomis API key (NB: THE BELOW IS AMMAR'S KEY!!)
  Sys.setenv(NOMIS_API_KEY = "0x01a88c6659d20042f087de2e585cdf3a07708983")

  # ACTION: set whether to re-download all datasets, even if already exists
  redownload_all <- FALSE

  # HERE package needed for dynamic pathfinding
  library("here") 
  
  ## If running on whole population or parents, should generally be FALSE
  whole_pop_output <- FALSE

#...............................................................................
#### Run LMU scripts ----
#...............................................................................

  # Create paths as strings
  source(here("SCRIPTS","SUBSCRIPTS","GLAE_paths.r"))
  
  # Data packages
  source(paste0(SUBSCRIPTS,"GLAE_packages_load",".r"))

  # Inputs such as borough codes in Nomis
  source(paste0(SUBSCRIPTS,"GLAE_data_presets",".r"))
  
  # Run the subscripts necessary for markdown
  source(paste0(SUBSCRIPTS,"GLAE_functions_load",".r"))
  
  # Main data
  source(paste0(SCRIPTS,"01_Functions_and_setup",".r"))
  
  source(paste0(SCRIPTS,"02_Dataload",".r"))
  
  source(paste0(SCRIPTS,"03_Setup",".r"))
  
  #source(paste0(SCRIPTS,"04_Sumstats",".r"))
  
  source(paste0(SCRIPTS,"05_Regressions",".r"))
  
  #source(paste0(SCRIPTS,"06_Output",".r"))

  
  
  
  