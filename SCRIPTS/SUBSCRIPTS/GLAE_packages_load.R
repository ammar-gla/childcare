#_______________________________________________________________________________
# LOAD STANDARD PACKAGES ----
#_______________________________________________________________________________

# The code below can be used to download all packages first
# 
# install.packages(c("here","devtools","remotes","knitr","tidyverse","lubridate","scales",
#                    "ggplot2","ggthemes","nomisr","devtools","gglaplot","data.table",
#                    "janitor","ggrepel","plotly","leaflet","leafsync","rgdal","httr",
#                    "flextable","officer","extrafont","svDialogs","sf","reactable",
#                    "openxlsx","haven","survey","sjlabelled","jtools","huxtable","crayon","sjPlot",
#                    "broom.mixed"))
# The below are needed to install GLA packages the first time it is runs
devtools::install_github("ammar-gla/gglaplot")
#devtools::install_github("Greater-London-Authority/ldndatar", auth_token = "96e66bb601f49f62f0bb9bdcb73a849ece358ad1")
#remotes::install_github("wilkelab/ggtext")

library("here") # To set project folder dynamically
library("remotes") # Makes it possible to load github package where necesary
library("knitr") # Needed to knit markdown
library("tidyverse") # Whole tidyverse package
library("lubridate") # Tideyverse date manipulations
library("scales") # For scales in ggplots
library("ggplot2") # Used for making main charts
library("ggthemes") # Additional themes for ggplot [not sure if used]
library("nomisr") # Downloading data from Nomis
library("devtools") # Allows downloading ggla packages
library("gglaplot") # GLA plotting functions
library("data.table") # Data table utility
library("janitor") # Cleaning up date files
library("ggrepel") # Used to repel overlapping text in charts [used in gglaplot?]
library("plotly") # Interactive charts
library("leaflet") # Interactive maps
library("leafsync")# Syncing two maps next to each other
library("rgdal") # Needed for geospatial manipulations
library("httr") # Quering websites to check if data exists
library("flextable") # Summary headline stats table
library("officer") # for fp_text
library("extrafont") #Fonts for the flextable
library("svDialogs") #for pop-ups
library("sf") # for geometry
library("reactable") #For interactive tables
library("openxlsx") # Excel output
library("haven") # For loading SPSS data
library("survey") # For using survey weights
library("sjlabelled") # For converting value labels
library("jtools") # For outputting nice regression tables
library("huxtable") # outputting regression into excel
library("crayon") # for displaying texts in loops
library("sjPlot") # to generate overviews of labelled data
library("broom.mixed") # for coefficient plotting

