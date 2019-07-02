# Purpose: Set up repo and folders for analyis / visualizations
# Author: Tim Essam, PhD
# Date: 2019/06/14
# 


# Load packages, prep folders and paths -----------------------------------
pacman::p_load("tidyverse", "sf", "lubridate", "extrafont", "readxl", "purrr", "scales", "llamar", "haven", "vtable", "data.table", "survey")
 

dir.create("Data")
dir.create("Articles")
dir.create("Graphics")
dir.create("Dataout")
dir.create("Rscripts")
dir.create("docs")

# Set up file path shortcuts
datapath <- "Data"
dataout <- "Dataout"
gispath <- "Data/GHA_adm"
imagepath <- "Images"
rpath <- "Rscripts"


#Source helper functions
file_list <- list("strip_geom.R", 
                  "KEN_helper_functions.R")
file_list %>% 
  map(~source(file.path(rpath, .)))


