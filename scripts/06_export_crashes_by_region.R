### C. Muhs, cdm@dksassociates.com
### April 2018
### ODOT ARTS
### This script: make crash spreadsheets for each region
################################################################################

# Load libraries. Use install.packages("[package name]") to install them before loading
library(tidyverse)
library(xlsx) # may need to install Java 64-bit to use
library(here)

here() # init working path

# Read data
crash <- read_csv(here("data", "crashes2011-2015.csv")) # all the data
# crash_sample <- sample_n(crash, 1000)

# Make separate data frames for each region
mydfs <- split(crash, crash$reg_id)

# Export data frames
lapply(1:length(mydfs), function(i) write.csv(mydfs[[i]],
                                     file = paste0(names(mydfs[i]), ".csv"),
                                     row.names = FALSE))