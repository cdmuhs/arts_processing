### C. Muhs, cdm@dksassociates.com
### Apr 2018
### ODOT ARTS
### This script: list of jurisdictions w/o fatal or inj A crashes
################################################################################

# Load libraries. Use install.packages("[package name]") to install them before loading
library(tidyverse)
library(xlsx) # may need to install Java 64-bit to use
library(here)

here() # init working path

# Read data
crash <- read_csv(here("data", "crashes2011-2015.csv")) # all the data

# Make jurisdiction variable. If there is a city name, use the city.
#   if there is no city name, assume unincorporated county area
crash <- crash %>%
    mutate(juris = case_when(is.na(city_sect_nm) ~ paste(cnty_nm, "County", sep = "_"), # if no city name...
                             TRUE ~ gsub('([[:punct:]])|\\s+','_', city_sect_nm))) # if there is city name, use it but with underscores not spaces
crash$juris <- gsub('([[:punct:]])|\\s+', '_', crash$juris) # Remove any remaining spaces in county name


# Filter out crashes
crash_tots <- crash %>% 
    filter(reg_id == 1) %>% # filter for region number
    filter(is.na(rdwy_no)) %>%
    group_by(juris) %>%
    summarize(num_of_crashes = n(),
              tot_fatal = sum(kabco=="fatal"),
              tot_inj_a = sum(kabco=="inj_a"),
              tot_inj_b = sum(kabco=="inj_b"),
              tot_inj_c = sum(kabco=="inj_c"),
              tot_pdo = sum(kabco=="pdo")
              ) %>%
    filter(tot_fatal == 0 & tot_inj_a == 0) 

# print list of jurisdictions with no fatal and no inj a
crash_tots$juris
paste(as.character(crash_tots$juris),collapse=", ",sep="")
