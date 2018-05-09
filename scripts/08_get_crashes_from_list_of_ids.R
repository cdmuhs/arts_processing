### C. Muhs, cdm@dksassociates.com
### May 2018
### ODOT ARTS
### This script: return a list of crashes from list of ids from gis
################################################################################

# Load libraries. Use install.packages("[package name]") to install them before loading
library(tidyverse)
library(xlsx) # may need to install Java 64-bit to use
library(here)

here() # init working path

# Read data
crash <- read_csv(here("data", "crashes2011-2015.csv")) # all the data
# Rename kabco types
crash$kabco <- recode_factor(crash_tbl$kabco, fatal = "FAT", inj_a = "INJ A",
                             inj_b = "INJ B", inj_c = "INJ C", pdo = "PDO")
# Many crashes do not have street listed. Add value from recre_rd_name if missing
crash <- crash %>%
    mutate(st_full_nm = case_when(is.na(st_full_nm) ~ recre_rd_nm,
                                  TRUE ~ st_full_nm))
# Make jurisdiction variable. If there is a city name, use the city.
#   if there is no city name, assume unincorporated county area
crash <- crash %>%
    mutate(juris = case_when(is.na(city_sect_nm) ~ paste(cnty_nm, "County", sep = "_"), # if no city name...
                             TRUE ~ gsub('([[:punct:]])|\\s+','_', city_sect_nm))) # if there is city name, use it but with underscores not spaces
crash$juris <- gsub('([[:punct:]])|\\s+', '_', crash$juris) # Remove any remaining spaces in county name

#### join crash table to the ids of interest to reduce it to a short list ####
# read id list
my_ids <- read_csv("C://temp/ids_ashland.csv")
names(my_ids) <- "crash_id"
crash <- crash %>% 
    inner_join(my_ids) %>%
    arrange(st_full_nm, isect_st_full_nm, crash_dt)

######### Export #########
write.xlsx(crash, 
           file = here("outputs", paste(Sys.Date(), "Ashland_crashes.xlsx", sep = "_")))
