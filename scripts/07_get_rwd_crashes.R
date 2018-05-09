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
crash$juris <- gsub('([[:punct:]])|\\s+', '_', crash_tbl$juris) # Remove any remaining spaces in county name

# Make a df for crashes of interest
crash_tbl <- crash %>% 
    filter(reg_id == 3) %>% # Region 3
    filter(is.na(rdwy_no)) %>% # No state highways. (rdwy_no = "NA" when off state hwy)
    # filter(juris == "Jackson_County") %>%
    # filter(st_full_nm %in% c("BLACKWELL RD", "PIONEER RD", "REESE CR RD", 
    #                          "MEADOWS RD", "EAGLE MILL RD", "E ANTELOPE RD", 
    #                          "KERSHAW RD", "TILLER TRAIL HWY 227", "FOOTHILL BLVD", 
    #                          "HILLCREST RD", "GIBBON RD", "GRIFFIN LN", 
    #                          "UPPER RIVER RD")) %>%
    # @CDM: remove crashes coded intersection or driveway or alley
    filter(juris == "Douglas_County") %>%
    filter(st_full_nm %in% c("LOOKINGGLASS RD", "BUCKHORN RD A", "OLD HWY 99 SOUTH",
                             "MELQUA RD", "TILLER TRAIL HWY")) %>%
    mutate(rd_crash = ifelse((crash_typ_cd %in% c("&", "8", "9")), 1, 0)) %>% # roadway departure
    filter(rd_crash == 1) %>% # filter on roadway departure
    arrange(st_full_nm, crash_dt, crash_id)

######### Export #########
write.xlsx(crash_tbl, 
           file = here("outputs", paste(Sys.Date(), "r3_Douglas_RwD_crashes.xlsx", sep = "_")))
