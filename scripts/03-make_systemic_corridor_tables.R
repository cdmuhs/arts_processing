### C. Muhs, cdm@dksassociates.com
### Apr 2018
### ODOT ARTS
### This script: make systemic crash corridor tables
################################################################################

# Load libraries. Use install.packages("[package name]") to install them before loading
library(tidyverse)
library(xlsx) # may need to install Java 64-bit to use
library(here)

here() # init working path

# Read data
crash <- read_csv(here("data", "crashes2011-2015.csv")) # all the data
# load(here("data", "crash_r5_all.Rdata")) # loads 'crash_r5_all

# Filter out crashes
crash_tbl <- crash %>% 
    filter(reg_id == 1) %>% # Region 1 only
    filter(is.na(rdwy_no)) # No state highways. (rdwy_no = "NA" when off state hwy)

# Rename kabco types
crash_tbl$kabco <- recode_factor(crash_tbl$kabco, fatal = "FAT", inj_a = "INJ A",
                                inj_b = "INJ B", inj_c = "INJ C", pdo = "PDO")

# Make jurisdiction variable. If there is a city name, use the city.
#   if there is no city name, assume unincorporated county area
crash_tbl = crash_tbl %>%
    mutate(juris = case_when(is.na(city_sect_nm) ~ paste(cnty_nm, "County", sep = "_"), # if no city name...
                             TRUE ~ gsub('([[:punct:]])|\\s+','_', city_sect_nm))) # if there is city name, use it but with underscores not spaces
crash_tbl$juris <- gsub('([[:punct:]])|\\s+', '_', crash_tbl$juris) # Remove any remaining spaces in county name


# Many crashes do not have street listed. Add value from recre_rd_name if missing
crash_tbl <- crash_tbl %>%
    mutate(st_full_nm = case_when(is.na(st_full_nm) ~ recre_rd_nm,
                                  TRUE ~ st_full_nm))

# Add variables for whether the crash was certain types
crash_tbl <- crash_tbl %>%
    mutate(bike_crash = ifelse(crash_typ_long_desc == "Pedalcyclist", 1, 0)) %>%
    mutate(ped_crash = ifelse(crash_typ_long_desc == "Pedestrian", 1, 0)) %>%
    mutate(int_crash = ifelse(rd_char_long_desc == "Intersection", 1, 0)) %>%
    mutate(rd_crash = ifelse((crash_typ_cd %in% c("&", "8", "9")), 1, 0)) %>%
    mutate(fat_crash = ifelse(kabco == "FAT", 1, 0)) %>%
    mutate(inj_a_crash = ifelse(kabco == "INJ A", 1, 0))

# summary(crash_tbl$bike_crash); summary(crash_tbl$ped_crash)
# summary(crash_tbl$rd_crash); summary(crash_tbl$int_crash)

####### Part 2: Systemic Corridors ##############################

# Define columns of interest for systemic corridor location tables
key_cols <- c("juris", "cnty_nm", "city_sect_nm", "st_full_nm", "kabco", 
              "bike_crash", "ped_crash", "int_crash", "rd_crash", 
              "fat_crash", "inj_a_crash")

# define function to make a corridor crash table.
#   inputs = name of jurisdiction name
#   outputs = table by corridor
makeCorridorTable = function(input_name){
    mytable <- crash_tbl %>%  # define new table
        select(key_cols) %>% # use the columns in 'keycols'
        filter(juris == input_name) %>% # limit to input jurisdiction name
        select(-(juris)) # now remove jurisdiction column from the table
    
    my_corridor_table <- mytable %>% # define new second table
        group_by(st_full_nm) %>% # rearrange table by corridor
        # summarize by totals
        summarize(Total_Crashes = n(),
                  Fatal = sum(fat_crash),
                  Severe_Injury = sum(inj_a_crash),
                  Pedestrian = sum(ped_crash),
                  Bicycle = sum(bike_crash),
                  Intersection = sum(int_crash),
                  Roadway_Departure = sum(rd_crash)
        ) %>%
        filter(Total_Crashes > 1) %>%
        filter(Fatal > 0 | Severe_Injury > 0)
    return(my_corridor_table)
}

# makeCorridorTable("Hood_River_County") # test

# make list of jurisdictions
mylist = unique(crash_tbl$juris)

# iterate over jurisdictions
result <- lapply(mylist, makeCorridorTable)

# Assign 'NA' value if no fatal or inj A corridors in jurisdiction
for (i in 1:length(result)) {
    if (nrow(result[[i]]) == 0) {
        result[[i]] = NA
        }
}

# Get list of street names
# st_names <- sapply(result, '[[', 1)
# str(st_names)
# summary(st_names)

# Commit non-NA list items to objects in working environment
for (i in 1:length(result)) {
    assign(mylist[i], result[[i]])
    print(paste("Object ", mylist[i], "added to environment."))
}

# Remove crash data frames from environment
rm(crash_tbl, crash)

########### # Export to excel ##########################

# Define function to save multiple data frames in workbook
#   inputs = output file; data frame objects in memory
#   outputs = excel file
save.xlsx <- function (file, ...)
{
    require(xlsx, quietly = TRUE)
    objects <- list(...)
    fargs <- as.list(match.call(expand.dots = TRUE))
    objnames <- as.character(fargs)[-c(1, 2)]
    nobjects <- length(objects)
    for (i in 1:nobjects) {
        if (i == 1)
            write.xlsx(objects[[i]], file, sheetName = objnames[i])
        else write.xlsx(objects[[i]], file, sheetName = objnames[i],
                        append = TRUE)
    }
    print(paste("Workbook", file, "has", nobjects, "worksheets."))
}

# Get list of data frames in memory. Put names into save.xlsx() below.
outlist <- sort(names(Filter(isTRUE, eapply(.GlobalEnv, is.data.frame))))
paste(as.character(outlist),collapse=", ",sep="")

# # R2 has list we aren't supposed to do. These ones we are supposed to do.
# r2_list <- c("Astoria", "Seaside", "St__Helens", "Rainier", "Newberg",
#              "Lincoln_City", "Lebanon", "Sweet_Home", "Cottage_Grove", "Stayton", 
#              "Dallas", "Independence")
# paste(as.character(r2_list),collapse=", ",sep="")

# Write excel file with a sheet for each jurisdiction
save.xlsx(here("outputs", paste(Sys.Date(), "r1_systemic_corridors.xlsx", sep = "_")),
          Barlow, Beaverton, Canby, Cascade_Locks, Clackamas_County, Cornelius, 
          Damascus, Estacada, Fairview, Forest_Grove, Gladstone, Gresham, 
          Happy_Valley, Hillsboro, Hood_River, Hood_River_County, King_City, 
          Lake_Oswego, Maywood_Park, Milwaukie, Molalla, Multnomah_County, 
          Oregon_City, Portland_Bridges, Portland_E__Burnside, Portland_N, 
          Portland_NE, Portland_NW, Portland_SE, Portland_SW, Portland_W__Burnside, 
          Sandy, Sherwood, Tigard, Troutdale, Tualatin, Washington_County, 
          West_Linn, Wilsonville, Wood_Village)

############# END ###############