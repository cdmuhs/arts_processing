### C. Muhs, cdm@dksassociates.com
### March 2018
### ODOT ARTS
### This script: Wrangle crash data 
################################################################################

# Load libraries. Use install.packages("[package name]") to install them before loading
library(tidyverse)
library(xlsx) # may need to install Java 64-bit to use
library(here)

here() # init working path

# Read data
# crash <- read_csv(here("data", "crashes2011-2015.csv")) # all the data
load(here("data", "crash_r5_all.Rdata")) # loads 'crash_r5_all


# Filter out crashes
crash_r5 <- crash_r5_all %>% 
    filter(reg_id == 5) %>% # Region 5 only
    filter(kabco %in% c("inj_a", "fatal")) %>% # Fatal & injury A only
    filter(is.na(rdwy_no)) # No state highways. (rdwy_no = "NA" when off state hwy)

# Rename kabco types
crash_r5$kabco <- recode_factor(crash_r5$kabco, fatal = "FAT", inj_a = "INJ A")


# Make jurisdiction variable. If there is a city name, use the city.
#   if there is no city name, assume unincorporated county area
crash_r5 = crash_r5 %>%
    mutate(juris = case_when(is.na(city_sect_nm) ~ paste(cnty_nm, "County", sep = "_"), # if no city name...
                             TRUE ~ gsub('([[:punct:]])|\\s+','_', city_sect_nm))) # if there is city name, use it but with underscores not spaces

####### Part 1: Spot locations ##############################

# Define columns of interest for spot location tables
key_cols <- c("crash_id", "juris", "cnty_nm", "city_sect_nm", "st_full_nm", 
              "isect_st_full_nm", "from_isect_dstnc_qty",
              "cmpss_dir_short_desc", "rd_char_long_desc", "mp_no", "crash_typ_long_desc", 
              "collis_typ_long_desc", "traf_cntl_device_long_desc", "kabco",
              "alchl_invlv_flg", "drug_invlv_flg", "crash_speed_invlv_flg", "crash_cause_1_long_desc")

# colnames(crash_r5)

# define function to make a spot location crash table.
#   inputs = name of jurisdiction name
#   outputs = table of spot location table
makeSpotTable = function(input_name){
    mytable <- crash_r5 %>%  # define new table
        select(key_cols) %>% # use the columns in 'keycols'
        filter(juris == as.character(input_name)) %>% # limit to input jurisdiction name
        select(-(juris)) # now remove jurisdiction column from the table
    return(mytable)
}

# makeSpotTable("Baker_County") # test

# make list of county names
mylist = unique(crash_r5$juris)

# iterate over counties
result <- lapply(mylist, makeSpotTable)

# Alternative: Use split function to split data frame by jurisdiction
# result <- split(crash_r5, crash_r5$cnty_nm)

# Commit list items to objects in working environment
for (i in 1:length(result)) {
    assign(mylist[i], result[[i]])
    print(paste("Object ", mylist[i], "added to environment."))
}

# Remove crash data frames from environment
rm(crash_r5_all, crash_r5)

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

# Write excel file with a sheet for each jurisdiction
save.xlsx("r5_spot.xlsx", 
          Baker_City, Baker_County, Boardman, Burns, Grant_County, Harney_County, 
          Hermiston, Irrigon, Joseph, La_Grande, Malheur_County, Milton_Freewater,
          Morrow_County, Nyssa, Ontario, Pendleton, Umatilla, Umatilla_County, 
          Union_County, Wallowa_County, Weston)

# save.xlsx("c://temp/r5_spot_test.xlsx",
#           Baker_City, Baker_County, Boardman, Burns, Grant_County, Harney_County, Hermiston, Irrigon, Joseph, La_Grande, Malheur_County, Milton_Freewater, Morrow_County, Nyssa, Ontario, Pendleton, Umatilla, Umatilla_County, Union_County, Wallowa_County, Weston)
