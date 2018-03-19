### C. Muhs, cdm@dksassociates.com
### March 2018
### ODOT ARTS
### This script: make detailed tables from systemic crash corridors
################################################################################

library(tidyverse)
library(xlsx)
library(here)
here() # init working path

# Read data
crash <- read_csv(here("data", "crashes2011-2015.csv")) # all the data
# load(here("data", "crash_r5_all.Rdata")) # loads 'crash_r5_all

# Filter out crashes
crash_tbl <- crash %>% 
    filter(reg_id == 2) %>% # Region 2 only
    filter(is.na(rdwy_no)) # No state highways. (rdwy_no = "NA" when off state hwy)

# Rename kabco types
crash_tbl$kabco <- recode_factor(crash_tbl$kabco, fatal = "FAT", inj_a = "INJ A",
                                 inj_b = "INJ B", inj_c = "INJ C", pdo = "PDO")

# Make jurisdiction variable. If there is a city name, use the city.
#   if there is no city name, assume unincorporated county area
crash_tbl <- crash_tbl %>%
    mutate(juris = case_when(is.na(city_sect_nm) ~ paste(cnty_nm, "County", sep = "_"), # if no city name...
                             TRUE ~ gsub('([[:punct:]])|\\s+','_', city_sect_nm))) # if there is city name, use it but with underscores not spaces

# Many crashes do not have street listed. Add value from recre_rd_name if missing
crash_tbl <- crash_tbl %>%
    mutate(st_full_nm = case_when(is.na(st_full_nm) ~ recre_rd_nm,
                                  TRUE ~ st_full_nm))

# Add variables for whether the crash was certain types
crash_tbl <- crash_tbl %>%
    mutate(fat_crash = ifelse(kabco == "FAT", 1, 0)) %>%
    mutate(inj_a_crash = ifelse(kabco == "INJ A", 1, 0))

####### Part 3: Tables of Crashes on Systemic Corridors  ##############################

# Define columns of interest for systemic corridor location tables
key_cols <- c("crash_id", "juris", "cnty_nm", "city_sect_nm", "st_full_nm", 
              "isect_st_full_nm", "from_isect_dstnc_qty", "cmpss_dir_short_desc", 
              "rd_char_long_desc", "mp_no", "crash_typ_long_desc", 
              "collis_typ_long_desc", "traf_cntl_device_long_desc", "kabco",
              "alchl_invlv_flg", "drug_invlv_flg", "crash_speed_invlv_flg", 
              "crash_cause_1_long_desc", "fat_crash", "inj_a_crash")

# define function to make a corridor crash table.
#   inputs = name of jurisdiction name
#   outputs = table by corridor
makeCrashTableFromSystemic = function(input_name){
    # first table -- hotspot
    mytable <- crash_tbl %>%  # define new table
        select(key_cols) %>% # use the columns in 'keycols'
        filter(juris == input_name) # limit to input jurisdiction name
    # second table -- systemic corridors
    my_corridor_table <- mytable %>% 
        group_by(st_full_nm) %>% # rearrange table by corridor
        # summarize by totals
        summarize(Total_Crashes = n(),
                  Fatal = sum(fat_crash),
                  Severe_Injury = sum(inj_a_crash)
        ) %>%
        filter(Total_Crashes > 1) %>%
        filter(Fatal > 0 | Severe_Injury > 0)
    # now use corridor names to filter the first table by streets in the second
    mytable_sum <- mytable %>%
        filter(st_full_nm %in% my_corridor_table$st_full_nm) %>%
        select(-(fat_crash:inj_a_crash)) %>% # remove binary crash type variables
        select(-(juris)) %>% # remove jurisdiction 
        arrange(st_full_nm, isect_st_full_nm) # sort by street name
    return(mytable_sum)
}

# makeCrashTableFromSystemic("Baker_City") # test

# make list of jurisdictions
mylist = unique(crash_tbl$juris)

# iterate over jurisdictions
result <- lapply(mylist, makeCrashTableFromSystemic)
# result

# Assign 'NA' value if no fatal or inj A corridors in jurisdiction
for (i in 1:length(result)) {
    if (nrow(result[[i]]) == 0) {
        result[[i]] = NA
    }
}

# result <- result[!is.na(result)] # remove "NA" list elements

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

# R2 has list we aren't supposed to do. These ones we are supposed to do.
r2_list <- c("Astoria", "Seaside", "St__Helens", "Rainier", "Newberg",
             "Lincoln_City", "Lebanon", "Sweet_Home", "Cottage_Grove", "Stayton", 
             "Dallas", "Independence")
paste(as.character(r2_list),collapse=", ",sep="")

# Write excel file with a sheet for each jurisdiction
options(java.parameters = "-Xmx4g")
save.xlsx(paste(Sys.Date(), "r2_crashes_on_systemic_corridors_allr2_5.xlsx", sep = "_"), 
          Springfield, St__Helens, Stayton, Sweet_Home, Tillamook, Tillamook_County, 
          Toledo, Veneta, Warrenton, Washington_County, Westfir, Willamina, Woodburn, Yamhill_County)

############# END ###############