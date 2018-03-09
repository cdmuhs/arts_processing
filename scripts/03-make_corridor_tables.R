### C. Muhs, cdm@dksassociates.com
### March 2018
### ODOT ARTS
### This script: make crash corridor tables
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
    filter(is.na(rdwy_no)) # No state highways. (rdwy_no = "NA" when off state hwy)

# Rename kabco types
crash_r5$kabco <- recode_factor(crash_r5$kabco, fatal = "FAT", inj_a = "INJ A",
                                inj_b = "INJ B", inj_c = "INJ C", pdo = "PDO")


# Make jurisdiction variable. If there is a city name, use the city.
#   if there is no city name, assume unincorporated county area
crash_r5 = crash_r5 %>%
    mutate(juris = case_when(is.na(city_sect_nm) ~ paste(cnty_nm, "County", sep = "_"), # if no city name...
                             TRUE ~ gsub('([[:punct:]])|\\s+','_', city_sect_nm))) # if there is city name, use it but with underscores not spaces

# Add variables for whether the crash was certain types
crash_r5 <- crash_r5 %>%
    mutate(bike_crash = ifelse(crash_typ_long_desc == "Pedalcyclist", 1, 0)) %>%
    mutate(ped_crash = ifelse(crash_typ_long_desc == "Pedestrian", 1, 0)) %>%
    mutate(int_crash = ifelse(rd_char_long_desc == "Intersection", 1, 0)) %>%
    mutate(rd_crash = ifelse((crash_typ_cd %in% c("&", "8", "9")), 1, 0)) %>%
    mutate(fat_crash = ifelse(kabco == "FAT", 1, 0)) %>%
    mutate(inj_a_crash = ifelse(kabco == "INJ A", 1, 0))

summary(crash_r5$bike_crash); summary(crash_r5$ped_crash)
summary(crash_r5$rd_crash); summary(crash_r5$int_crash)

####### Part 2: Corridors ##############################

# Define columns of interest for spot location tables
key_cols <- c("juris", "cnty_nm", "city_sect_nm", "st_full_nm", "kabco", 
              "bike_crash", "ped_crash", "int_crash", "rd_crash", 
              "fat_crash", "inj_a_crash")

# define function to make a corridor crash table.
#   inputs = name of jurisdiction name
#   outputs = table by corridor
makeCorridorTable = function(input_name){
    mytable <- crash_r5 %>%  # define new table
        select(key_cols) %>% # use the columns in 'keycols'
        filter(juris == input_name) %>% # limit to input jurisdiction name
        select(-(juris)) # now remove jurisdiction column from the table
    
    my_corridor_table <- mytable %>% 
        group_by(st_full_nm) %>% # rearrange table by corridor
        # summarize by totals
        summarize(Total_Crashes = n(),
                  Fatal = sum(fat_crash),
                  Severe_Injury = sum(inj_a_crash),
                  Pedestrian = sum(ped_crash),
                  Bicycle = sum(bike_crash),
                  Intersection = sum(int_crash),
                  Roadway_Departure = sum(rd_crash)
        )
    return(my_corridor_table)
}

makeCorridorTable("Baker_County") # test

# make list of jurisdictions
mylist = unique(crash_r5$juris)

# iterate over jurisdictions
result <- lapply(mylist, makeCorridorTable)

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
save.xlsx("r5_corridor.xlsx", 
          Adams, Athena, Baker_City, Baker_County, Boardman, Burns, 
          Canyon_City, Cove, Echo, Elgin, Enterprise, Grant_County, 
          Haines, Halfway, Harney_County, Heppner, Hermiston, Hines, 
          Huntington, Imbler, Ione, Irrigon, Island_City, John_Day, 
          Joseph, La_Grande, Malheur_County, Milton_Freewater, Morrow_County, 
          Mt__Vernon, North_Powder, Nyssa, Ontario, Pendleton, Pilot_Rock, 
          Prairie_City, Richland, Seneca, Stanfield, Umatilla, Umatilla_County, 
          Union, Union_County, Vale, Wallowa, Wallowa_County, Weston)

############# END ###############