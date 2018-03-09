### C. Muhs, cdm@dksassociates.com
### March 2018
### ODOT ARTS
# This script: Read geodatabase data
################################################################################

library(rgdal)
library(here)
library(tidyverse)

here() # set the working directory

path_11 <- here("data", "crashes2011.gdb")  # get these crashes from ftp://ftp.odot.state.or.us/tdb/trandata/GIS_data/Safety/ 
path_12 <- here("data", "crashes2012.gdb") 
path_13 <- here("data", "crashes2013.gdb") 
path_14 <- here("data", "crashes2014.gdb") 
path_15 <- here("data", "crashes2015.gdb") 

# check db 
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(path_11) # "crashes[YYYY]"
print(fc_list)

# read feature class
cr_11 <- readOGR(dsn = path_11, layer = "crashes2011")
cr_12 <- readOGR(dsn = path_12, layer = "crashes2012")
cr_13 <- readOGR(dsn = path_13, layer = "crashes2013")
cr_14 <- readOGR(dsn = path_14, layer = "crashes2014")
cr_15 <- readOGR(dsn = path_15, layer = "crashes2015")

# make R tbl_df (like a data frame)
cr_11_df <- tbl_df(cr_11); rm(cr_11)
cr_12_df <- tbl_df(cr_12); rm(cr_12)
cr_13_df <- tbl_df(cr_13); rm(cr_13)
cr_14_df <- tbl_df(cr_14); rm(cr_14)
cr_15_df <- tbl_df(cr_15); rm(cr_15)

# check dimensions
dim(cr_11_df); dim(cr_12_df); dim(cr_13_df); dim(cr_14_df); dim(cr_15_df)
# identical(colnames(cr_14_df), colnames(cr_15_df))

# stack data frames together
cr <- rbind(cr_11_df, cr_12_df, cr_13_df, cr_14_df, cr_15_df)

# convert column names to lowercase
colnames(cr) <- tolower(colnames(cr))

# Define KABCO var
cr$kabco <- "pdo"
cr$kabco[cr$tot_inj_lvl_c_cnt > 0] <- "inj_c"
cr$kabco[cr$tot_inj_lvl_b_cnt > 0] <- "inj_b"
cr$kabco[cr$tot_inj_lvl_a_cnt > 0] <- "inj_a"
cr$kabco[cr$tot_fatal_cnt > 0] <- "fatal"
summary(as.factor(cr$kabco))

# save all crashes as csv
out_path <- here("data", "crashes2011-2015.csv")
write_csv(cr,
           path = out_path)

# Save file of just R5
crash_r5_all <- crash %>% 
    filter(reg_id == 5)
save(crash_r5_all, file = here("data", "crash_r5_all.Rdata"))
