### C. Muhs, cdm@dksassociates.com
### March 2018
### ODOT ARTS
### This script: make detailed tables from systemic crash corridors
################################################################################

library(tidyverse)
library(xlsx)
library(here)
here() # init working path

# Load files from script 03
load(file = here("data", "crash_tbl_r5.Rdata"))
load(file = here("data", "outlist_r5.Rdata"))


table(crash_tbl_r5$juris)
