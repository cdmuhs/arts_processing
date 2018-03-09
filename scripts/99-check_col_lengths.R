library(tidyverse)
library(readxl)

cr_2015 = read_csv("c://temp/gis/crashes2015/crashes2015_gdb.csv", n_max = 100)
ncol(cr_2015)


cr_arts = read_excel("c://temp/20180228_2011 to 2015.xlsx", sheet = "Raw 2011-2015", n_max = 100)

