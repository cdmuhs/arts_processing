# ARTS processing

Tools to automate transformations and reports of crash data for ARTS. T

## Data needs

This repository does not contain the input data needed to run the scripts.  
The crash data were retrieved from ODOT. Separate geodatabase files are available [here: ftp://ftp.odot.state.or.us/tdb/trandata/GIS_data/Safety/](ftp://ftp.odot.state.or.us/tdb/trandata/GIS_data/Safety/). Retrieve `crashes2011.gdb` through `crashes2015.gdb`.

## Script description

See `~\scripts` folder

Use the `.Rproj` file to open as an R project. Then, in Rstudio, open scripts from the "file" pane.  

The scripts are ordered sequentially. They should be executed in order. 

* `01-read_gdb_data.R` - read geodatabase files, compile 2011-2015 crash tables, export as csv & rdata. **Warning:** processing the `gdb` files take a while.
* `02-make_spot_loc_tables.R` - make crash hotspot location tables for an input region
* `03-make_systemic_corridor_tables.R` - make systemic crash corridor tables for an input region
* `04-make_systemic_crash_list_tables.R` - make crash hotspot-style tables from the set of systemic crash corridors
* `05-list_safe_juris.R` - list the jurisdictions in an input region without fatal or inj A crashes

## Instructions

In scripts 02 through 05, modify line 21 `filter(reg_id == 2) %>%` and change `2` to the number of the region of interest.

You'll also need to modify the arguments in the `save.xlsx(paste(Sys.Date()), ...)` function statement at about line 115:
* The file name
* The names of the jurisdictions to include (this will show up in the console from line ~104 `paste(as.character(outlist))`