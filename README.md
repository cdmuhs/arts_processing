# ARTS processing

Tools to automate export of crash data.

## Script description

See ~\scripts

* 01-read_gdb_data.R - read geodatabase files, compile crash tables, export as csv & rdata
* 02-make_spot_loc_tables.R - make crash hotspot location tables for an input region
* 03-make_systemic_corridor_tables.R - make systemic crash corridor tables for an input region
* 04-make_systemic_crash_list_tables.R - make crash hotspot-style tables from the set of systemic crash corridors
* 05-list_safe_juris.R - list the jurisdictions in an input region without fatal or inj A crashes
* 99-check_col_lengths.R - just a simple check on # columns for each year of crash data