This is a draft of a tool that is intended to analyze statewide industrial stormwater monitoring data reported to the California State Water Resources Control Board, and assess the monitoring results relative to other indicators of impairment and pollution burden (e.g., 303d impaired water bodies, CalEnviroScreen scores). It could potentially be used to target inspections of facilities that discharge stormwater. 

## Application Location
The application is available at: https://daltare.shinyapps.io/Stormwater_Enforcement_Tool

If the link above doesn't work, the application is also available on an alternate site at: https://sccwrp.shinyapps.io/Stormwater_Enforcement_Tool-master (thanks to [SCCWRP](http://www.sccwrp.org/Homepage.aspx) for hosting this site)

## Data Sources
Stormwater monitoring data used in this tool is from the [SMARTS](https://smarts.waterboards.ca.gov/smarts/faces/SwSmartsLogin.xhtml) Public Access interface. To access the files used, go to *View SW Data* → *Download NOI Data By Regional Board* → from the dropdown menu select *State Board*. The files used are:
- *Industrial Ad Hoc Reports - Parameter Data* (Monitoring Data)
- *Industrial Application Specific Data* (Facility Data)

303(d) impaired water body information is from the [Final 2012 California Integrated Report](https://www.waterboards.ca.gov/water_issues/programs/tmdl/integrated2012.shtml) (shapefiles are available under the *Data Download* tab, including [polygons](https://gispublic.waterboards.ca.gov/webmap/303d_2012/files/2012_Impaired_Polys_Final.zip) and [polylines](https://gispublic.waterboards.ca.gov/webmap/303d_2012/files/2012_Impaired_Lines_Final.zip), as well as tables with [listing comments](https://gispublic.waterboards.ca.gov/webmap/303d_2012/files/2012_USEPA_approv_303d_List_Final_20150807.xlsx) and [potential sources](https://gispublic.waterboards.ca.gov/webmap/303d_2012/files/2012_USEPA_approv_303d_List_Final_20150807wsrcs.xls)).

Pollution burden information by census tract is from the California Office of Environmental Health Hazard Assessment's (OEHHA) [CalEnviroScreen 3.0](https://oehha.ca.gov/calenviroscreen/report/calenviroscreen-30) (a shapefile is available under the *CalEnviroScreen 3.0 Data and Additional Materials* header, [here](https://oehha.ca.gov/media/downloads//ces3shp.zip)).