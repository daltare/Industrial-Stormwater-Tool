This is a draft of a tool that is intended to analyze statewide industrial stormwater monitoring data reported to the California State Water Resources Control Board, and assess the monitoring results relative to other indicators of impairment and pollution burden (e.g., 303d impaired water bodies, CalEnviroScreen scores). It could potentially be used to target inspections of facilities that discharge stormwater. 

## Application Location
The application is available at: https://daltare.shinyapps.io/Stormwater_Enforcement_Tool

If the link above doesn't work, the application is also available on an alternate site at: https://sccwrp.shinyapps.io/Stormwater_Enforcement_Tool-master (thanks to [SCCWRP](http://www.sccwrp.org/Homepage.aspx) for hosting this site)

## Data Sources
#### Stormwater Monitoring and Facility Information
Results of industrial stormwater effluent monitoring, as well as information about the industrial discharge facilities where that monitoring occurs, is retrieved from the datasets stored on the data.ca.gov portal, here:
- [Industrial Stormwater Effluent Monitoring Data](https://data.ca.gov/dataset/stormwater-%E2%80%93-regulatory-and-enforcement-actions-%E2%80%93-smarts/resource/fe4712db-015a-4e92-a13f)
- [Industrial Stormwater Discharger Facility Information](https://data.ca.gov/dataset/stormwater-%E2%80%93-regulatory-and-enforcement-actions-%E2%80%93-smarts/resource/a5f001af-abbb-4bc7-9196#{})

The data contained in those two datasets is from the California Water Resources Control Board's Stormwater Multiple Application and Report Tracking System (SMARTS). These two datasets are updated daily using data contained in files on the [SMARTS Public Access Interface](https://smarts.waterboards.ca.gov/smarts/faces/SwSmartsLogin.xhtml), which can be accessed by following the link to *View SW Data* → *Download NOI Data By Regional Board* → from the dropdown menu select *State Board*. The files used are:
- *Industrial Ad Hoc Reports - Parameter Data* (Monitoring Data)
- *Industrial Application Specific Data* (Facility Data)

The tools used to automatically upload the SMARTS data to the data.ca.gov portal at regular (daily) intervals are available here: https://github.com/daltare/SMARTS_DataPortal_Automation

#### 303d Impaired Waterbodies
303(d) impaired water body information is from the [Final 2012 California Integrated Report](https://www.waterboards.ca.gov/water_issues/programs/tmdl/integrated2012.shtml) (shapefiles are available under the *Data Download* tab, including [polygons](https://gispublic.waterboards.ca.gov/webmap/303d_2012/files/2012_Impaired_Polys_Final.zip) and [polylines](https://gispublic.waterboards.ca.gov/webmap/303d_2012/files/2012_Impaired_Lines_Final.zip), as well as tables with [listing comments](https://gispublic.waterboards.ca.gov/webmap/303d_2012/files/2012_USEPA_approv_303d_List_Final_20150807.xlsx) and [potential sources](https://gispublic.waterboards.ca.gov/webmap/303d_2012/files/2012_USEPA_approv_303d_List_Final_20150807wsrcs.xls)).

#### CalEnviroScreen 3.0
Pollution burden information by census tract is from the California Office of Environmental Health Hazard Assessment's (OEHHA) [CalEnviroScreen 3.0](https://oehha.ca.gov/calenviroscreen/report/calenviroscreen-30) (a shapefile is available under the *CalEnviroScreen 3.0 Data and Additional Materials* header, [here](https://oehha.ca.gov/media/downloads//ces3shp.zip)).