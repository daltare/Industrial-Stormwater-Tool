# Load Packages (NOTE: most of the functions in the script below are called explicitly with the `package::function()` syntax, but listing the packages here to make it easy to see what packages are being used)
    # Shiny platform
        library(shiny) # the Shiny web application framework (https://shiny.rstudio.com) (https://cran.r-project.org/package=shiny)
    # General data analysis and transformation
        library(dplyr) # data transformation (http://dplyr.tidyverse.org/) (https://github.com/rstudio/cheatsheets/raw/master/data-transformation.pdf) (https://cran.r-project.org/package=dplyr)
        library(magrittr) # just for the ` %>% ` operator, used to make code more readable (http://magrittr.tidyverse.org/) (https://cran.r-project.org/package=magrittr)
        library(readr) # for reading data into R from various sources (in this case from text files and RDS objects) (https://cran.r-project.org/package=readr)
        library(tidyr) # for reshaping data (http://tidyr.tidyverse.org/) (https://github.com/rstudio/cheatsheets/raw/master/data-import.pdf) (https://cran.r-project.org/package=tidyr)
        library(tidyverse) # loads all of the tidyverse packages (including dplyr, magrittr, readr, tidyr) (https://www.tidyverse.org/) (https://cran.r-project.org/package=tidyverse)
    # Interactive objects for data entry and display
        library(DT) # interactive data tables (https://rstudio.github.io/DT/) (https://cran.r-project.org/package=DT)
        library(crosstalk) # interaction between html objects (in this case, used to link data table with map) (https://rstudio.github.io/crosstalk/) (https://cran.r-project.org/package=crosstalk)
        library(rhandsontable) # allows data entry by user, in a table format (https://jrowen.github.io/rhandsontable/) (https://cran.r-project.org/package=rhandsontable)
    # Mapping and GIS operations
        library(leaflet) # creates interactive maps (https://rstudio.github.io/leaflet/) (https://cran.r-project.org/package=leaflet)
        library(htmlwidgets) #  framework for creating HTML widgets - used in this case for adding some custom features to the leaflet map (https://cran.r-project.org/web/packages/htmlwidgets/vignettes/develop_intro.html) (https://cran.r-project.org/package=htmlwidgets)
        library(sf) # for working with vector-based geospatial data using 'Simple Features' objects (reading, analyzing, etc.) (https://cran.r-project.org/web/packages/sf/vignettes/sf1.html) (https://cran.r-project.org/package=sf)
        library(rmapshaper) # used in this case for simplifying polylines and polygons - also has some other geospatial analysis functions (https://cran.r-project.org/web/packages/rmapshaper/vignettes/rmapshaper.html) (https://cran.r-project.org/package=rmapshaper)
        library(raster) # for working with raster-based geospatial data - in this case for simplifying large Region 1 polylines, by converting polylines -> raster, then raster -> polygons (http://www.rspatial.org/spatial/rst/4-rasterdata.html) (https://cran.r-project.org/package=raster)
        library(units) # for working with measurement units (assigning units, unit conversion, etc.) (https://cran.r-project.org/web/packages/units/vignettes/units.html) (https://cran.r-project.org/package=units)
        library(rgeos) # interface to Geometry Engine - not used explicitly here (https://cran.r-project.org/package=rgeos)

# Assign the pipe operator (if not loading/attaching packages with the library() function)
    # `%>%` <- magrittr::`%>%` # manually assigns the pipe operator (`%>%`) - not really needed if calling library(magrittr)


# Set the data source - this tool is based on an MS Access database tool designed by the San Diego Coastkeepers for Region 9 - this determines whether to use a copy of the inputs to that tool, or to use statewide data from SMARTS
    # NOTE: to set the data source, comment out one of the following lines; the uncommented line assigns the data source:
        # data.type <- 'SMARTS' # use statewide monitoring data from SMARTS
        # data.type <- 'Database' # get data from text files exported from the Access database tool, to replicate the original Access-based tool (Region 9 data only)
        data.type <- 'DataPortal'

# Set the simplification options for geospatial objects/layers
    # 303d Polylines (not including some large Region 1 303d polylines)
        simplification.303polylines.tf <- FALSE # simplification level: keep = 0.4
    # Max size of Region 1 303d polylines to plot as polylines - convert polylines larger than this to (simplified) polygons)
        simplification.maxR1Polyline.size <- units::set_units(0.1, Mb)
    # source of Region 1 polylines to polygons (R or ArcGIS) - if true, this uses R-generated polygons
        simplification.R1_303dLinesToPoly.R <- TRUE
    # CalEnviroScreen Polygons (not including large Region 1 polylines converted to polygons)
        simplification.303polygons.tf <- FALSE # simplification level: keep = 0.7    
    # CalEnviroScreen Polygons
        simplification.ces.tf <- TRUE # simplification level: keep = 0.5
    # Regional Board Boundary Polygons
        simplification.rbBoundaries.tf <- TRUE # simplification level: keep = 0.5
# End of simplificaiton options



# # Old code, not used anymore. Was used to get data directly from the Coastkeepers' original MS Access database --------------------------------------------------------------------------------------------------------------------
#     # NOTE: HAVE TO SET THE R VERSION TO 32 BIT FIRST TO READ DIRECTLY FROM THE DATABASE (go to: Tools -> Global Options -> General -> R version)
#     # Set up the connection
#         dbPath <- 'data/Industrial_Stormwaterv24.accdb'
#         con <- DBI::dbConnect(drv = odbc::odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)}; Dbq=", dbPath))
#     # Read the data
#         standards <- tibble::as_tibble(DBI::dbReadTable(conn = con, name = 'Standards'))
#         monitoring.data <- tibble::as_tibble(DBI::dbReadTable(conn = con, name = 'SMARTS monitoring data'))
#         facilities <- tibble::as_tibble(DBI::dbReadTable(conn = con, name = 'NOIs'))
#         receiving.waters <- tibble::as_tibble(DBI::dbReadTable(conn = con, name = 'Recieving Waters'))
#     # Close the connection
#         DBI::dbDisconnect(con)
# ----------------------------------------------------------------------------------------------------------------------------

# Read data into R from text files ------------------------------------------------------------------------------------------------------------------
    # standards
        standards <- suppressMessages(readr::read_tsv('data/Standards.txt'))
        names(standards) <- make.names(names(standards))
    # monitoring data
        if (data.type == 'Database') {
            monitoring.data <- suppressMessages(readr::read_tsv('data/SMARTS_monitoring_data.txt'))
        }
        if (data.type == 'SMARTS') { 
                # fix errors encountered in reading the SMARTS monitoring data file by first finding and replacing any quoted characters in the data
                    monitoring_lines <- readLines('data/SMARTS_data/Industrial_Ad_Hoc_Reports_-_Parameter_Data.txt') # save the data as lines of text
                    problems <- grep(pattern = '\"*\"', x = monitoring_lines) # find the lines where there is a quoted character
                    for (i in seq(length(problems))) { # for each problem line found...
                        monitoring_lines[problems[i]] <- gsub(pattern = '\"*\"', replacement = '', x = monitoring_lines[problems[i]]) # get rid of the quotes, but keep the text between the quotes
                    }
                # write the corrected dataset to a temporary file
                    t <- tempfile()
                    writeLines(text = monitoring_lines, con = t, sep = '\n')
                # read the new dataset into an R dataframe, then close the temporary file
                    monitoring.data <- readr::read_tsv(file = t)
                    unlink(t)
                # get rid of data points with missing latitude and/or longitude data
                    # monitoring.data <- monitoring.data[(!is.na(monitoring.data$MONITORING_LATITUDE) & !is.na(monitoring.data$MONITORING_LONGITUDE)),] # !!!! Remove all missing lat/lon points from the start !!!
        }
        if (data.type == 'DataPortal') {
            # mon.temp <- tempfile()
            # download.file(url = 'https://data.ca.gov/node/2176/download', destfile = mon.temp)
            # monitoring.data <- readr::read_csv(file = mon.temp)
            # unlink(mon.temp)
            # rm(mon.temp)
            monitoring_data_id <- '7871e8fe-576d-4940-acdf-eca0b399c1aa'
            ckan_resource_MonData <- ckanr::resource_show(url = 'https://data.ca.gov', id = monitoring_data_id, as = 'table') # resource
            monitoring.data <- readr::read_csv(file = ckan_resource_MonData$url, guess_max = 50000)
        }
        names(monitoring.data) <- make.names(names(monitoring.data))
    # facilities
        if (data.type == 'Database') {
            facilities <- suppressMessages(readr::read_tsv('data/NOIs.txt'))
        }
        if (data.type == 'SMARTS') { 
            # fix errors encountered in reading the SMARTS facilities file by first finding and replacing any quoted characters in the data
                facilities_lines <- readLines('data/SMARTS_data/Industrial_Application_Specific_Data.txt')
                problems <- grep(pattern = '\"*\"', x = facilities_lines)
                for (i in seq(length(problems))) {
                    facilities_lines[problems[i]] <- gsub(pattern = '\"*\"', replacement = '', x = facilities_lines[problems[i]])
                }
            # write the corrected dataset to a temporary file
                t <- tempfile()
                writeLines(text = facilities_lines, con = t, sep = '\n')
            # read the new dataset, then close the temporary file
                facilities <- suppressMessages(readr::read_tsv(file = t))
                unlink(t)
        }
        if (data.type == 'DataPortal') {
            # fac.temp <- tempfile()
            # download.file(url = 'https://data.ca.gov/node/2171/download', destfile = fac.temp)
            # facilities <- readr::read_csv(file = fac.temp)
            # unlink(fac.temp)
            # rm(fac.temp)
            facility_data_id <- '33e69394-83ec-4872-b644-b9f494de1824'
            ckan_resource_FacData <- ckanr::resource_show(url = 'https://data.ca.gov', id = facility_data_id, as = 'table') # resource
            facilities <- readr::read_csv(file = ckan_resource_FacData$url, guess_max = 50000)
        }
        names(facilities) <- make.names(names(facilities))
    # receiving waters
        receiving.waters <- suppressMessages(readr::read_tsv('data/Receiving_Waters.txt'))
        names(receiving.waters) <- make.names(names(receiving.waters))
    # remove duplicates
        receiving.waters <- receiving.waters %>% dplyr::distinct()

# Data transformations (monitoring data) --------------------------------------------------------------------------------------------------------
    if (data.type == 'Database') {
        # create a column of sample dates (separate out from the time)
            monitoring.data <- tidyr::separate(data = monitoring.data, col = Date.time.of.sample.collection, into = c('Sample_date_calc','Sample_time_calc'), sep = ' ')
        # convert into a date class
            monitoring.data <- monitoring.data %>% 
                dplyr::mutate(Sample_date_calc = lubridate::mdy(Sample_date_calc))
    }
    if (data.type == 'SMARTS' | data.type == 'DataPortal') {
        # Rename columns            
            monitoring.data <- monitoring.data %>% 
                dplyr::rename(Parameter = PARAMETER, Result = RESULT, Units = UNITS)
        # convert date field to a date class
            # monitoring.data <- monitoring.data %>% dplyr::mutate(Sample_date_calc = lubridate::mdy(SAMPLE_DATE))
            monitoring.data <- monitoring.data %>% 
                dplyr::mutate(Sample_date_calc = lubridate::ymd(SAMPLE_DATE))
        # Rename latitude and longitude columns
            monitoring.data <- monitoring.data %>% 
                dplyr::rename(Latitude = MONITORING_LATITUDE) %>% 
                dplyr::rename(Longitude = MONITORING_LONGITUDE)
    }
    
    # filter the SMARTS data so that it only includes effluent data
        monitoring.data <- monitoring.data %>% dplyr::filter(MONITORING_LOCATION_TYPE == 'Effluent Monitoring')
    
    # filter for reasonable dates (since the year 2000 or later, but nothing past today's date)
        monitoring.data <- monitoring.data %>% dplyr::filter(Sample_date_calc >= 2000/01/01 & Sample_date_calc <= Sys.time())
    
    # get the WB region from the WDID
        monitoring.data <- monitoring.data %>% dplyr::mutate(Region_calc = substr(x = WDID, start = 1, stop = 2))
        monitoring.data <- monitoring.data %>% dplyr::mutate(Region_calc = gsub(x = Region_calc, pattern = ' ', replacement = ''))
        # regions <- monitoring.data %>% dplyr::distinct(Region_calc)
        # regions.count <- monitoring.data %>% dplyr::group_by(Region_calc) %>% summarise(count = n())
    
    # Create a column for monitoring period
        monitoring.data <- monitoring.data %>% dplyr::mutate(Monitoring.Period = dplyr::if_else(
            lubridate::month(Sample_date_calc) >= 7, 
            paste0(lubridate::year(Sample_date_calc), ' - ', lubridate::year(Sample_date_calc) + 1), 
            paste0(lubridate::year(Sample_date_calc)-1, ' - ', lubridate::year(Sample_date_calc))
        ))
    
    # Create a list of the monitoring periods
        periods.list <- monitoring.data %>% dplyr::group_by(Monitoring.Period) %>% dplyr::summarise(count = n()) %>% dplyr::arrange(desc(Monitoring.Period))
        periods.list <- periods.list$Monitoring.Period
    
    # Create a column with ug/L Results converted to mg/L
        monitoring.data <- monitoring.data %>% dplyr::mutate(Result.Conv = dplyr::if_else(Units=='ug/L', Result / 1000, Result))
        monitoring.data <-  monitoring.data %>% dplyr::mutate(Unit.Conv = dplyr::if_else(Units=='ug/L', 'mg/L', Units))
    
    # Create a list of WDIDs
        WDID.list <- monitoring.data %>% dplyr::filter(Parameter %in% standards$Parameter) %>% dplyr::distinct(WDID)
        WDID.list <- WDID.list$WDID
        WDID.list <- c('All WDIDs', WDID.list)
    
    
    # create a sf variable for the monitoring data
        # monitoring_sf <- sf::st_as_sf(monitoring.data[!is.na(monitoring.data$Latitude),], coords = c('Longitude', 'Latitude'), crs = 4326, agr = 'constant')
    
    # # make a sf variable with unique monitoring points (1 per WDID in monitoring data)
        #     unique_points <- monitoring_sf %>% dplyr::distinct(WDID, .keep_all = TRUE) 
    


# Data transformations (standards) --------------------------------------------------------------------------------------------------------------
    # change water type column name
        standards <- standards %>% dplyr::rename(Standard.water.type = Water.type) 


# GET GEOSPATIAL LAYERS (Regional Board Boundaries, CalEnviroScreen Polygons, and 2012 303d Impaired Waters) ----
    # Regional Board Office Boundaries --------------------------------------------------------------------------------------------------------------
        # These steps show how to access and transform the RB boundary geospatial data, but they only need to be done once:
            # RB_Bounds <- sf::st_read('data/Regional_Board_Office_Boundaries_ModifiedR6_6A_6B/Regional_Board_Offices.shp')
            # RB_Bounds_transform <- sf::st_transform(RB_Bounds, 4326)
            # save the unsimplified polygons
                # saveRDS(object = RB_Bounds_transform, file = 'data/unsimplified_RB_Bounds.RDS')
            # simplify the polygons and save the simplified data in an RDS file
                # RB_Bounds_simple <- rmapshaper::ms_simplify(RB_Bounds_transform, keep = 0.5)
                # saveRDS(object = RB_Bounds_simple, file = 'data/simplified_RB_Bounds.RDS')
        # Read the data from the saved RDS file
            if (simplification.rbBoundaries.tf == TRUE) {
                RB_Boundaries <- readr::read_rds('data/simplified_RB_Bounds.RDS') 
            } else {
                RB_Boundaries <- readr::read_rds('data/unsimplified_RB_Bounds.RDS')
            }
        # check (plot)
            # g <- ggplot2::ggplot() + ggplot2::geom_sf(data = RB_Boundaries)
    
    
    # CalEnviroScreen Polygons ----------------------------------------------------------------------------------------------------------------------
        # These steps show how to access and transform the CES geospatial data, but they only need to be done once:
            # download the data
                # temp_zip <- tempfile()
                # ces_url <- 'https://oehha.ca.gov/media/downloads//ces3shp.zip'  # ALTERNATVIE: ces_url <- 'https://data.ca.gov/sites/default/files/CES3Results_SHP.zip'
                # download.file(url = ces_url, destfile = temp_zip, method = 'curl')
                # unzip(zipfile = temp_zip, exdir = 'data/CES3Results', junkpaths = TRUE) #files = c('CES3Results.shp','CES3Results.shx','CES3Results.prj'),
                # unlink(temp_zip)
            # read the shapefile into R
                # ces <- sf::st_read('data/CES3Results/CES3Results.shp')
            # transform
                # ces_transform <- sf::st_transform(ces, 4326)
                # save the unsimplified data as RDS
                # saveRDS(object = ces_transform, file = 'data/unsimplified_CalEnvironScreen_poly.RDS')
            # simplify
                # ces_simple <- rmapshaper::ms_simplify(ces_transform, keep = 0.5)
                # save the simplified polygons as an RDS file
                # saveRDS(object = ces_simple, file = 'data/simplified_CalEnvironScreen_poly.RDS')
        # Read the data from the saved RDS file
            if (simplification.ces.tf == TRUE) {
                ces_poly <- readr::read_rds('data/simplified_CalEnvironScreen_poly.RDS')
            } else {
                ces_poly <- readr::read_rds('data/unsimplified_CalEnvironScreen_poly.RDS')
            }
        
        # create a list of the CES parameter choices to plot
            ces_choices <- data.frame(Name = c('CES Percentile', 'Pollution Burden', 'Impaired Water Bodies'), CES.Variable = c('Percentile', 'Poll_pctl', 'IWB_pctl'), stringsAsFactors = FALSE) #' CES Percentile' = 'Percentile'
    
    # 303d Polygons ------------------------------------------------------------------------------------------------------------------------------------
        # These steps show how to access and transform the 303d geospatial polygon data, but they only need to be done once, 
        # and the results have been save in an RDS file (an R data format):
            # # download the data from the original source (a zip file on the internet), then unzip the file, and save the 
            # # unzipped file in the 'data' folder
                # temp_zip_impaired_poly <- tempfile()
                # impaired_poly_url <- 'https://gispublic.waterboards.ca.gov/webmap/303d_2012/files/2012_Impaired_Polys_Final.zip'
                # download.file(url = impaired_poly_url, destfile = temp_zip_impaired_poly, method = 'curl')
                # unzip(zipfile = temp_zip_impaired_poly, exdir = 'data/2012_Impaired_Polygons_Final', junkpaths = TRUE)
                # unlink(temp_zip_impaired)
            # # Read the shapefile into R
                # impaired_303d_poly <- sf::st_read('data/2012_Impaired_Polygons_Final/2012_Impaired_Polys_Final.shp')
            # # transform the data into a coordinate reference system
                # impaired_303d_transform_poly <- sf::st_transform(impaired_303d_poly, 4326)
            # save the unsimplified polygons as an RDS file, in case they will be used instead of the simplified polygons
                # saveRDS(object = impaired_303d_transform_poly, file = 'data/unsimplified_2012_303d_polygons.RDS')
            # # simplify the polygons (NOTE: the default level is keep = 0.05, but with keep < 0.6 some polygons are deleted altogether)
                # impaired_303d_simple_poly <- rmapshaper::ms_simplify(impaired_303d_transform_poly, keep = 0.7)
            # # save the simplified polygons as an R object
                # saveRDS(object = impaired_303d_simple_poly, file = 'data/simplified_2012_303d_polygons.RDS')
            # Read the data from the saved RDS file - choose from the simplified or unsimplified data
                if (simplification.303polygons.tf == TRUE) {
                    impaired_303d_polygons <- readr::read_rds('data/simplified_2012_303d_polygons.RDS')            
                } else {
                    impaired_303d_polygons <- readr::read_rds('data/unsimplified_2012_303d_polygons.RDS')            
                }
            # check (plot)
                # g <- ggplot2::ggplot() + ggplot2::geom_sf(data = dplyr::filter(impaired_303d_polygons, REGION_NUM == 9))
    
    # 303d PolyLines ------------------------------------------------------------------------------------------------------------------------------------
        # These steps show how to access and transform the 303d geospatial polyline data, but they only need to be done once
        # The results have been saved to an R data object
            # download the data from the original source (a zip file on the internet), then unzip the file, and save the
            # unzipped file in the 'data' folder
                # temp_zip_impaired <- tempfile()
                # impaired_url <- 'https://gispublic.waterboards.ca.gov/webmap/303d_2012/files/2012_Impaired_Lines_Final.zip'
                # download.file(url = impaired_url, destfile = temp_zip_impaired, method = 'curl')
                # unzip(zipfile = temp_zip_impaired, exdir = 'data/2012_Impaired_Lines_Final', junkpaths = TRUE)
                # unlink(temp_zip_impaired)
            # Read the shapefile into R
                # impaired_303d <- sf::st_read('data/2012_Impaired_Lines_Final/2012_Impaired_Lines_Final.shp')
            # transform the data into a coordinate reference system
                # impaired_303d_transform <- sf::st_transform(impaired_303d, 4326)
            # save the un-simplified 303d polyLines as an R object, in case they will be used instead of a simplified version
                # saveRDS(object = impaired_303d_transform, file = 'data/unsimplified_2012_303d_lines.RDS')
            # simplify the polygons
                # impaired_303d_simple <- rmapshaper::ms_simplify(impaired_303d_transform, keep = 0.4) # increase the 'keep' parameter to increase the level of detail retained
            # save the simplified polygons as a RDS object
                # saveRDS(object = impaired_303d_simple, file = 'data/simplified_2012_303d_lines.RDS')
            # Read the data from the saved RDS file - choose from the simplified or unsimplified version
                if (simplification.303polylines.tf == TRUE) {
                    impaired_303d_lines <- readr::read_rds('data/simplified_2012_303d_lines.RDS')
                } else {
                    impaired_303d_lines <- readr::read_rds('data/unsimplified_2012_303d_lines.RDS')            
                }
    
    # Region 1 303d PolyLines (Some are too large to plot, have to convert to simplified polygons) -------------------------------------------------------
        # get rid of large 303d lines in region 1, because they're very dense and crash the tool (will replace these with simplified polygons)
            # get the memory size of each polyline, so that the larger ones can be converted to polygons
                impaired_303d_lines <- impaired_303d_lines %>% dplyr::mutate(memSize = apply(X = impaired_303d_lines, MARGIN = 1, FUN = object.size))
            # set the size to units of bytes
                impaired_303d_lines <- impaired_303d_lines %>% dplyr::mutate(memSize = units::set_units(memSize, b))
            # convert the size from bytes to megabytes (MB)
                impaired_303d_lines <- impaired_303d_lines %>% dplyr::mutate(memSizeMB = units::set_units(memSize, Mb))
                # impaired_303d_lines <- impaired_303d_lines %>% dplyr::mutate(rank = dplyr::min_rank(size)) # provide a rank for each waterbody based on the memory size
            # convert back to sf (if needed)
                impaired_303d_lines <- sf::st_as_sf(impaired_303d_lines)
            # get the WBIDs of the larger polylines in R1, so that they can be dropped (arbitrarily selected 0.1 MB as the limit - could use something different)
                R1_303d_LargePolylines <- as.data.frame(impaired_303d_lines %>% dplyr::filter(memSizeMB > simplification.maxR1Polyline.size & REGION_NUM == 1) %>% dplyr::select(WBID)) # %>% dplyr::select(-geometry)
            # drop the size column
                impaired_303d_lines <- impaired_303d_lines %>% dplyr::select(-memSize, -memSizeMB)
            # drop the large R1 polylines
                # impaired_303d_lines_dropped <- impaired_303d_lines %>% dplyr::filter(WBID %in% R1_303d_LargePolylines$WBID) # just to check
                impaired_303d_lines <- impaired_303d_lines %>% dplyr::filter(!(WBID %in% R1_303d_LargePolylines$WBID))
    
    # ---------------------------------------------------------------------------------------------------------------------------------------------- #
    # Convert R1 lines to polygons, using R's raster package -  --------------------------------------------------------------------------------------- #
    # ---------------------------------------------------------------------------------------------------------------------------------------------- #
        # # NOTE: These steps show how to transform the Region 1 303d polylines into polygons (to simplify them), but they only need to be done once
        # # The results have been saved to an R data object
        # # read in the 303d polylines as an sf object, filter for R1 polylines, and convert to object type sp (SpatialLines object - data in this format is needed to work with the raster package)
        #     R1_303d_lines <- readr::read_rds('data/simplified_2012_303d_lines.RDS') %>% dplyr::filter(REGION_NUM == 1) %>% sf::st_as_sf()
        # # transform to a projected coordinate system
        #     R1_303d_lines <- sf::st_transform(R1_303d_lines, 3857) # Transform to projected coordinate system; may also try 3310?
        #     # Convert the Region 1 303d polylines to polygons - step through 1 waterbody at a time --------------------------------
        #         for (i in seq(nrow(R1_303d_lines))) {
        #             # Make sure the output variable isn't already populated with data
        #                 if (i == 1) {
        #                     if (exists('R1_lines_toPolygon')) {rm(list = 'R1_lines_toPolygon')}
        #                 }
        #             # get the current waterbody, and convert to sp
        #                 R1_303d_lines_sp <- as(R1_303d_lines[i,], 'Spatial') 
        #             # create an empy raster that bounds the waterbody. NOTE: This is where the resolution (grid size) is set !!!!!
        #                 R1_Raster <- raster::raster(x = R1_303d_lines_sp, resolution = 250)
        #             # convert the polylines to raster (rasterize), based on the resolution (i.e., grid size) defined above. Give each cell the value of the 'OBJECTID' field so they can be grouped to the appropriate waterbody.
        #                 R1_Raster_rasterize <- raster::rasterize(x = R1_303d_lines_sp, y = R1_Raster, field = 'OBJECTID')
        #             # convert the raster to a polygon
        #                 R1_RasterToPolygon <- raster::rasterToPolygons(R1_Raster_rasterize, dissolve = TRUE) # dissolve means that if there is a disconnect in the polygon, it just creates one multi-part polygon
        #             # convert the polygon from object type sp to sf
        #                 R1_RasterToPolygon <- sf::st_as_sf(R1_RasterToPolygon)
        #             # simplify the polygon - NOTE: set the simplification level here !!!!!
        #                 R1_RasterToPolygon <- rmapshaper::ms_simplify(R1_RasterToPolygon, keep = 0.4)
        #             # join the polyline data to the new corresponding polygons
        #                 R1_RasterToPolygon <- R1_RasterToPolygon %>% dplyr::left_join(R1_303d_lines %>% dplyr::select(-c(geometry)), by = c('layer' = 'OBJECTID')) %>% sf::st_as_sf()
        #             # append the polygon to the complete dataframe of polygons
        #                 if (!exists('R1_lines_toPolygon')) {
        #                     R1_lines_toPolygon <- R1_RasterToPolygon} else {
        #                         R1_lines_toPolygon[i,] <- R1_RasterToPolygon
        #                     }
        #             # save the complete dataframe of polygons to an RDS file after each new polygon is added (in case it stops working before all polygons are done)
        #                 saveRDS(object = R1_lines_toPolygon, file = paste0('data/Region1_linesToPolygons_Rworkflow_', i, '.RDS'))
        #             # delete the file created in the previous step of this loop
        #                 if (i > 1) {
        #                     file.remove(paste0('data/Region1_linesToPolygons_Rworkflow_', i-1, '.RDS'))
        #                 }
        #             # clear variables
        #                 rm(list = c('R1_RasterToPolygon', 'R1_303d_lines_sp', 'R1_Raster', 'R1_Raster_rasterize'))
        #             # after the last waterbody has been converted, rename the final file, then clear remaining variables
        #                 if (i == max(seq(nrow(R1_303d_lines)))) {
        #                     saveRDS(object = R1_lines_toPolygon, file = 'data/Region1_linesToPolygons_Rworkflow.RDS')
        #                     file.remove(paste0('data/Region1_linesToPolygons_Rworkflow_', i, '.RDS'))
        #                     rm(list = c('R1_lines_toPolygon', 'i'))
        #                     }
        #         } 
    # ---------------------------------------------------------------------------------------------------------------------------------------------- #
    # End of R1 lines to polygons script ----------------------------------------------------------------------------------------------------------- #
    # ---------------------------------------------------------------------------------------------------------------------------------------------- #
    # read data from the saved RDS file
        # Read the data from the saved RDS file - can either use the polygons created with R or created with ArcGIS
        if (simplification.R1_303dLinesToPoly.R == TRUE) {
            # this uses polygons created with R's raster package, above
            impaired_303d_R1_linesToPoly <- readr::read_rds('data/Region1_linesToPolygons_Rworkflow.RDS')    
        } else {
            # this uses a shapefile was created in ArcGIS using its Conversion Tools toolbox (the functions "To Raster -> Polyline to Raster" and "From Raster -> Raster to Polygon" )
            impaired_303d_R1_linesToPoly <- sf::st_read('data/2012_Impaired_Lines_R1_ToPoly/RasToPoly100_dissolve.shp')
            # this is some old code that was used to further simplify the ArcGIS-generated polygons - may want to delete
            # impaired_303d_R1_LineToPoly <- sf::st_read('data/2012_Impaired_Lines_R1_ToPoly/RasToPoly100_dissolve.shp')
            # impaired_303d_R1_transform <- sf::st_transform(impaired_303d_R1_LineToPoly, 4326)
            # impaired_303d_R1_simple <- rmapshaper::ms_simplify(impaired_303d_R1_transform, keep = 0.3) # default = 0.05
            # saveRDS(object = impaired_303d_R1_simple, file = 'data/simplified_2012_303d_lines_R1toPoly.RDS')
        }
        # transform to be consistent with the 303d polygons data
            impaired_303d_R1_linesToPoly <- sf::st_transform(impaired_303d_R1_linesToPoly, 4326)
        # Extract just those waterbodies that were dropped above (the large polylines, stored in variable R1_303d_LargePolylines)
            impaired_303d_R1_linesToPoly <- impaired_303d_R1_linesToPoly %>% dplyr::filter(WBID %in% R1_303d_LargePolylines$WBID)
        # rename / add some columns, to be consistent with the 303d polygons data
            impaired_303d_R1_linesToPoly <- impaired_303d_R1_linesToPoly %>% dplyr::rename(OBJECTID = layer)
            impaired_303d_R1_linesToPoly <- impaired_303d_R1_linesToPoly %>% dplyr::mutate(Shape_Area = NA)
        # convert back to sf (if needed)
            impaired_303d_R1_linesToPoly <- sf::st_as_sf(impaired_303d_R1_linesToPoly)
        # join these new R1 polygons to the original polygons file
            # impaired_303d_polygons <- impaired_303d_polygons %>% dplyr::bind_rows(impaired_303d_R1_linesToPoly)
            impaired_303d_polygons <- rbind(impaired_303d_polygons, impaired_303d_R1_linesToPoly)
    
    
    
    # 303d Waterbodies Pollutant Information (table) ----------------------------------------------------------------------------------------------------
        # # These steps show how to access and transform the 303d tabular data, but they only need to be done once:
        #     download.file(url = 'https://gispublic.waterboards.ca.gov/webmap/303d_2012/files/2012_USEPA_approv_303d_List_Final_20150807.xlsx', destfile = 'data/2012_USEPA_approv_303d_List_Final_20150807.xlsx', method = 'curl')
        #     impaired_pollutants <- readxl::read_excel('data/2012_USEPA_approv_303d_List_Final_20150807.xlsx', sheet = 'Final 303(d) List')
        #     names(impaired_pollutants) <- make.names(names(impaired_pollutants))
        # # Create a column for comments that also includes the pollutant (only for rows where there is a comment)
        #     impaired_pollutants <- impaired_pollutants %>% dplyr::mutate(Pollutant_Comment= dplyr::if_else(!is.na(COMMENTS.INCLUDED.ON.303.d..LIST), paste0(POLLUTANT, ': ', COMMENTS.INCLUDED.ON.303.d..LIST), 'NA'))
        #     impaired_pollutants$Pollutant_Comment[impaired_pollutants$Pollutant_Comment == 'NA'] <- NA # replace text NAs from formual above with actual NAs
        # # Create a list of the unique IDs
        #     impaired_IDs <- impaired_pollutants %>% dplyr::distinct(WBID)
        # # for each ID in the list, append the list of pollutants associated with that ID, and the comments associated with those pollutants (if any)
        #     for (i in seq(nrow(impaired_IDs))){
        #         temp <- impaired_pollutants %>% dplyr::filter(WBID == impaired_IDs$WBID[i])
        #         impaired_IDs$Pollutant[i] <- paste0(temp$POLLUTANT, collapse = ' | ')
        #         temp2 <- impaired_pollutants %>% dplyr::filter(WBID == impaired_IDs$WBID[i] & !is.na(Pollutant_Comment))
        #         impaired_IDs$Comments[i] <- paste0(temp2$Pollutant_Comment, collapse = ' | ')
        #     }
        # # save as an RDS file
        #     saveRDS(object = impaired_IDs, file = 'data/303d_List.RDS')
        # # read the data from the RDS file into an R object
            impaired_303d_list <- readr::read_rds('data/303d_List.RDS')
    
    
    # 303d Pollutant Potential Sources (table 2) --------------------------------------------------------------------------------------------------------
    # # These steps show how to access and transform the 303d tabular data, but they only need to be done once:
    #     download.file(url = 'https://gispublic.waterboards.ca.gov/webmap/303d_2012/files/2012_USEPA_approv_303d_List_Final_20150807wsrcs.xls', destfile = 'data/2012_USEPA_approv_303d_List_Final_20150807wsrcs.xls', method = 'curl')
    #     impaired_pollutants <- readxl::read_excel('data/2012_USEPA_approv_303d_List_Final_20150807wsrcs.xls', sheet = 'Final 303(d) List wsrcs')
    #     names(impaired_pollutants) <- make.names(names(impaired_pollutants))
    #     # Create a column for source that also includes the pollutant (only for rows where there is a source given)
    #         # impaired_pollutants <- impaired_pollutants %>% dplyr::mutate(Pollutant_Source= dplyr::if_else(POTENTIAL.SOURCES != 'Source Unknown', paste0(POLLUTANT, ': ', Hmisc::capitalize(tolower(SOURCE.CATEGORY)), ' (', POTENTIAL.SOURCES, ')'), 'NA'))
    #         impaired_pollutants <- impaired_pollutants %>% dplyr::mutate(Source= dplyr::if_else(POTENTIAL.SOURCES != 'Source Unknown', paste0(Hmisc::capitalize(tolower(SOURCE.CATEGORY)), ' (', POTENTIAL.SOURCES, ')'), 'NA'))
    #         impaired_pollutants$Source[impaired_pollutants$Source == 'NA'] <- NA # replace text NAs from formual above with actual NAs
    #     # Create a list of the unique IDs
    #         impaired_IDs_Source <- impaired_pollutants %>% dplyr::distinct(WBID)
    #         impaired_IDs_Pollutant <- impaired_pollutants %>% dplyr::select(WBID, POLLUTANT) %>% dplyr::distinct()
    #     # for each ID in the list of ID & pollutant combinations, append the list of sources associated with that combination of ID and pollutant (if any)
    #         for (i in seq(nrow(impaired_IDs_Pollutant))){
    #             temp <- impaired_pollutants %>% dplyr::filter(WBID == impaired_IDs_Pollutant$WBID[i] & POLLUTANT == impaired_IDs_Pollutant$POLLUTANT[i] & !is.na(Source))
    #             impaired_IDs_Pollutant$Source[i] <- paste0(temp$Source, collapse = '; ')
    #             # temp2 <- impaired_pollutants %>% dplyr::filter(WBID == impaired_IDs$WBID[i] )
    #             # impaired_IDs$Comments[i] <- paste0(temp2$Pollutant_Source, collapse = ' | ')
    #         }
    #         impaired_IDs_Pollutant <- impaired_IDs_Pollutant %>% dplyr::mutate(Pollutant_Source = dplyr::if_else(Source != '', paste0(POLLUTANT, ': ', Source), 'NA'))
    #         impaired_IDs_Pollutant$Pollutant_Source[impaired_IDs_Pollutant$Pollutant_Source == 'NA'] <- NA
    #     # for each ID in the list of WDIDs, append the list of sources for all pollutants associated with that ID (if any)
    #         for (i in seq(nrow(impaired_IDs_Source))){
    #             temp2 <- impaired_IDs_Pollutant %>% dplyr::filter(WBID == impaired_IDs_Source$WBID[i])
    #             impaired_IDs_Source$Pollutant[i] <- paste0(temp2$POLLUTANT, collapse = ' | ')
    #             temp3 <- impaired_IDs_Pollutant %>% dplyr::filter(WBID == impaired_IDs_Source$WBID[i] & !is.na(Pollutant_Source))
    #             impaired_IDs_Source$Sources[i] <- paste0(temp3$Pollutant_Source, collapse = ' | ')
    #         }
    #     # save as an RDS file
    #         saveRDS(object = impaired_IDs_Source, file = 'data/303d_List_Sources.RDS')
    # read the data from the RDS file into an R object
        impaired_303d_sources <- readr::read_rds('data/303d_List_Sources.RDS')
    # join the pollution source info to the list of pollutants and comments
        impaired_303d_list <- impaired_303d_list %>% dplyr::left_join(impaired_303d_sources, by = c('WBID', 'Pollutant'))
    # make blanks into NAs
        impaired_303d_list$Comments[impaired_303d_list$Comments == ''] <- NA
        impaired_303d_list$Sources[impaired_303d_list$Sources == ''] <- NA
    
    
    # join the 303d information to the polygons and lines shapefile datasets, by WBID
        impaired_303d_polygons <- sf::st_as_sf(impaired_303d_polygons %>% dplyr::left_join(impaired_303d_list, by = 'WBID'))
        impaired_303d_lines <- sf::st_as_sf(impaired_303d_lines %>% dplyr::left_join(impaired_303d_list, by = 'WBID'))


# ----------------------------------------------------------------------------------------------------------------------------------------------- #       
# ----------------------------------------------------------------------------------------------------------------------------------------------- #
# Define UI --------------------------------------------------------------------
ui <- navbarPage(title = "Industrial Stormwater Assessment Tool", # theme = shinythemes::shinytheme('flatly'),
                 tabPanel('Home',
                          # this piece of code creates a way to link to other tabPanel pages - it's from: https://stackoverflow.com/questions/36412407/shiny-add-link-to-another-tabpanel-in-another-tabpanel
                              tags$head(tags$script(HTML('
                                    var fakeClick = function(tabName) {
                                                       var dropdownList = document.getElementsByTagName("a");
                                                       for (var i = 0; i < dropdownList.length; i++) {
                                                       var link = dropdownList[i];
                                                       if(link.getAttribute("data-value") == tabName) {
                                                       link.click();
                                                       };
                                                       }
                                                       };
                               '))), # end of the code to link to other tabPanel pages
                          # this defines some link style text
                            tags$head(tags$style(".linkstyle{color: steelblue;}")), # color: cadetblue; text-decoration: underline; font-weight: bold;}")), color: #286090
                          h3('Background:'),
                          p('This draft version of the Industrial Stormwater Assessment Tool is intended to summarize statewide industrial stormwater quality 
                            monitoring data reported to the ',
                            tags$a(href = 'https://www.waterboards.ca.gov/','California State Water Resources Control Board,'), 'and assess the monitoring results 
                            relative to other indicators of impairment and pollution burden, including 2012 303(d) impaired water bodies and CalEnviroScreen 3.0 
                            (CES) scores. It computes a ', 
                            tags$b('Water Quality Index (WQI)'), 
                            ' score for each facility in a selected Water Board Region for a selected time period, and displays the computed scores on a map and 
                            in tabular format (on the ', 
                            tags$em('WQI Scores', class = 'linkstyle', onclick = "fakeClick('WQI Scores')"), 
                            ' tab). The WQI scores are based on a user-selected standard, which defines threshold levels for a subset of the parameters which may be 
                            sampled for at any given site (sampling data for parameters not included in the selected standard are not considered in the WQI score). 
                            The default standards are based on common metrics used to assess and regulate stormwater discharges, but the standards can be modified by 
                            the user, or an entirely customized standard can be defined. For more information about how the WQI scores are determined, see the ', 
                            tags$em('WQI Calculations', class = 'linkstyle', onclick = "fakeClick('WQI Calculations')"), 
                            ' page.'),
                          #br(),
                          hr(), #style="border: 1px solid darkgrey"),
                          #br(),
                          h3('Instructions:'),
                          p('In general, you will view and interact with the data through the ', 
                            tags$em('WQI Scores', class = 'linkstyle', onclick = "fakeClick('WQI Scores')"), 
                            ' and ', 
                            tags$em('Standards', class = 'linkstyle', onclick = "fakeClick('Standards')"), 
                            'tabs. The contents of each tab is described below:'), 
                          tags$ul(
                              tags$li(tags$u(tags$b('WQI Scores:', class = 'linkstyle', onclick = "fakeClick('WQI Scores')")),
                                      'The panel on the left side of this tab contains a menu with inputs that can be used to customize the WQI scores calcualted and 
                                      displayed in the map, and in the corresponding data table below the map. Use the inputs in the menu to select a Water Board Region, 
                                      Year, and Standard to apply in computing the WQI scores. You can also filter for sites whose computed WQI scores fall 
                                      within a given range, sites which fall within CES tracts with a given range of scores for a selected CES parameter, 
                                      and/or sites that are within a given proximity to 303(d) waterbodies. Within the map, you can select the background 
                                      map and toggle layers on or off, through the menu in the upper right corner of the map. You can also view and download 
                                      a tabular summary of the WQI scores in the table below the map.'),
                              tags$li(tags$u(tags$b('Standards:', class = 'linkstyle', onclick = "fakeClick('Standards')")), 
                                      'Contains a table that defines the water quality parameters and associated thresholds used to calculate the WQI 
                                      scores (the standard to apply is selected in the ', 
                                      tags$em('WQI Scores', class = 'linkstyle', onclick = "fakeClick('WQI Scores')"), 
                                      ' tab). The table contains three different sets of pre-defined water quality standards commonly applied to assess stormwater 
                                      discharges, as well as an additional column for a new user-defined standard. Each standard includes default thresholds for a 
                                      subset of common water quality parameters. You can edit the default threshold values, add new parameters (right click on 
                                      the table to add new rows), or enter a completely new standard. You can also download the table as a csv file using the button 
                                      below the table.'),
                              tags$li(tags$u(tags$b('Additional Data:', class = 'linkstyle', onclick = "fakeClick('Additional Data')")), 
                                      'Contains links to download the complete set of sampling data and facility information considered in computing the WQI scores.'),
                              tags$li(tags$u(tags$b('All Facilities Map:', class = 'linkstyle', onclick = "fakeClick('All Facilities Map')")), 
                                      'Displays a map of all regulated industrial stormwater facilities in the selected region, without consideration of WQI scores and 
                                      associated monitoring data. The panel on the left side of this tab contains a menu with inputs that can be used to select the region 
                                      and filter the facilities by the CES scores of the census tract they are in. The table below the map provides the details for all of the 
                                      facilities shown in the map, and this tabular data can be downloaded to a csv or excel file by clicking on the "Download Data" 
                                      button above the table.'),
                              tags$li(tags$u(tags$b('More Information:', class = 'linkstyle')), 
                                      tags$ul(
                                          tags$li(tags$u(tags$b('WQI Calculations:', class = 'linkstyle', onclick = "fakeClick('WQI Calculations')")),
                                                  'Additional information about how the WQI scores are calculated.'), 
                                          tags$li(tags$u(tags$b('Data Sources:', class = 'linkstyle', onclick = "fakeClick('Data Sources')")),
                                                  'Links to data sources used in this tool.'), 
                                          tags$li(tags$u(tags$b('Application Information:', class = 'linkstyle', onclick = "fakeClick('Application Information')")),
                                                  'Access to the source code for this tool, and information about how to provide feedback or ask questions.')
                                          )
                              )
                          ),
                          hr(), #style="border: 1px solid darkgrey"),
                          tags$a(href = 'https://github.com/CAWaterBoardDataCenter', 
                                 tags$img(src = 'data_center_logo_withText_crop_resize.png', width = '400px', height = '97px')) # , style = "border:1px solid ghostwhite"))
                          ),
                 tabPanel('WQI Scores',
                          sidebarLayout(
                              sidebarPanel( # Sidebar with inputs 
                                  # h4('Filters:'), # Filters
                                  selectInput(inputId = 'region.selected', label = 'Select Water Board Region:', choices = c(1:4, '5R', '5S', '6A', '6B', 7:9), selected = '9'),
                                  selectInput(inputId = 'standard',label = 'Select Standard:', 
                                              choices = c('California Toxics Rule (CTR)' = 'CTR', 'Multi-Sector General Permit (MSGP) - Benchmark' = 'MSGP - Benchmark', 'Numeric Action Level (NAL)' = 'NAL', 'Custom')),
                                  selectInput(inputId = 'monitoring.period', label = 'Select Monitoring Period:', choices = periods.list, selected = '2016 - 2017'),
                                  # htmlOutput('monitoring.period.selector'),
                                  selectInput(inputId = 'WDID.selected', label = 'Select Facility WDIDs (Optional):', choices = WDID.list, multiple = TRUE, selected = WDID.list[1]),
                                  sliderInput(inputId = 'score.range', label = 'Select Water Quality Index (WQI) Score Range:', min = 0, max = 100, value = c(0,100)),
                                  # actionButton('refresh','Update')
                                  selectInput(inputId = 'ces.parameter', 'Select CalEnviroScreen (CES) Parameter:', choices = ces_choices$Name, selected = 'Pollution Burden'),
                                  sliderInput(inputId = 'ces.score.range', label = 'Filter by Score of Selected CES Parameter:', min = 0, max = 100, value = c(0,100)),
                                  textInput(inputId = 'dist.to.303', label = 'Filter for proximity to a 303d listed water body (ft):', placeholder = 'Enter a distance in feet'),
                                  checkboxInput(inputId = 'show.303d.buffer', label = 'Show 303d proximity buffer', value = FALSE),
                                  checkboxInput(inputId = 'show.excluded.points', label = 'Show excluded points', value = FALSE)#,
                                  # checkboxInput(inputId = 'show.parameters', label = 'Show parameters included in WQI score for each facility', value = FALSE)#,
                                  # hr(style="border: 1px solid darkgrey"),
                              ),
                              mainPanel( # Show map and data table
                                  h4('Water Quality Index (WQI) Scores:'),
                                  leaflet::leafletOutput('monitoring.map',height = 500),
                                  h6('*WQI = Water Quality Index Score', 
                                     br(), 
                                     textOutput('ces.legend.note')),
                                  hr(style="border: 3px solid darkgrey"),
                                  h5('WQI Scores - Tabular Data:'),
                                  DT::dataTableOutput('WQI.table')# ,
                                  # hr(style="border: 3px solid darkgrey"),
                              )
                          )
                          ),
                 tabPanel('Standards',
                          h3('Enter / Edit Standards:'),
                          p('This table defines the parameters and associated thresholds used to calculate the WQI scores (the standard to apply is selected in the ',
                            tags$u('WQI Scores', class = 'linkstyle', onclick = "fakeClick('WQI Scores')"), 
                            ' tab). You can edit the values in the table, add new parameters by right clicking anywhere in the table to add a new row, or enter a 
                            completely new standard using the Custom column. You can also use the buttons below the table to reset to the default values, or download the 
                            table as a csv file.'),
                          rhandsontable::rHandsontableOutput("hot"),
                          br(),
                          tags$head(tags$style(".buttonstyle{background-color:#f2f2f2;} .buttonstyle{color: black;}")), # define button style (background color and font color)
                          actionButton("reset", "Reset Standards",class = 'buttonstyle'),
                          downloadButton('downloadStandards', 'Standards Used in WQI Calculations', class = "buttonstyle"), HTML('&emsp;')
                          ),
                 tabPanel('Additional Data',
                          h3('Download Additional Data:'),
                          br(),
                          h4('Sampling Data:'),
                          p('The button below downloads all water quality monitoring data all for industrial stormwater discharges in the source dataset as a csv file, 
                            including data for parameters and/or time periods not considered in the WQI calculations:'),
                          downloadButton('downloadRawData', 'All Sampling Data', class = "buttonstyle"), HTML('&emsp;'),
                          br(), br(), br(),
                          h4('Facility Information:'),
                          p('The button below downloads facility information for all industrial stormwater dischargers in the source dataset as a csv file, including those facilities not 
                            assigned a WQI score (which occurs if a given facility does not have any monitoring data for parameters in the selected standard during the selected
                            time period):'),
                          downloadButton('downloadFacilities', 'All Facility Information', class = "buttonstyle")
                          ),
                 tabPanel('All Facilities Map',
                          sidebarLayout(
                              sidebarPanel( # Sidebar with inputs
                                  # h4('Filters:'), # Filters
                                  selectInput(inputId = 'region.selected_2', label = 'Select Water Board Region:', choices = c(1:4, '5R', '5S', '6A', '6B', 7:9), selected = '9'),
                                  selectInput(inputId = 'WDID.selected_2', label = 'Select Facility WDIDs (Optional):', choices = WDID.list, multiple = TRUE, selected = WDID.list[1]),
                                  selectInput(inputId = 'ces.parameter_2', 'Select CalEnviroScreen (CES) Parameter:', choices = ces_choices$Name, selected = 'Pollution Burden'),
                                  sliderInput(inputId = 'ces.score.range_2', label = 'Filter by Score of Selected CES Parameter:', min = 0, max = 100, value = c(0,100)),
                                  # textInput(inputId = 'dist.to.303', label = 'Filter for proximity to a 303d listed water body (ft):', placeholder = 'Enter a distance in feet'),
                                  # checkboxInput(inputId = 'show.303d.buffer', label = 'Show 303d proximity buffer', value = FALSE),
                                  # checkboxInput(inputId = 'show.excluded.points', label = 'Show excluded points', value = FALSE)#,
                              ),
                              mainPanel( # Show map and data table
                                  h4('Facilities:'),
                                  leaflet::leafletOutput('all.facilities.map',height = 500),
                                  h6(textOutput('ces.legend.note_facilities')),
                                  hr(style="border: 3px solid darkgrey"),
                                  h5('Facilities - Tabular Data:'),
                                  DT::dataTableOutput('facilities.map.table')
                              )
                          )
                 ),
                 navbarMenu('More Information',
                          tabPanel('WQI Calculations',
                                   withMathJax(), # to create equations
                                   # Describe the WQI Calculations:
                                   h3('Water Quality Index (WQI) Calculations:'),
                                   p('Based on the San Diego Coastkeeper\'s WQI, this is an adapted version of the official Canadian WQI, which was adoped by
                                     the United Nations Environment Program Global Environmental Monitoring System in 2007 for evaluating global water quality. The WQI 
                                     score for an individual site is based on the number of tests exceeding basin plan water quality thresholds, and the magnitude 
                                     of those exceedances, as follows:'),
                                   # h5('Frequency:'),
                                   tags$li(tags$b('Frequency:')),
                                   tags$ul(helpText('\\(F1=\\frac{\\text{Number of Samples Exceeding Standard}}{\\text{Total Number of Samples}}\\times{100}\\)')),
                                   # h5('Magnitude:'),
                                   tags$li(tags$b('Magnitude:')),
                                   tags$ul(helpText('\\(Excursion_i=\\frac{\\text{Value of Sample Exceeding Standard}_i}{\\text{Standard Value}}-1\\)')),
                                   tags$ul(helpText('\\(NSE=\\frac{\\sum{Excursion}}{\\text{Total Number of Samples}}\\)')),
                                   tags$ul(helpText('\\(F2=\\frac{NSE}{0.01(NSE)+0.01}\\)')),
                                   # h5('WQI:'),
                                   tags$li(tags$b('WQI:')),
                                   # tags$ul(helpText('\\(\\text{WQI=}100-\\frac{\\sqrt{F1^2+F2^2}}{1.4142}\\)')),
                                   tags$ul(helpText('\\(WQI=100-\\frac{\\sqrt{F1^2+F2^2}}{1.412}\\)'))#,
                                   # hr(style="border: 1px solid darkgrey") # ,
                                   ),
                          tabPanel('Data Sources',
                                   h3('Data Sources:'),
                                   tags$ul(
                                   tags$li(tags$b('Industrial Stormwater Data: '), 'This tool assesses industrial stormwater discharge monitoring data reported to the 
                                           California State Water Resources Control Boards\'', 
                                           a(href = 'https://smarts.waterboards.ca.gov', 'Storm Water Multiple Application and Report Tracking System (SMARTS)'), 
                                           'database. Selected data from the SMARTS database is available via the ',
                                           a(href = 'https://data.ca.gov/', 'California Open Data Portal'), 
                                           ', where the data is refreshed daily to multiple different datasets. Each time the tool is opened, it accesses the most recent 
                                           version of the relevant SMARTS datasets on the open data portal, including:'),
                                   tags$ul(
                                   tags$li(tags$a(href = 'https://data.ca.gov/dataset/stormwater-%E2%80%93-regulatory-and-enforcement-actions-%E2%80%93-smarts/resource/fe4712db-015a-4e92-a13f', 'Monitoring Data')),
                                   tags$li(tags$a(href = 'https://data.ca.gov/dataset/stormwater-%E2%80%93-regulatory-and-enforcement-actions-%E2%80%93-smarts/resource/a5f001af-abbb-4bc7-9196', 'Facility information'))
                                   ),
                                   tags$li(tags$b('CalEnviroScreen 3.0: '), 'This dataset is available as a shapefile from the ',
                                           a(href = 'https://oehha.ca.gov/calenviroscreen/report/calenviroscreen-30', 'California Office of Environmental Health Hazard Assessment\'s (OEHHA) website')),
                                   tags$li(tags$b('303d Listed Waterbodies: '), 'This dataset is available as a shapefile and a set of Excel files from the California State 
                                           Water Resoures Control Board\'s ',
                                           a(href = 'https://www.waterboards.ca.gov/water_issues/programs/tmdl/integrated2012.shtml', 'California 2012 Integrated Report website'), 
                                           ' (NOTE: this dataset was processed to simplify the representation of some waterbodies in Region 1 for this tool).')
                                   )
                                   ),
                          tabPanel('Application Information', # Link to the code, etc...
                                   h3('Application Information:'),
                                   h4('Source Code:'),
                                   p('This tool was buit by staff at the ',
                                     a(href = 'https://www.waterboards.ca.gov/', 'California State Water Resources Control Board\'s'), 
                                     'Office of Information Management and Analysis (OIMA), using the ', 
                                     a(href = 'https://shiny.rstudio.com/', 'Shiny'), 
                                     ' package for the R programming language. The source code for this tool is available here:',  '  '),
                                   actionButton(inputId = 'github', label = 'Code on GitHub', icon = icon('github', class = 'fa-1x'),
                                                class = 'buttonstyle',
                                                onclick ="window.open('https://github.com/daltare/Stormwater_Enforcement_Tool')"),
                                   br(), br(),
                                   h4('Feedback and Questions:'),
                                   p('For quesitons or comments about this tool, you can send an email to: ', 
                                     a(href = 'mailto:david.altare@waterboards.ca.gov', 'david.altare@waterboards.ca.gov'),
                                   p('Alternatively, you can ',
                                   a(href = 'https://github.com/daltare/Stormwater_Enforcement_Tool/issues', 'open an issue on this project\'s GitHub page'),
                                   'to request a new feature, note a bug, or leave any other comments about the tool. Feedback is appreciated!'))
                                   )
                          )
                 )

        
        

# Define server logic required to draw map -------------------------------------
server <- function(input, output, session) {
    
    output$ces.legend.note <- renderText({paste0('**CES =  CalEnviroScreen 3.0 ', input$ces.parameter, if(input$ces.parameter != 'CES Percentile') {' Percentile'}) })
    output$ces.legend.note_facilities <- renderText({paste0('**CES =  CalEnviroScreen 3.0 ', input$ces.parameter_2, if(input$ces.parameter_2 != 'CES Percentile') {' Percentile'}) })
    
    # 0. Get the standards---------------------------------------------------------------------------------------------------------------------------------------------------------
        # User Entered Standards ---------------------------------------------------------
        values <- reactiveValues()
        observe({
            DF <- standards %>% dplyr::select(-Standard.water.type) %>%  tidyr::spread(Standard.Type, Standard) %>% dplyr::mutate(Custom = '')
            DF$Custom <- as.numeric(DF$Custom)
            
            if (!is.null(input$hot)) {
                DF = rhandsontable::hot_to_r(input$hot)
            } else {
                if (is.null(values[["DF"]]))
                    DF <- DF
                else
                    DF <- values[["DF"]]
            }
            values[["DF"]] <- DF
            
            # Get the standards to apply in calculations --------------------------------------
                standards.applied <- as.data.frame(DF) %>% dplyr::select(Parameter, input$standard)
                # standards.applied <- as.data.frame(DF) %>% dplyr::select(Parameter, 'CTR') # to run manually
                tf <- !is.na(standards.applied %>% dplyr::select(input$standard))
                # tf <- !is.na(standards.applied %>% dplyr::select('CTR')) # to run manually
                standards.applied <- standards.applied[tf,]
                names(standards.applied)[2] <- 'Standard'
                standards.applied <- standards.applied %>% dplyr::mutate(Standard.Type = input$standard)
                # standards.applied <- standards.applied %>% dplyr::mutate(Standard.Type = 'CTR') # to run manually
            
            # Standards Data Download Button --------------------------------------------------
                output$downloadStandards <- downloadHandler(
                    filename = 'WQI_Standards.csv',
                    content = function(con) {
                        write.csv(as.data.frame(DF), con, row.names = FALSE)
                    },
                    contentType = 'text/csv'
                )
        
        # 1. First filter the monitoring data for the selected region (to avoid doing operations on the entire dataset) ----------------------------------------------------------------
            monitoring.data.WQI <- monitoring.data %>% dplyr::filter(Region_calc == input$region.selected) 
            # monitoring.data.WQI <- monitoring.data %>% dplyr::filter(Region_calc == 9)
            
            # get the sf (geospatial) data for the selected region
                rb.boundary.selected <- sf::st_as_sf(RB_Boundaries %>% dplyr::filter(RB_OFF == input$region.selected))
                # rb.boundary.selected <- sf::st_as_sf(RB_Boundaries %>% dplyr::filter(RB_OFF == 9))
                # g <- ggplot2::ggplot() + ggplot2::geom_sf(data = rb.boundary.selected)
        
        
        # 2. Then filter the CES polygons for the selected region ----------------------------------------------------------------------------------------------------------------------
            # filter using the RB boundary polygons
                tf_poly_bounds <- sf::st_intersects(ces_poly, rb.boundary.selected, sparse = FALSE) 
            # create a polygon that bounds the monitoring data, then create sf variable with the CES polygons in the study 
                # lat_min <- min(monitoring.data.WQI$Latitude[monitoring.data.WQI$Latitude >= 32], na.rm = TRUE)
                # lat_max <- max(monitoring.data.WQI$Latitude[monitoring.data.WQI$Latitude <= 43], na.rm = TRUE)
                # lon_min <- min(monitoring.data.WQI$Longitude[monitoring.data.WQI$Longitude >= -125], na.rm = TRUE)
                # lon_max <- max(monitoring.data.WQI$Longitude[monitoring.data.WQI$Longitude <= -114], na.rm = TRUE)
                # poly_bounds <- sf::st_polygon(list(rbind(c(lon_min,lat_max),
                #                                          c(lon_max,lat_max),
                #                                          c(lon_max,lat_min),
                #                                          c(lon_min,lat_min),
                #                                          c(lon_min,lat_max))))
                # # tf_poly_bounds <- st_within(ces_poly, poly_bounds, sparse = FALSE)
                # tf_poly_bounds <- sf::st_intersects(ces_poly, poly_bounds, sparse = FALSE) # sparse = FALSE because there is only one element in poly_bounds, so this just returns a single true/false list
                # 
            # create a new variable with the selected ces polygons
                ces_poly_study_area <- ces_poly[tf_poly_bounds,] # creates a sf variable with just the CES polygons in / around the area where there are monitoring points
            # plot
                # g <-ggplot2::ggplot() + ggplot2::geom_sf(data = ces_poly_study_area, aes(fill = Poll_pctl))
        
        # 3. Then filter the 303d lines and polygons for the selected region ----------------------------------------------------------------------------------------------------------------------
            # first filter using the attributes in the 303d shapefile
                impaired_lines_region <- impaired_303d_lines %>% dplyr::filter(REGION_NUM == substr(x = input$region.selected, start = 1, stop = 1)) %>% sf::st_as_sf()
                # impaired_lines_region <- impaired_303d_lines %>% dplyr::filter(REGION_NUM == substr(x = 9, start = 1, stop = 1)) %>% sf::st_as_sf()
                impaired_polygons_region <- impaired_303d_polygons %>% dplyr::filter(REGION_NUM == substr(x = input$region.selected, start = 1, stop = 1)) %>% sf::st_as_sf()
                # impaired_polygons_region <- impaired_303d_polygons %>% dplyr::filter(REGION_NUM == substr(x = 9, start = 1, stop = 1)) %>% sf::st_as_sf()
            # then filter using the RB boundary polygons (for regions where there is more that 1 office / subregion)
                tf_impaired_bounds_lines <- sf::st_intersects(impaired_lines_region, rb.boundary.selected, sparse = FALSE) # sparse = FALSE because there is only one element in poly_bounds, so this just returns a single true/false list
                tf_impaired_bounds_polys <- sf::st_intersects(impaired_polygons_region, rb.boundary.selected, sparse = FALSE)
                # old method using a polygon that bounds the study area
                    # tf_impaired_bounds <- sf::st_intersects(impaired_303d_lines, poly_bounds, sparse = FALSE) # sparse = FALSE because there is only one element in poly_bounds, so this just returns a single true/false list
            # create a new variable with the selected 303d lines
                impaired_lines_study_area <- impaired_lines_region[tf_impaired_bounds_lines,] # creates a sf variable with just the impaired waters in / around the area where there are monitoring points
                impaired_polygons_study_area <- impaired_polygons_region[tf_impaired_bounds_polys,]
        
        # 4. Prepare the monitoring data to be used in the WQI score --------------------------------------------------------------------------------------------------------------------
            # get rid of commas in the parameter names in the monitoring data
            if (data.type == 'SMARTS' | data.type == 'DataPortal') {
                monitoring.data.WQI <- monitoring.data.WQI %>% dplyr::mutate(Parameter = gsub(x = Parameter, pattern = ',', replacement = ''))
            }            
            # Filter for the relevant parameters, and compare the results to standards
            monitoring.data.WQI <- monitoring.data.WQI %>% dplyr::filter(Parameter %in% standards.applied$Parameter)
            # monitoring.data.WQI <- monitoring.data.WQI[(!is.na(monitoring.data.WQI$Latitude) & !is.na(monitoring.data.WQI$Longitude)),] # Remove missing lat/lon points !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            # add the standards to the table
            monitoring.data.WQI <- monitoring.data.WQI %>% dplyr::left_join(standards.applied, by = 'Parameter') 
            # add facility information to the table
            # monitoring.data.WQI <- monitoring.data.WQI %>% dplyr::left_join(facilities %>% dplyr::select(WDID, STATUS_CODE_NAME, REGION, FACILITY_NAME, FACILITY_ADDRESS, FACILITY_CITY, FACILITY_STATE, FACILITY_ZIP, FACILITY_COUNTY, RECEIVING_WATER_NAME, PRIMARY_SIC), by = 'WDID') 
            monitoring.data.WQI <- monitoring.data.WQI %>% dplyr::left_join(facilities %>% dplyr::select(WDID, RECEIVING_WATER_NAME), by = 'WDID')
            # add the receiving water type - NOTE: THIS IS NOT BEING USED FOR ANYTHING RIGHT NOW
            monitoring.data.WQI <- monitoring.data.WQI %>% dplyr::left_join(receiving.waters, by = c('RECEIVING_WATER_NAME' = 'Receiving.Water'))
            # For F1, compute exceedance for each sample, by comparing the result to the standard for each sample (Note: in R, TRUE = 1, FALSE = 0)
            monitoring.data.WQI <- monitoring.data.WQI %>% dplyr::mutate(Exceedance = dplyr::if_else(Result.Conv > Standard, TRUE, FALSE))
            # For F2, compute excursion for each sample
            monitoring.data.WQI <- monitoring.data.WQI %>% dplyr::mutate(Excursion = dplyr::if_else(Result.Conv > Standard, (Result.Conv / Standard - 1), 0))
            # NOTE -- SHOULD WE CHECK WHETHER THE RECEIVING WATER IS FRESHWATER BEFORE THE ABOVE STEPS, AND MAYBE REMOVE SALTWATER??? DON'T THINK THE ACCESS DB DOES THIS
        
        # 5. Calculate the WQI scores --------------------------------------------------------------------------------------------------------------------------------------------------
            # Calculate Exceedance (for F1) and Excursion (for F2)
                WQI.Scores <- monitoring.data.WQI %>% dplyr::group_by(WDID, Monitoring.Period, Standard.Type) %>% dplyr::summarize(Exceedances = sum(Exceedance), Sum.Excursion = sum(Excursion), Total.Samples = n()) # Total.Samples(Exceedance))
                # Calculate F1 (Exceedance)
                    WQI.Scores <- WQI.Scores %>% dplyr::mutate(F1 = Exceedances / Total.Samples * 100)
                # Calculate F2 (Excursion)
                    WQI.Scores <- WQI.Scores %>% dplyr::mutate(NSE = Sum.Excursion / Total.Samples)
                    WQI.Scores <- WQI.Scores %>% dplyr::mutate(F2 = NSE / (0.01 * NSE + 0.01))
            # Final WQI Scores (!!!! NOTE: THE DATABASE USES 1.412, BUT THE POWERPOINT PRESENTATION USES 1.4142 - USED 1.412 HERE TO REPLICATE DATABASE TOOL RESULTS !!!)
                WQI.Scores <- WQI.Scores %>% dplyr::mutate(WQI = pmax(0, round((100 - sqrt(F1^2 + F2^2) / 1.412),1)))
                # WQI.Scores <-  WQI.Scores %>% dplyr::mutate(Frequency = round(F1,0))
                # WQI.Scores <-  WQI.Scores %>% dplyr::mutate(Magnitude = round(F2,0))
                if (data.type == 'SMARTS' | data.type == 'DataPortal') {
                    facilities <- facilities %>% dplyr::rename(STATUS_CODE_NAME = STATUS, REGION = REGION_BOARD) 
                }
            # Join the facilities info to the WQI scores, by WDID   
                WQI.Scores <- WQI.Scores %>% dplyr::left_join(facilities %>% dplyr::select(WDID, STATUS_CODE_NAME, REGION, FACILITY_NAME, FACILITY_ADDRESS, FACILITY_CITY, FACILITY_STATE, FACILITY_ZIP, FACILITY_COUNTY, RECEIVING_WATER_NAME, PRIMARY_SIC, FACILITY_LATITUDE, FACILITY_LONGITUDE), by = 'WDID')
            # rename the lat/lon fields, to make mapping easier
                WQI.Scores <- WQI.Scores %>% dplyr::rename(Latitude = FACILITY_LATITUDE, Longitude = FACILITY_LONGITUDE)
        
        # 6. Get a list of parameters in the WQI score
            # if (input$show.parameters == TRUE) {
                # Create a dataframe of the parameters for each WDID and monitoring period
                    grouped_mon_data <- monitoring.data.WQI %>% dplyr::group_by(WDID, Monitoring.Period) %>% dplyr::select(WDID, Parameter, Monitoring.Period) %>% dplyr::distinct()
                    distinct_WDIDs_MonPeriods <- grouped_mon_data %>% dplyr::summarize(Parameters.In.Score = paste0(Parameter, collapse = ' | '))
                # # Create a column of data that lists the different parameters that were included in the WQI calculations for each WDID and monitoring period
                #     # Create a df of distinct combinations of WDIDs, Monitoring Periods, and Parameters
                #         grouped_mon_data <- monitoring.data.WQI %>% dplyr::group_by(WDID, Parameter, Monitoring.Period) %>% dplyr::select(WDID, Parameter, Monitoring.Period) %>% dplyr::distinct()
                #     # Create a df of distinct WDIDs and Monitoring Periods
                #         distinct_WDIDs_MonPeriods <- as.data.frame(grouped_mon_data) %>% dplyr::select(WDID, Monitoring.Period) %>% dplyr::distinct()
                # # To the df of distinct WDIDs and Monitoring Periods, append a character string of the Parameters for each combination of WDID and Monitoring Period
                #     for (i in 1:nrow(distinct_WDIDs_MonPeriods)) {
                #         temp_params <- grouped_mon_data %>% dplyr::filter(WDID == distinct_WDIDs_MonPeriods$WDID[i] & Monitoring.Period == distinct_WDIDs_MonPeriods$Monitoring.Period[i]) # df filtered for given WDID and Monitoring Period
                #         temp_params <- paste0(temp_params$Parameter, collapse = ', ') # character string of parameters in the above df
                #         distinct_WDIDs_MonPeriods$Parameters.In.Score[i] <- temp_params # append the parameters string to the df of distinct WDIDs and Monitoring Periods
                #     }
                # # join the list of parameters to the WQI.Scores df
                    WQI.Scores <- WQI.Scores %>% dplyr::left_join(distinct_WDIDs_MonPeriods, by = c('WDID', 'Monitoring.Period'))
            # } # else {WQI.Scores$Parameters.In.Score = ''}
        
        # 7. Create a sf object from the WQI Scores data
            WQI.Scores_sf <- sf::st_as_sf(WQI.Scores[!is.na(WQI.Scores$Latitude),], coords = c('Longitude', 'Latitude'), crs = 4326, agr = 'constant')
        
        
        # 8. Apply filters and create the map ----------------------------------------------------------------------------------------------------------------------------------
            # Create the color palette for the WQI scores
                wqi.leaflet.pal <- leaflet::colorNumeric(
                    palette = colorRamp(c('olivedrab2', 'red3'), interpolate='spline'),
                    domain = WQI.Scores$WQI,
                    reverse = TRUE
                )
        
            # Create the color palette for the CES polygons
                param <- ces_choices[ces_choices$Name == input$ces.parameter,2]
                # param <- ces_choices[ces_choices$Name == 'Pollution Burden',2] 
            # create a numeric variable for the CES percentile that is in the middle of the values of the given 5% range
                ces.percentile.numeric <- suppressWarnings(tidyr::separate(as.data.frame(ces_poly_study_area), Percentile, sep = '-', into = c('Percentile', 'Perc2'), convert = TRUE)[,2]+1.5) # error here because there are NAs in the CES percentile scores
                ces_poly_study_area <- ces_poly_study_area %>% dplyr::mutate(ces.percentile.numeric = ces.percentile.numeric)
        
            # get the selected parameter of CES data - if CES percentile is selected, use the numeric field that was created from the factor values in the lines above
                if (param == 'Percentile') {
                    ces.pal.domain <- as.data.frame(ces_poly_study_area %>% dplyr::select(ces.percentile.numeric))[,1]
                } else {
                    ces.pal.domain <- as.data.frame(ces_poly_study_area %>% dplyr::select(param))[,1]
                }
            # create a new column in the ces data frame called fill.variable, with the selected parameter data
                ces_poly_study_area <- ces_poly_study_area %>% dplyr::mutate(fill.variable = ces.pal.domain)
        
            # create the pallete
                ces.pal.color <- 'YlOrBr' #'Blues' #'Greys'
                # ces.pal.color <- colorRamp(c('white', 'wheat4'), interpolate='spline')
                # ces.pal.color <- heat.colors(n=100)
            
                # if (param == 'Percentile') {
                #     ces.leaflet.pal <- leaflet::colorFactor(
                #         palette = ces.pal.color,
                #         domain = ces_poly_study_area$Percentile
                #     )
                # } else {
                ces.leaflet.pal <- leaflet::colorNumeric(
                    palette = ces.pal.color,
                    domain = ces_poly_study_area$fill.variable,
                    reverse = FALSE
                )
                # }
        
        ces_poly_study_area <- sf::st_as_sf(ces_poly_study_area)
        
        # Filter WQI points by attribues of CES polygons containing each point
            # find the polygons that meet the selected criteria (range of values)
                ces_poly_study_area_filtered <- ces_poly_study_area %>% dplyr::filter(fill.variable >= input$ces.score.range[1] & fill.variable <= input$ces.score.range[2]) %>% sf::st_as_sf()
        
        # create a list of WDIDs in the polygons that meet the selected criteria
        # points_ces_filter <- st_intersects(ces_poly_study_area_filtered, WQI.Scores_sf, sparse = TRUE) # lists the monitoring point(s) in each polygon 
        points_ces_filter <- sf::st_intersects(WQI.Scores_sf, ces_poly_study_area_filtered, sparse = TRUE) # lists the polygon number for each monitoring point; if no polygon number, the point is not in a polygon that meets the criteria
        tf_points <- as.logical(sapply(points_ces_filter, length)) # gives a logical vector where TRUE means the monitoring point meets the criteria (sapply and length argument needed because there may be more than 1 polygon in some cases)
        WDIDs_ces_filter <- (as.data.frame(WQI.Scores_sf)[tf_points,] %>% dplyr::select(WDID))[,1] # list of WDIDs in the polygons that satisfy the criteria
        
        # filter for points within the selected maximum distance of 303d waterbodies
        # check to see if a valid number is entered
        if (!is.na(as.numeric(input$dist.to.303))) {
            proximity_ft <- units::as_units(as.numeric(input$dist.to.303), 'ft')
            # proximity_ft <- units::as_units(as.numeric(5000), ft)
            # convert distance from feet to meters
            proximity_meters <- units::set_units(proximity_ft, 'm')
            # have to convert points and lines to Cartesian coordinate system (x,y rather than lat/lon) to do the distance check
            WQI.Scores_sf_mercator <- sf::st_transform(WQI.Scores_sf, 3857) 
            impaired_lines_study_area_mercator <- sf::st_transform(impaired_lines_study_area, 3857)
            impaired_polygons_study_area_mercator <- sf::st_transform(impaired_polygons_study_area, 3857)
            
            # check for points within the given distance of impaired lines (returns a sparse matrix - i.e., a list the same length as the number of WQI points, that lists the stream segment(s), if any, that meet the criteria for each point)
            WQI_303d_dist_check_lines <- sf::st_is_within_distance(WQI.Scores_sf_mercator, impaired_lines_study_area_mercator, dist = proximity_meters)
            tf_points_303_distance_lines <- as.logical(sapply(WQI_303d_dist_check_lines, length)) # gives a logical vector where TRUE means the monitoring point has at least one 303d stream segment that meets the proximity criteria (sapply and length argument needed because there may be more than 1 polygon in some cases)
            
            # check for points within the given distance of impaired polygons (returns a sparse matrix - i.e., a list the same length as the number of WQI points, that lists the stream segment(s), if any, that meet the criteria for each point)
            WQI_303d_dist_check_polygons <- sf::st_is_within_distance(WQI.Scores_sf_mercator, impaired_polygons_study_area_mercator, dist = proximity_meters)
            tf_points_303_distance_polygons <- as.logical(sapply(WQI_303d_dist_check_polygons, length)) # gives a logical vector where TRUE means the monitoring point has at least one 303d stream segment that meets the proximity criteria (sapply and length argument needed because there may be more than 1 polygon in some cases)
            # combine the line and polygon checks
            tf_points_303_distance <- tf_points_303_distance_lines | tf_points_303_distance_polygons
            
            
            # create a variable with the points that meet the criteria for proximity to either impaired lines or polygons (or both)
            WQI_303d_dist_points <- WQI.Scores_sf[tf_points_303_distance,] # points that satisfy the distance to 303d waters criteria
            WQI_303d_dist_points_list <- (as.data.frame(WQI_303d_dist_points) %>% dplyr::select('WDID'))[,1]
            # dataset of excluded points
            WQI_303d_excluded_points <- WQI.Scores_sf[!tf_points_303_distance,] # points that don't satisfy the distance to 303d waters criteria
            # create a variable with polygons representing the buffered region around the streams
            buffered_streams_lines <- sf::st_buffer(x = impaired_lines_study_area_mercator, dist = proximity_meters)
            buffered_streams_polygons <- sf::st_buffer(x = impaired_polygons_study_area_mercator, dist = proximity_meters)
            # join the buffered lines and polygons datasets
            buffered_streams <- rbind(buffered_streams_lines, (buffered_streams_polygons %>% dplyr::select(-Shape_Area)))
            buffered_streams_geographic <- sf::st_transform(buffered_streams, 4326)
            # for testing only - this creates a map of the points (red = meets proximity criteria, black = doesn't meet proximity criteria) + streams + buffers
            #     g <-ggplot2::ggplot() + ggplot2::geom_sf(data = buffered_streams) + ggplot2::geom_sf(data = WQI.Scores_sf[!tf_points_303_distance,], color = 'black', aes(color = 'black')) + ggplot2::geom_sf(data = WQI_303d_dist_points, color = 'red', aes(color = 'red')) + ggplot2::geom_sf(data = impaired_lines_study_area)
        }
        
        
        
        
        
        
        
        
        
        
        
    # All facilities map
        # create the dataset
            all.facilities.map_data <- as.data.frame(facilities %>% dplyr::filter(REGION == input$region.selected_2))
            all.facilities.map_data <- all.facilities.map_data %>% dplyr::rename(Latitude = FACILITY_LATITUDE, Longitude = FACILITY_LONGITUDE)
        # get the sf (geospatial) data for the selected region
            rb_boundary_selected_facilities <- sf::st_as_sf(RB_Boundaries %>% dplyr::filter(RB_OFF == input$region.selected_2))
        # filter the CES polygons for the selected region ----------------------------------------------------------------------------------------------------------------------
            # filter using the RB boundary polygons
                tf_poly_bounds_facilities <- sf::st_intersects(ces_poly, rb_boundary_selected_facilities, sparse = FALSE) 
        # create a new variable with the selected ces polygons
            ces_poly_study_area_facilities <- ces_poly[tf_poly_bounds_facilities,] # creates a sf variable with just the CES polygons in / around the area where there are monitoring points
            # create a new column in the ces data frame called fill.variable, with the selected parameter data
                # Create the color palette for the CES polygons
                    param_facilities <- ces_choices[ces_choices$Name == input$ces.parameter_2,2]
                # create a numeric variable for the CES percentile that is in the middle of the values of the given 5% range
                    ces.percentile.numeric_facilities <- suppressWarnings(tidyr::separate(as.data.frame(ces_poly_study_area_facilities), Percentile, sep = '-', into = c('Percentile', 'Perc2'), convert = TRUE)[,2]+1.5) # error here because there are NAs in the CES percentile scores
                    ces_poly_study_area_facilities <- ces_poly_study_area_facilities %>% dplyr::mutate(ces.percentile.numeric_facilities = ces.percentile.numeric_facilities)
                # get the selected parameter of CES data - if CES percentile is selected, use the numeric field that was created from the factor values in the lines above
                    if (param_facilities == 'Percentile') {
                        ces.pal.domain_facilities <- as.data.frame(ces_poly_study_area_facilities %>% dplyr::select(ces.percentile.numeric_facilities))[,1]
                    } else {
                        ces.pal.domain_facilities <- as.data.frame(ces_poly_study_area_facilities %>% dplyr::select(param_facilities))[,1]
                    }
                    ces_poly_study_area_facilities <- ces_poly_study_area_facilities %>% dplyr::mutate(fill.variable = ces.pal.domain_facilities)
        # create a list of WDIDs in the CES polygons that meet the selected criteria
            # Create a sf object from the Facilities data
                facilities_sf <- sf::st_as_sf(facilities[!is.na(facilities$FACILITY_LATITUDE),], coords = c('FACILITY_LONGITUDE', 'FACILITY_LATITUDE'), crs = 4326, agr = 'constant')
             # find the CES polygons that meet the selected criteria (range of values)
                ces_poly_study_area_facilities_filtered <- ces_poly_study_area_facilities %>% dplyr::filter(fill.variable >= input$ces.score.range_2[1] & fill.variable <= input$ces.score.range_2[2]) %>% sf::st_as_sf()
            facilities_ces_filter <- sf::st_intersects(facilities_sf, ces_poly_study_area_facilities_filtered, sparse = TRUE) # lists the polygon number for each monitoring point; if no polygon number, the point is not in a polygon that meets the criteria
            tf_points_facilities <- as.logical(sapply(facilities_ces_filter, length)) # gives a logical vector where TRUE means the monitoring point meets the criteria (sapply and length argument needed because there may be more than 1 polygon in some cases)
            WDIDs_ces_filter_facilities <- (as.data.frame(facilities_sf)[tf_points_facilities,] %>% dplyr::select(WDID))[,1] # list of WDIDs in the polygons that satisfy the criteria
        # Filter for points in polygons that meet the CES filter criteria - only do this if a filter is applied because some points may fall outside of CES polygons (NOTE: May need to find a better way to handle those points that do fall outside of CES polygons)
            if (input$ces.score.range_2[1] > 0 | input$ces.score.range_2[2] < 100) {
                all.facilities.map_data <- all.facilities.map_data %>% dplyr::filter(WDID %in% WDIDs_ces_filter_facilities)
            }
        # Filter for the selected WDIDs (if selected)
            all.facilities.map_data <- tryCatch(expr = if(input$WDID.selected_2 != 'All WDIDs') {all.facilities.map_data <- all.facilities.map_data %>% dplyr::filter(WDID %in% input$WDID.selected_2)} else {all.facilities.map_data}, error = function(e) {all.facilities.map_data})
            
            
        # filter the 303d lines and polygons for the selected region ----------------------------------------------------------------------------------------------------------------------
            # first filter using the attributes in the 303d shapefile
                impaired_lines_region_facilities <- impaired_303d_lines %>% dplyr::filter(REGION_NUM == substr(x = input$region.selected_2, start = 1, stop = 1)) %>% sf::st_as_sf()
                impaired_polygons_region_facilities <- impaired_303d_polygons %>% dplyr::filter(REGION_NUM == substr(x = input$region.selected_2, start = 1, stop = 1)) %>% sf::st_as_sf()
            # then filter using the RB boundary polygons (for regions where there is more that 1 office / subregion)
                tf_impaired_bounds_lines_facilities <- sf::st_intersects(impaired_lines_region_facilities, rb_boundary_selected_facilities, sparse = FALSE) # sparse = FALSE because there is only one element in poly_bounds, so this just returns a single true/false list
                tf_impaired_bounds_polys_facilities <- sf::st_intersects(impaired_polygons_region_facilities, rb_boundary_selected_facilities, sparse = FALSE)
            # create a new variable with the selected 303d lines
                impaired_lines_study_area_facilities <- impaired_lines_region_facilities[tf_impaired_bounds_lines_facilities,] # creates a sf variable with just the impaired waters in / around the area where there are monitoring points
                impaired_polygons_study_area_facilities <- impaired_polygons_region_facilities[tf_impaired_bounds_polys_facilities,]
                
            # create shared map data, that links the map and the data table
                shared.map.data_facilities <- crosstalk::SharedData$new(all.facilities.map_data)
                
        # Create the map
            output$all.facilities.map <- leaflet::renderLeaflet({
                # create the empty map
                    l_facilities <- leaflet::leaflet(shared.map.data_facilities)
                # enter the basemap options to allow the user to select
                    basemap.options <- c('Esri.WorldTopoMap', 'CartoDB.Positron', 'Esri.WorldGrayCanvas','Esri.WorldImagery','Esri.WorldStreetMap') 
                    #'OpenStreetMap', 'OpenStreetMap.BlackAndWhite', 'Thunderforest.SpinalMap'
                # add the basemaps listed above to the map (for options, see: http://leaflet-extras.github.io/leaflet-providers/preview/)
                    for (provider in basemap.options) {
                        l_facilities <- l_facilities %>% leaflet::addProviderTiles(provider, group = provider)
                    }
                # add the min-map window
                    l_facilities <- l_facilities %>% leaflet::addMiniMap(tiles = basemap.options[[1]], toggleDisplay = TRUE, position = "bottomleft")
                # code to make the basemap/min-map selector work (copied from: https://rstudio.github.io/leaflet/morefeatures.html)
                    l_facilities <- l_facilities %>% htmlwidgets::onRender("
                                                         function(el, x) {
                                                         var myMap = this;
                                                         myMap.on('baselayerchange',
                                                         function (e) {
                                                         myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
                                                         })
                                                     }")
                # get the bounds of the regional board
                    bounds_facilities <- attributes(sf::st_geometry(rb_boundary_selected_facilities))$bbox
                # Set the bounds of the map dynamically - initial view is based on the full extent of the WQI score points for the selected region, after that the map is based on the most recent bounds when a new option (standard, period, etc) is selected
                    isolate(if (is.null(input$all.facilities.map_bounds)) {
                        l_facilities <- l_facilities %>% leaflet::fitBounds(lng1 = bounds_facilities[[1]], 
                                                                            lat1 = bounds_facilities[[2]], 
                                                                            lng2 = bounds_facilities[[3]], 
                                                                            lat2 = bounds_facilities[[4]])
                    } else { # maintain the current view
                        l_facilities <- l_facilities %>% leaflet::setView(lng = mean(c(input$all.facilities.map_bounds$west, input$all.facilities.map_bounds$east)), 
                                                                          lat = mean(c(input$all.facilities.map_bounds$north, input$all.facilities.map_bounds$south)), 
                                                                          zoom = input$all.facilities.map_zoom)                                
                    })
                # create a button to re-center the map
                    l_facilities <- l_facilities %>% leaflet::addEasyButton(leaflet::easyButton(
                        icon="fa-globe", title="Center Map on Regional Board Boundary",
                        # fit to WQI data points
                        # onClick=leaflet::JS(paste0('function(btn, map){ map.fitBounds([[',
                        #                   min(map.data$Latitude, na.rm = TRUE), ', ',
                        #                   min(map.data$Longitude, na.rm = TRUE), '],[',
                        #                   max(map.data$Latitude, na.rm = TRUE), ', ',
                        #                   max(map.data$Longitude, na.rm = TRUE), ']]); }'))))
                        # fit to RB boundary
                        onClick=leaflet::JS(paste0('function(btn, map){ map.fitBounds([[',
                                                   round(bounds_facilities[[2]],4), ', ',
                                                   round(bounds_facilities[[1]],4), '],[',
                                                   round(bounds_facilities[[4]],4), ', ',
                                                   round(bounds_facilities[[3]],4), ']]); }'))))
                # Add the CalEnvironScreen Polygons
                    l_facilities <- l_facilities %>% leaflet::addPolygons(data = ces_poly_study_area_facilities, 
                                                                          color = 'grey', # "#444444", 
                                                                          weight = 0.5, 
                                                                          smoothFactor = 1.0,
                                                                          opacity = 0.8, 
                                                                          fillOpacity = 0.5,
                                                                          # fillColor = ~colorNumeric('YlOrBr', Poll_pctl)(Poll_pctl), # view RColorBrewer palettes with: RColorBrewer::display.brewer.all()
                                                                          fillColor = ~ces.leaflet.pal(fill.variable),
                                                                          highlightOptions = leaflet::highlightOptions(color = "white", weight = 2),#,bringToFront = TRUE
                                                                          popup = ~paste0('<b>', '<u>','CalEnviroScreen 3.0 Tract', '</u>','</b>','<br/>',
                                                                                          '<b>', 'Tract: ', '</b>', Tract_1,'<br/>',
                                                                                          '<b>', 'Location: ', '</b>', City,', ', County, ' ', ZIP,'<br/>',
                                                                                          '<b>', 'Population: ', '</b>', Population,'<br/>','<br/>',
                                                                                          '<b>', 'Scores: ', '</b>','<br/>',
                                                                                          '<b>', 'CES Percentile: ', '</b>', Percentile,'<br/>',
                                                                                          '<b>', 'Impaired Waterbodies Percentile: ', '</b>', IWB_pctl,'<br/>',
                                                                                          '<b>', 'Pollution Burden Percentile: ', '</b>', Poll_pctl,'<br/>',
                                                                                          '<b>', 'Pesticides Percentile: ', '</b>', Pest_pctl,'<br/>',
                                                                                          '<b>', 'Toxic Releases Percentile: ', '</b>', TR_pctl,'<br/>',
                                                                                          '<b>', 'Cleanups Percentile: ', '</b>', Clean_Pctl,'<br/>',
                                                                                          '<b>', 'Groundwater Threats Percentile: ', '</b>', GW_pctl,'<br/>',
                                                                                          '<b>', 'Hazardous Waste Generators Percentile: ', '</b>', Haz_pctl),
                                                                          group = 'CES Polygons')#,'<br/>'
                # Add the 303d lines
                    l_facilities <- l_facilities %>% leaflet::addPolylines(data = impaired_lines_study_area_facilities, 
                                                                           color = 'blue',
                                                                           weight = 2.0, 
                                                                           opacity = 1.0, 
                                                                           # fillOpacity = 0.5,
                                                                           smoothFactor = 1.0,
                                                                           highlightOptions = leaflet::highlightOptions(color = "white", weight = 2),
                                                                           popup = ~paste0('<b>', '<u>','303d Listed Waterbody (2012)','</u>', '</b>','<br/>',
                                                                                           '<b>', 'Water Body Name: ', '</b>', WBNAME,'<br/>',
                                                                                           '<b>', 'Type: ', '</b>', WBTYPE,'<br/>',
                                                                                           '<b>', 'Region: ', '</b>', REGION_NUM, ' (', REGION_NAM,')','<br/>',
                                                                                           '<b>', 'ID: ', '</b>', WBID, '<br/>',
                                                                                           '<b>', 'Listed Pollutants: ', '</b>', Pollutant, '<br/>',
                                                                                           '<b>', 'Listing Comments: ', '</b>', Comments,  '<br/>',
                                                                                           '<b>', 'Potential Sources: ', '</b>', Sources),
                                                                           
                                                                           group = '303d Listed Waters')
                # Add the 303d polygons
                    l_facilities <- l_facilities %>% leaflet::addPolygons(data = impaired_polygons_study_area_facilities, 
                                                                          color = 'darkblue',
                                                                          weight = 0.5, 
                                                                          opacity = 0.8,
                                                                          fillColor = 'blue',
                                                                          fillOpacity = 0.5,
                                                                          smoothFactor = 1.0,
                                                                          highlightOptions = leaflet::highlightOptions(color = "white", weight = 2),
                                                                          popup = ~paste0('<b>', '<u>','303d Listed Waterbody (2012)', '</u>','</b>','<br/>',
                                                                                          '<b>', 'Water Body Name: ', '</b>', WBNAME,'<br/>',
                                                                                          '<b>', 'Type: ', '</b>', WBTYPE,'<br/>',
                                                                                          '<b>', 'Region: ', '</b>', REGION_NUM, ' (', REGION_NAM,')','<br/>',
                                                                                          '<b>', 'ID: ', '</b>', WBID, '<br/>',
                                                                                          '<b>', 'Listed Pollutants: ', '</b>', Pollutant, '<br/>',
                                                                                          '<b>', 'Listing Comments: ', '</b>', Comments, '<br/>',
                                                                                          '<b>', 'Potential Sources: ', '</b>', Sources),
                                                                          group = '303d Listed Waters')
                # Add the regional board boundary
                    l_facilities <- l_facilities %>% leaflet::addPolygons(data = rb_boundary_selected_facilities,
                                                                          color = 'black', # "#444444",
                                                                          weight = 2.0,
                                                                          smoothFactor = 1.0,
                                                                          opacity = 1.0,
                                                                          fill = FALSE,
                                                                          # fillOpacity = 0.5,
                                                                          # fillColor = 'lightblue',
                                                                          highlightOptions = highlightOptions(color = "white", weight = 2),#,bringToFront = TRUE
                                                                          popup = ~paste0('<b>', '<u>', 'Regional Board Boundary', '</u>', '</b>','<br/>',
                                                                                          '<b>', 'Region Number: ', '</b>', RB_OFF, '<br/>',
                                                                                          '<b>', 'Region Name: ', '</b>', RB_NAME),
                                                                          group = 'Regional Board Boundary'
                    )
                # Add the facilities
                    l_facilities <- l_facilities %>% leaflet::addCircleMarkers(radius = 4,
                                                                               stroke = TRUE, weight = 0.5, color = 'black', opacity = 1,
                                                                               fill = TRUE, fillOpacity = 1, fillColor = 'grey', # ~wqi.leaflet.pal(WQI),
                                                                               # clusterOptions = leaflet::markerClusterOptions(spiderfyDistanceMultiplier = 2),# freezeAtZoom = 13, maxClusterRadius = 10),#,#singleMarkerMode = TRUE),
                                                                               popup = ~paste0('<b>', '<u>', 'Facility Information', '</u>','</b>','<br/>',
                                                                                               '<b>', 'WDID: ', '</b>', WDID,'<br/>',
                                                                                               '<b>', 'Region: ', '</b>', REGION,'<br/>',
                                                                                               '<b>', 'Facility Name: ', '</b>', FACILITY_NAME, '<br/>',
                                                                                               '<b>', 'Operator: ', '</b>', OPERATOR_NAME, '<br/>',
                                                                                               '<b>', 'Status: ', '</b>', STATUS_CODE_NAME, '<br/>',
                                                                                               '<b>', 'Primary SIC: ', '</b>', PRIMARY_SIC,'<br/>',
                                                                                               '<b>', 'Address: ', '</b>', FACILITY_ADDRESS, '<br/>',
                                                                                               '<b>', 'City: ', '</b>', FACILITY_CITY, '<br/>',
                                                                                               '<b>', 'Receiving Water: ', '</b>', RECEIVING_WATER_NAME,'<br/>'
                                                                                               ),
                                                                               group = 'Facilities')
                # add the legend
                    l_facilities <- l_facilities %>% leaflet::addLegend(position = 'bottomright', colors = 'blue', opacity = 1.0, labels = '2012 303d Listed Waterbodies', layerId = '303d.list')
                    l_facilities <- l_facilities %>% leaflet::addLegend(position = 'bottomright', pal = ces.leaflet.pal, values = ces_poly_study_area_facilities$fill.variable, opacity = 1, layerId = 'ces.legend', bins = 4, title = paste0('CES**'))#: ', input$ces.parameter))
                    # l_facilities <- l_facilities %>% leaflet::addLegend(position = "bottomright", pal = wqi.leaflet.pal, values = WQI.Scores$WQI, title = "WQI*", opacity = 1, layerId = 'wqi.legend', bins = 2)
                # Add controls to select the basemap
                    l_facilities <- l_facilities %>% leaflet::addLayersControl(baseGroups = basemap.options,
                                                         overlayGroups = c('Facilities', 'CES Polygons', '303d Listed Waters', 'Regional Board Boundary'),
                                                         options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex = TRUE))
                # Add the measuring tool
                    l_facilities <- l_facilities %>% leaflet::addMeasure(position = 'topleft')
                # output the map object
                    l_facilities
        })
        
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
                
        
        # Create the mapping data 
            # create the dataset 
                map.data <- as.data.frame(WQI.Scores)
            # filter for the selected region
                # map.data <- map.data %>% dplyr::filter(Region_calc == input$region.selected)
            # Filter for the selected monitoring period
                map.data <- map.data %>% dplyr::filter(Monitoring.Period == input$monitoring.period)
                # map.data <- map.data %>% dplyr::filter(Monitoring.Period == '2015 - 2016')
            # Filter for the selected WQI range
                map.data <- map.data %>% dplyr::filter(WQI >= input$score.range[1] & WQI <= input$score.range[2])
            # Filter for points in polygons that meet the CES filter criteria - only do this if a filter is applied because some points may fall outside of CES polygons (NOTE: May need to find a better way to handle those points that do fall outside of CES polygons)
                if (input$ces.score.range[1] > 0 | input$ces.score.range[2] < 100) {
                    map.data <- map.data %>% dplyr::filter(WDID %in% WDIDs_ces_filter)
                }
            # Filter for points that meet the 303d proximity criteria
                if (!is.na(as.numeric(input$dist.to.303))) {
                    map.data <- map.data %>% dplyr::filter(WDID %in% WQI_303d_dist_points_list)
                }
            # Filter for the selected WDIDs (if selected)
                map.data <- tryCatch(expr = if(input$WDID.selected != 'All WDIDs') {map.data <- map.data %>% dplyr::filter(WDID %in% input$WDID.selected)} else {map.data}, error = function(e) {map.data})
            # create shared map data, that links the map and the data table
                shared.map.data <- crosstalk::SharedData$new(map.data)
        
        # create a dataset with all excluded points that have a WQI score for the chosen standard
            # points with a score for the selected monitoring period
                WQI.score.mon.period <- as.data.frame(WQI.Scores) %>% dplyr::filter(Monitoring.Period == input$monitoring.period)
            # find excluded points (points with a WQI score for the monitoring period, but not in the mapped data)
                excluded_WDIDs <- !(WQI.score.mon.period$WDID %in% map.data$WDID)
                excluded.points <- WQI.score.mon.period[excluded_WDIDs,]
                excluded.points <- excluded.points[!is.na(excluded.points$Latitude),] # get rid of points with missing lat/lon (those cause an error)
                excluded.points_sf <- sf::st_as_sf(excluded.points, coords = c('Longitude', 'Latitude'), crs = 4326, agr = 'constant')
        
        # Create the map
            output$monitoring.map <- leaflet::renderLeaflet({
                # create the empty map
                    l <- leaflet::leaflet(shared.map.data)
                # enter the basemap options to allow the user to select
                    basemap.options <- c('Esri.WorldTopoMap', 'CartoDB.Positron', 'Esri.WorldGrayCanvas','Esri.WorldImagery','Esri.WorldStreetMap') 
                    #'OpenStreetMap', 'OpenStreetMap.BlackAndWhite', 'Thunderforest.SpinalMap'
                # add the basemaps listed above to the map (for options, see: http://leaflet-extras.github.io/leaflet-providers/preview/)
                    for (provider in basemap.options) {
                        l <- l %>% leaflet::addProviderTiles(provider, group = provider)
                    }
                # add the min-map window
                    l <- l %>% leaflet::addMiniMap(tiles = basemap.options[[1]], toggleDisplay = TRUE, position = "bottomleft")
                # code to make the basemap/min-map selector work (copied from: https://rstudio.github.io/leaflet/morefeatures.html)
                    l <- l %>% htmlwidgets::onRender("
                                                     function(el, x) {
                                                     var myMap = this;
                                                     myMap.on('baselayerchange',
                                                     function (e) {
                                                     myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
                                                     })
                                                     }")
                # get the bounds of the regional board
                    bounds <- attributes(sf::st_geometry(rb.boundary.selected))$bbox
                # Set the bounds of the map dynamically - initial view is based on the full extent of the WQI score points for the selected region, after that the map is based on the most recent bounds when a new option (standard, period, etc) is selected
                    isolate(if (is.null(input$monitoring.map_bounds)) {
                        l <- l %>% leaflet::fitBounds(lng1 = bounds[[1]], lat1 = bounds[[2]], lng2 = bounds[[3]], lat2 = bounds[[4]])
                        # l <- l %>% leaflet::fitBounds(lng1 = min(monitoring.data.WQI$Longitude, na.rm = TRUE), lat1 = min(monitoring.data.WQI$Latitude, na.rm = TRUE), lng2 = max(monitoring.data.WQI$Longitude, na.rm = TRUE), lat2 = max(monitoring.data.WQI$Latitude, na.rm = TRUE))
                    } else { # maintain the current view
                        l <- l %>% leaflet::setView(lng = mean(c(input$monitoring.map_bounds$west, input$monitoring.map_bounds$east)), lat = mean(c(input$monitoring.map_bounds$north, input$monitoring.map_bounds$south)), zoom = input$monitoring.map_zoom)                                
                    })
                # create a button to re-center the map
                    l <- l %>% leaflet::addEasyButton(leaflet::easyButton(
                        icon="fa-globe", title="Center Map on Regional Board Boundary",
                        # fit to WQI data points
                        # onClick=leaflet::JS(paste0('function(btn, map){ map.fitBounds([[',
                        #                   min(map.data$Latitude, na.rm = TRUE), ', ',
                        #                   min(map.data$Longitude, na.rm = TRUE), '],[',
                        #                   max(map.data$Latitude, na.rm = TRUE), ', ',
                        #                   max(map.data$Longitude, na.rm = TRUE), ']]); }'))))
                        # fit to RB boundary
                        onClick=leaflet::JS(paste0('function(btn, map){ map.fitBounds([[',
                                                   round(bounds[[2]],4), ', ',
                                                   round(bounds[[1]],4), '],[',
                                                   round(bounds[[4]],4), ', ',
                                                   round(bounds[[3]],4), ']]); }'))))
                # Add the CalEnvironScreen Polygons
                    l <- l %>% leaflet::addPolygons(data = ces_poly_study_area, 
                                                    color = 'grey', # "#444444", 
                                                    weight = 0.5, 
                                                    smoothFactor = 1.0,
                                                    opacity = 0.8, 
                                                    fillOpacity = 0.5,
                                                    # fillColor = ~colorNumeric('YlOrBr', Poll_pctl)(Poll_pctl), # view RColorBrewer palettes with: RColorBrewer::display.brewer.all()
                                                    fillColor = ~ces.leaflet.pal(fill.variable),
                                                    highlightOptions = leaflet::highlightOptions(color = "white", weight = 2),#,bringToFront = TRUE
                                                    popup = ~paste0('<b>', '<u>','CalEnviroScreen 3.0 Tract', '</u>','</b>','<br/>',
                                                                    '<b>', 'Tract: ', '</b>', Tract_1,'<br/>',
                                                                    '<b>', 'Location: ', '</b>', City,', ', County, ' ', ZIP,'<br/>',
                                                                    '<b>', 'Population: ', '</b>', Population,'<br/>','<br/>',
                                                                    '<b>', 'Scores: ', '</b>','<br/>',
                                                                    '<b>', 'CES Percentile: ', '</b>', Percentile,'<br/>',
                                                                    '<b>', 'Impaired Waterbodies Percentile: ', '</b>', IWB_pctl,'<br/>',
                                                                    '<b>', 'Pollution Burden Percentile: ', '</b>', Poll_pctl,'<br/>',
                                                                    '<b>', 'Pesticides Percentile: ', '</b>', Pest_pctl,'<br/>',
                                                                    '<b>', 'Toxic Releases Percentile: ', '</b>', TR_pctl,'<br/>',
                                                                    '<b>', 'Cleanups Percentile: ', '</b>', Clean_Pctl,'<br/>',
                                                                    '<b>', 'Groundwater Threats Percentile: ', '</b>', GW_pctl,'<br/>',
                                                                    '<b>', 'Hazardous Waste Generators Percentile: ', '</b>', Haz_pctl),
                                                    group = 'CES Polygons')#,'<br/>'
                # Add the 303d lines
                    l <- l %>% leaflet::addPolylines(data = impaired_lines_study_area, 
                                                     color = 'blue',
                                                     weight = 2.0, 
                                                     opacity = 1.0, 
                                                     # fillOpacity = 0.5,
                                                     smoothFactor = 1.0,
                                                     highlightOptions = leaflet::highlightOptions(color = "white", weight = 2),
                                                     popup = ~paste0('<b>', '<u>','303d Listed Waterbody (2012)','</u>', '</b>','<br/>',
                                                                     '<b>', 'Water Body Name: ', '</b>', WBNAME,'<br/>',
                                                                     '<b>', 'Type: ', '</b>', WBTYPE,'<br/>',
                                                                     '<b>', 'Region: ', '</b>', REGION_NUM, ' (', REGION_NAM,')','<br/>',
                                                                     '<b>', 'ID: ', '</b>', WBID, '<br/>',
                                                                     '<b>', 'Listed Pollutants: ', '</b>', Pollutant, '<br/>',
                                                                     '<b>', 'Listing Comments: ', '</b>', Comments,  '<br/>',
                                                                     '<b>', 'Potential Sources: ', '</b>', Sources),
                                                     
                                                     group = '303d Listed Waters')
                # Add the 303d polygons
                    l <- l %>% leaflet::addPolygons(data = impaired_polygons_study_area, 
                                                    color = 'darkblue',
                                                    weight = 0.5, 
                                                    opacity = 0.8,
                                                    fillColor = 'blue',
                                                    fillOpacity = 0.5,
                                                    smoothFactor = 1.0,
                                                    highlightOptions = leaflet::highlightOptions(color = "white", weight = 2),
                                                    popup = ~paste0('<b>', '<u>','303d Listed Waterbody (2012)', '</u>','</b>','<br/>',
                                                                    '<b>', 'Water Body Name: ', '</b>', WBNAME,'<br/>',
                                                                    '<b>', 'Type: ', '</b>', WBTYPE,'<br/>',
                                                                    '<b>', 'Region: ', '</b>', REGION_NUM, ' (', REGION_NAM,')','<br/>',
                                                                    '<b>', 'ID: ', '</b>', WBID, '<br/>',
                                                                    '<b>', 'Listed Pollutants: ', '</b>', Pollutant, '<br/>',
                                                                    '<b>', 'Listing Comments: ', '</b>', Comments, '<br/>',
                                                                    '<b>', 'Potential Sources: ', '</b>', Sources),
                                                    group = '303d Listed Waters')
                # Add 303d proximity buffer (if selected)
                    if (input$show.303d.buffer == TRUE & !is.na(as.numeric(input$dist.to.303))) {
                        l <- l %>% leaflet::addPolygons(data = buffered_streams_geographic,
                                                        color = 'darkblue', # "#444444",
                                                        weight = 0.5,
                                                        smoothFactor = 1.0,
                                                        opacity = 1.0,
                                                        fillOpacity = 0.5,
                                                        fillColor = 'lightblue',
                                                        highlightOptions = highlightOptions(color = "white", weight = 2),#,bringToFront = TRUE
                                                        popup = ~paste0('<b>', '<u>', '303d Buffer', '</b>','</u>','<br/>',
                                                                        '<b>', 'Water Body Name: ', '</b>', WBNAME,'<br/>',
                                                                        '<b>', 'Water Body Type: ', '</b>', WBTYPE,'<br/>',
                                                                        '<b>', 'Region: ', '</b>', REGION_NUM, ' (', REGION_NAM,')','<br/>',
                                                                        '<b>', 'ID: ', '</b>', WBID),
                                                        group = '303d Buffers'
                        )
                    }
                # Add the regional board boundary
                    l <- l %>% leaflet::addPolygons(data = rb.boundary.selected,
                                                    color = 'black', # "#444444",
                                                    weight = 2.0,
                                                    smoothFactor = 1.0,
                                                    opacity = 1.0,
                                                    fill = FALSE,
                                                    # fillOpacity = 0.5,
                                                    # fillColor = 'lightblue',
                                                    highlightOptions = highlightOptions(color = "white", weight = 2),#,bringToFront = TRUE
                                                    popup = ~paste0('<b>', '<u>', 'Regional Board Boundary', '</u>', '</b>','<br/>',
                                                                    '<b>', 'Region Number: ', '</b>', RB_OFF, '<br/>',
                                                                    '<b>', 'Region Name: ', '</b>', RB_NAME),
                                                    group = 'Regional Board Boundary'
                    )
                # Add the excluded points (by all filters) (if selected)
                    if (input$show.excluded.points == TRUE) { # & input$show.303d.buffer == TRUE & !is.na(as.numeric(input$dist.to.303))
                        l <- l %>% leaflet::addCircleMarkers(data = excluded.points_sf,
                                                             radius = 4,
                                                             stroke = TRUE, weight = 0.5, color = 'black', opacity = 1,
                                                             fill = TRUE, fillOpacity = 1, fillColor = 'grey',
                                                             popup = ~paste0('<b>', '<u>','Excluded Discharger WQI Score', '</u>','</b>','<br/>',
                                                                             '<b>', 'Facility Information:','</b>','<br/>',
                                                                             '<b>', 'WDID: ', '</b>', WDID,'<br/>',
                                                                             '<b>', 'Facility Name: ', '</b>', FACILITY_NAME,'<br/>',
                                                                             '<b>', 'SIC: ', '</b>', PRIMARY_SIC,'<br/>',
                                                                             '<b>', 'Address: ', '</b>', FACILITY_ADDRESS, '<br/>',
                                                                             '<b>', 'City: ', '</b>', FACILITY_CITY, '<br/>',
                                                                             '<b>', 'Receiving Water: ', '</b>', RECEIVING_WATER_NAME,'<br/>',
                                                                             '<br/>',
                                                                             '<b>', 'Scoring:','</b>','<br/>',
                                                                             '<b>', 'Monitoring Period: ', '</b>', Monitoring.Period,'<br/>',
                                                                             '<b>', 'Standard: ', '</b>', Standard.Type,'<br/>',
                                                                             '<b>', 'Total Samples: ', '</b>', Total.Samples,'<br/>',
                                                                             '<b>', 'Exceedence Frequency: ', '</b>', round(F1,0),'<br/>',
                                                                             '<b>', 'Exceedence Magnitude: ', '</b>', round(F2,0),'<br/>',
                                                                             '<b>', 'WQI: ', '</b>', WQI, '<br/>'),
                                                             group = 'Excluded Points'
                        )
                    } 
                # Add the selected WQI data
                    # if (input$show.parameters == TRUE) {
                        l <- l %>% leaflet::addCircleMarkers(radius = 4,
                                                             stroke = TRUE, weight = 0.5, color = 'black', opacity = 1,
                                                             fill = TRUE, fillOpacity = 1, fillColor = ~wqi.leaflet.pal(WQI),
                                                             # clusterOptions = leaflet::markerClusterOptions(spiderfyDistanceMultiplier = 2),# freezeAtZoom = 13, maxClusterRadius = 10),#,#singleMarkerMode = TRUE),
                                                             popup = ~paste0('<b>', '<u>', 'Discharger WQI Score', '</u>','</b>','<br/>',
                                                                             '<b>', 'Facility Information:','</b>','<br/>',
                                                                             '<b>', 'WDID: ', '</b>', WDID,'<br/>',
                                                                             '<b>', 'Facility Name: ', '</b>', FACILITY_NAME,'<br/>',
                                                                             '<b>', 'SIC: ', '</b>', PRIMARY_SIC,'<br/>',
                                                                             '<b>', 'Address: ', '</b>', FACILITY_ADDRESS, '<br/>',
                                                                             '<b>', 'City: ', '</b>', FACILITY_CITY, '<br/>',
                                                                             '<b>', 'Receiving Water: ', '</b>', RECEIVING_WATER_NAME,'<br/>',
                                                                             '<br/>',
                                                                             '<b>','Scoring:','</b>','<br/>',
                                                                             '<b>', 'Monitoring Period: ', '</b>', Monitoring.Period,'<br/>',
                                                                             '<b>', 'Standard: ', '</b>', Standard.Type,'<br/>',
                                                                             '<b>', 'Total Samples: ', '</b>', Total.Samples,'<br/>',
                                                                             '<b>', 'Parameters In WQI Score: ', '</b>', Parameters.In.Score, '<br/>',
                                                                             '<b>', 'Exceedence Frequency: ', '</b>', round(F1,0),'<br/>',
                                                                             '<b>', 'Exceedence Magnitude: ', '</b>', round(F2,0),'<br/>',
                                                                             '<b>', 'WQI: ', '</b>', WQI, '<br/>'),
                                                             group = 'WQI Scores')
                    # }
                    # if (input$show.parameters == FALSE) {
                    #     l <- l %>% leaflet::addCircleMarkers(radius = 4,
                    #                                          stroke = TRUE, weight = 0.5, color = 'black', opacity = 1,
                    #                                          fill = TRUE, fillOpacity = 1, fillColor = ~wqi.leaflet.pal(WQI),
                    #                                          # clusterOptions = leaflet::markerClusterOptions(spiderfyDistanceMultiplier = 2),# freezeAtZoom = 13, maxClusterRadius = 10),#,#singleMarkerMode = TRUE),
                    #                                          popup = ~paste0('<b>', '<u>', 'Discharger WQI Score', '</u>', '</b>','<br/>',
                    #                                                          '<b>', 'Facility Information:','</b>','<br/>',
                    #                                                          '<b>', 'WDID: ', '</b>', WDID,'<br/>',
                    #                                                          '<b>', 'Facility Name: ', '</b>', FACILITY_NAME,'<br/>',
                    #                                                          '<b>', 'SIC: ', '</b>', PRIMARY_SIC,'<br/>',
                    #                                                          '<b>', 'Address: ', '</b>', FACILITY_ADDRESS, '<br/>',
                    #                                                          '<b>', 'City: ', '</b>', FACILITY_CITY, '<br/>',
                    #                                                          '<b>', 'Receiving Water: ', '</b>', RECEIVING_WATER_NAME,'<br/>',
                    #                                                          '<br/>',
                    #                                                          '<b>', 'Scoring:','</b>','<br/>',
                    #                                                          '<b>', 'Monitoring Period: ', '</b>', Monitoring.Period,'<br/>',
                    #                                                          '<b>', 'Standard: ', '</b>', Standard.Type,'<br/>',
                    #                                                          '<b>', 'Total Samples: ', '</b>', Total.Samples,'<br/>',
                    #                                                          '<b>', 'Exceedence Frequency: ', '</b>', round(F1,0),'<br/>',
                    #                                                          '<b>', 'Exceedence Magnitude: ', '</b>', round(F2,0),'<br/>',
                    #                                                          '<b>', 'WQI: ', '</b>', WQI, '<br/>'),
                    #                                          group = 'WQI Scores')
                    # }
                
            
            # add the legend
                l <- l %>% leaflet::addLegend(position = 'bottomright', colors = 'blue', opacity = 1.0, labels = '2012 303d Listed Waterbodies', layerId = '303d.list')
                l <- l %>% leaflet::addLegend(position = 'bottomright', pal = ces.leaflet.pal, values = ces_poly_study_area$fill.variable, opacity = 1, layerId = 'ces.legend', bins = 4, title = paste0('CES**'))#: ', input$ces.parameter))
                l <- l %>% leaflet::addLegend(position = "bottomright", pal = wqi.leaflet.pal, values = WQI.Scores$WQI, title = "WQI*", opacity = 1, layerId = 'wqi.legend', bins = 2)
            
            # Add controls to select the basemap
                l <- l %>% leaflet::addLayersControl(baseGroups = basemap.options,
                                                     overlayGroups = c('WQI Scores', 'CES Polygons', '303d Listed Waters', 'Regional Board Boundary', '303d Buffers', 'Excluded Points'),
                                                     options = leaflet::layersControlOptions(collapsed = TRUE, autoZIndex = TRUE)) 
            
            # Add the measuring tool
                l <- l %>% leaflet::addMeasure(position = 'topleft')
            # output the map object
                l
        })
        
        # Data Table -----------------------------------------------------------
            output$WQI.table <- DT::renderDataTable(
                shared.map.data, 
                extensions = c('Buttons', 'Scroller'),
                options = list(dom = 'Bfrtip', 
                               buttons = list('colvis', list(
                                   extend = 'collection',
                                   buttons = list(list(extend='csv', filename = 'WQI_Scores'),
                                                  list(extend='excel', filename= 'WQI_Scores')),
                                   text = 'Download Data' )),
                               scrollX = TRUE,
                               scrollY = 250, 
                               scroller = TRUE, 
                               deferRender = TRUE),
                class = 'cell-border stripe',
                server = FALSE,
                rownames = FALSE
            )
            
            # facilities map
            output$facilities.map.table <- DT::renderDataTable(
                shared.map.data_facilities, 
                extensions = c('Buttons', 'Scroller'),
                options = list(dom = 'Bfrtip', 
                               buttons = list('colvis', list(
                                   extend = 'collection',
                                   buttons = list(list(extend='csv', filename = 'Filtered_Facilities'),
                                                  list(extend='excel', filename= 'Filtered_Facilities')),
                                   text = 'Download Data' )),
                               scrollX = TRUE,
                               scrollY = 250, 
                               scroller = TRUE, 
                               deferRender = TRUE),
                class = 'cell-border stripe',
                server = FALSE,
                rownames = FALSE
            )
            
            # output$all_facilities_table <- DT::renderDataTable(
            #     shared.map.data, 
            #     extensions = c('Buttons', 'Scroller'),
            #     options = list(dom = 'Bfrtip', 
            #                    buttons = list('colvis', list(
            #                        extend = 'collection',
            #                        buttons = list(list(extend='csv', filename = 'WQI_Scores'),
            #                                       list(extend='excel', filename= 'WQI_Scores')),
            #                        text = 'Download Data' )),
            #                    scrollX = TRUE,
            #                    scrollY = 250, 
            #                    scroller = TRUE, 
            #                    deferRender = TRUE),
            #     class = 'cell-border stripe',
            #     server = FALSE,
            #     rownames = FALSE
            # )
        
        
        # Monitoring Data Download -------------------------------------------------
            output$downloadRawData <- downloadHandler(
                filename = 'WQI_Monitoring_Data.csv', 
                content = function(con) {
                    write.csv(monitoring.data.WQI, con, row.names = FALSE)
                },
                contentType = 'text/csv'
            )
        
        # # Standards Data Download ------------------------------------------------
            #     output$downloadStandards <- downloadHandler(
            #         filename = 'WQI_Standards.csv', 
            #         content = function(con) {
            #             write.csv(standards, con, row.names = FALSE)
            #         },
            #         contentType = 'text/csv'
            #     )
        
        # Facilities Data Download -------------------------------------------------
            output$downloadFacilities <- downloadHandler(
                filename = 'WQI_Facilities.csv', 
                content = function(con) {
                    write.csv(facilities, con, row.names = FALSE)
                },
                contentType = 'text/csv'
            )    
})
    
    # Center map on change in region selection
        observeEvent(input$region.selected, {
            # fit to monitoring data for selected region
            # filtered.data <- monitoring.data %>% dplyr::filter(Region_calc == input$region.selected)
            # if (data.type == 'SMARTS') {
            #     filtered.data <- filtered.data %>% dplyr::mutate(Parameter = gsub(x = Parameter, pattern = ',', replacement = ''))
            # }
            # filtered.data <- filtered.data %>% dplyr::filter(Parameter %in% standards$Parameter)
            # filtered.data <- filtered.data[(!is.na(filtered.data$Latitude) & !is.na(filtered.data$Longitude)), ]
            # filtered.data <- filtered.data %>% dplyr::group_by(WDID, Latitude, Longitude) %>% dplyr::distinct(WDID)
            # fit to RB boundary - get RB bounds
            rb.boundary.selected <- sf::st_as_sf(RB_Boundaries %>% dplyr::filter(RB_OFF == input$region.selected))
            bounds <- attributes(sf::st_geometry(rb.boundary.selected))$bbox
            # update map to appropriate bounds
            leaflet::leafletProxy(mapId = 'monitoring.map') %>% 
                # leaflet::fitBounds(lng1 = min(filtered.data$Longitude, na.rm = TRUE), lat1 = min(filtered.data$Latitude, na.rm = TRUE), lng2 = max(filtered.data$Longitude, na.rm = TRUE), lat2 = max(filtered.data$Latitude, na.rm = TRUE))
                leaflet::fitBounds(lng1 = bounds[[1]], 
                                   lat1 = bounds[[2]], 
                                   lng2 = bounds[[3]], 
                                   lat2 = bounds[[4]])
        })
        
    # Center facilities map on change in region selection
        observeEvent(input$region.selected_2, {
            # fit to RB boundary - get RB bounds
                rb.boundary.selected_facilities <- sf::st_as_sf(RB_Boundaries %>% dplyr::filter(RB_OFF == input$region.selected_2))
                bounds_facilities <- attributes(sf::st_geometry(rb.boundary.selected_facilities))$bbox
            # update map to appropriate bounds
                leaflet::leafletProxy(mapId = 'all.facilities.map') %>% 
                    # leaflet::fitBounds(lng1 = min(filtered.data$Longitude, na.rm = TRUE), lat1 = min(filtered.data$Latitude, na.rm = TRUE), lng2 = max(filtered.data$Longitude, na.rm = TRUE), lat2 = max(filtered.data$Latitude, na.rm = TRUE))
                    leaflet::fitBounds(lng1 = bounds_facilities[[1]], 
                                       lat1 = bounds_facilities[[2]], 
                                       lng2 = bounds_facilities[[3]], 
                                       lat2 = bounds_facilities[[4]])
        })
    
    # Reset Standards
        observeEvent(input$reset, {
            DF <- standards %>% dplyr::select(-Standard.water.type) %>%  tidyr::spread(Standard.Type, Standard) %>% dplyr::mutate(Custom = '')
            DF$Custom <- as.numeric(DF$Custom)
            values[["DF"]] <- DF
        })   
    
        output$hot <- rhandsontable::renderRHandsontable({
            DF <- values[["DF"]]
            if (!is.null(DF))
                rhandsontable::rhandsontable(DF, useTypes = TRUE, stretchH = "all")
        })    
    
    
    }

# Run the application 
    shinyApp(ui = ui, server = server)
