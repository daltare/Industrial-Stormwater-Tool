library(shiny)
library(DT)
library(crosstalk)
library(leaflet)
library(magrittr)
# library(DBI)
# library(odbc)
library(dplyr)
library(tidyverse)
library(rhandsontable)
library(sf)
library(rmapshaper)
library(units)

`%>%` <- magrittr::`%>%`

# # get data from the database --------------------------------------------------------------------------------------------------------------------
#     # NOTE: HAVE TO SET THE R VERSION TO 32 BIT FIRST (go to: Tools -> Global Options -> General -> R version)
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
# 
data.type <- 'SMARTS'
# data.type <- 'Database'
#
# get data from extracted tables ------------------------------------------------------------------------------------------------------------------
    # standards
        standards <- suppressMessages(readr::read_tsv('data/Standards.txt'))
        names(standards) <- make.names(names(standards))
    # monitoring data
        if (data.type == 'Database') {
            monitoring.data <- suppressMessages(readr::read_tsv('data/SMARTS_monitoring_data.txt'))
        }
        if (data.type == 'SMARTS') { 
            # fix errors encountered in reading the SMARTS monitoring data file by first finding and replacing any quoted characters in the data
                monitoring_lines <- readLines('data/SMARTS_data/Industiral_Monitoring_Data.txt') # save the data as lines of text
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
                monitoring.data <- monitoring.data %>% dplyr::mutate(Sample_date_calc = lubridate::mdy(Sample_date_calc))
        }
        if (data.type == 'SMARTS') {
            # Rename columns            
                monitoring.data <- monitoring.data %>% dplyr::rename(Parameter = PARAMETER, Result = RESULT, Units = UNITS)
            # convert date field to a date class
                monitoring.data <- monitoring.data %>% dplyr::mutate(Sample_date_calc = lubridate::mdy(SAMPLE_DATE))
            # Rename latitude and longitude columns
                monitoring.data <- monitoring.data %>% dplyr::rename(Latitude = MONITORING_LATITUDE) %>% dplyr::rename(Longitude = MONITORING_LONGITUDE)
        }
            
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
    #     unique_points <- monitoring_sf %>% distinct(WDID, .keep_all = TRUE) 
        


# Data transformations (standards) --------------------------------------------------------------------------------------------------------------
    # change water type column name
        standards <- standards %>% dplyr::rename(Standard.water.type = Water.type) 


# Regional Board Office Boundaries --------------------------------------------------------------------------------------------------------------
        # These steps show how to access and transform the RB boundary geospatial data, but they only need to be done once:
        # RB_Bounds <- sf::st_read('data/Regional_Board_Office_Boundaries_ModifiedR6_6A_6B/Regional_Board_Offices.shp')
        # RB_Bounds_transform <- sf::st_transform(RB_Bounds, 4326)
        # RB_Bounds_simple <- rmapshaper::ms_simplify(RB_Bounds_transform)
        # saveRDS(object = RB_Bounds_simple, file = 'data/simplified_RB_Bounds.RDS')
        # Read the data from the saved RDS file
        RB_Boundaries <- readr::read_rds('data/simplified_RB_Bounds.RDS')
        # g <- ggplot2::ggplot() + ggplot2::geom_sf(data = RB_Boundaries)
        
        
# CalEnviroScreen Polygons ----------------------------------------------------------------------------------------------------------------------
    # These steps show how to access and transform the CES geospatial data, but they only need to be done once:
        # temp_zip <- tempfile()
        # ces_url <- 'https://oehha.ca.gov/media/downloads//ces3shp.zip'  # ALTERNATVIE: ces_url <- 'https://data.ca.gov/sites/default/files/CES3Results_SHP.zip'
        # download.file(url = ces_url, destfile = temp_zip, method = 'curl')
        # unzip(zipfile = temp_zip, exdir = 'data/CES3Results', junkpaths = TRUE) #files = c('CES3Results.shp','CES3Results.shx','CES3Results.prj'),
        # unlink(temp_zip)
        # ces <- sf::st_read('data/CES3Results/CES3Results.shp')
        # ces_transform <- st_transform(ces, 4326)
        # ces_simple <- rmapshaper::ms_simplify(ces_transform)
        # saveRDS(object = ces_simple, file = 'data/simplified_CalEnvironScreen_poly.RDS')
        # Read the data from the saved RDS file
        ces_poly <- readr::read_rds('data/simplified_CalEnvironScreen_poly.RDS')
    

        
    
    # # filter for CES polygons that contain monitoring points
    #     ces_monitoring_points <- st_within(unique_points, ces_poly, sparse = TRUE) # lists the polygon number for each monitoring point
    #     tf_ces_monitoring_points <- (1:nrow(ces_poly)) %in% ces_monitoring_points # checks to see if each polygon is in the list created above
    #     ces_poly_monitoring_points <- ces_poly[tf_ces_monitoring_points,] # filters for the polygons in the list
    # 
    # # filter for CES polygons with high pollution load
    #     ces_highPol <- ces_poly %>% filter(Poll_pctl >= 80)                
    #     ces_study_area_highPol <- ces_poly_study_area %>% filter(Poll_pctl >= 80)
    # 
    # # get the monitoring points in the high pollution polygons
    #     points_within_hipoll_polygon <- st_within(unique_points, ces_highPol, sparse = TRUE) # lists the polygon number for each monitoring point
    #     tf_points <- !is.na(as.logical(points_within_hipoll_polygon)) # creates a true/false list of the same lenth as the number of monitoring points (true = point in selected polygons)
    #     
    # # map only polygons in the study area, along with the points
    #     g <- ggplot() + geom_sf(data = ces_poly_study_area)
    #     g <- g + geom_sf(data = unique_points)
    #     
    # # map high pollution polygons in the study area, and monitoring points in those polygons
    #     g1 <- ggplot() + geom_sf(data = ces_study_area_highPol) + geom_sf(data = unique_points[tf_points,])
    #     
    # # map high pollution polygons in the study area, and all monitoring points
    #     g2 <- ggplot() + geom_sf(data = ces_study_area_highPol) + geom_sf(data = unique_points)
    #     
    # # map all high pollution polygons
    #     g <- ggplot() + geom_sf(data = ces_highPol)
    
    # CES parameter choices
        ces_choices <- data.frame(Name = c('CES Percentile', 'Pollution Burden', 'Impaired Water Bodies'), CES.Variable = c('Percentile', 'Poll_pctl', 'IWB_pctl'), stringsAsFactors = FALSE) #' CES Percentile' = 'Percentile'

# 303d Lines ------------------------------------------------------------------------------------------------------------------------------------
    # These steps show how to access and transform the 303d geospatial line data, but they only need to be done once:
        # temp_zip_impaired <- tempfile()
        # impaired_url <- 'https://gispublic.waterboards.ca.gov/webmap/303d_2012/files/2012_Impaired_Lines_Final.zip'
        # download.file(url = impaired_url, destfile = temp_zip_impaired, method = 'curl')
        # unzip(zipfile = temp_zip_impaired, exdir = 'data/2012_Impaired_Lines_Final', junkpaths = TRUE)
        # unlink(temp_zip_impaired)
        # impaired_303d <- sf::st_read('data/2012_Impaired_Lines_Final/2012_Impaired_Lines_Final.shp')
        # impaired_303d_transform <- sf::st_transform(impaired_303d, 4326)
        # impaired_303d_simple <- rmapshaper::ms_simplify(impaired_303d_transform)
        # saveRDS(object = impaired_303d_simple, file = 'data/simplified_2012_303d_lines.RDS')
        # Read the data from the saved RDS file
            impaired_303d_lines <- readr::read_rds('data/simplified_2012_303d_lines.RDS')
        # get rid of 303d lines in region 1, because they're very dense and crash the tool
            impaired_303d_lines <- impaired_303d_lines %>% dplyr::filter(REGION_NUM != 1)

    # filter for monitoring points within a certain distance of impaired waterbodies
    # for testing only:
        # WQI.Scores <- as_data_frame(monitoring.data %>% dplyr::group_by(WDID, Latitude, Longitude) %>% summarize(number = n()))
        # WQI.Scores_sf <- sf::st_as_sf(WQI.Scores[!is.na(WQI.Scores$Latitude),], coords = c('Longitude', 'Latitude'), crs = 4326, agr = 'constant')
        # WQI.Scores_sf_mercator <- sf::st_transform(WQI.Scores_sf, 3857)
        # impaired_lines_study_area_mercator <- sf::st_transform(impaired_lines_study_area, 3857)
        #  z <- units::set_units(2000, m)
        # WQI_303d_dist_check <- sf::st_is_within_distance(WQI.Scores_sf_mercator, impaired_lines_study_area_mercator, dist = z)
        # 
        # tf_points_303_distance <- as.logical(sapply(WQI_303d_dist_check, length)) # gives a logical vector where TRUE means the monitoring point meets the criteria (sapply and length argument needed because there may be more than 1 polygon in some cases)
        # WQI_303d_dist_points <- WQI.Scores_sf[tf_points_303_distance,] # points that satisfy the distance to 303d waters criteria
        # buffered_streams <- sf::st_buffer(x = impaired_lines_study_area_mercator, dist = z)
        # 
        # # map points (red = within distance criteria) + streams + buffers
        #     g <- ggplot() + geom_sf(data = buffered_streams) + geom_sf(data = WQI.Scores_sf[!tf_points_303_distance,], color = 'black') + geom_sf(data = WQI_303d_dist_points, color = 'red') + geom_sf(data = impaired_lines_study_area)

            
# 303d Polygons ------------------------------------------------------------------------------------------------------------------------------------
    # These steps show how to access and transform the 303d geospatial polygon data, but they only need to be done once:
        # temp_zip_impaired_poly <- tempfile()
        # impaired_poly_url <- 'https://gispublic.waterboards.ca.gov/webmap/303d_2012/files/2012_Impaired_Polys_Final.zip'
        # download.file(url = impaired_poly_url, destfile = temp_zip_impaired_poly, method = 'curl')
        # unzip(zipfile = temp_zip_impaired_poly, exdir = 'data/2012_Impaired_Polygons_Final', junkpaths = TRUE)
        # unlink(temp_zip_impaired)
        # impaired_303d_poly <- sf::st_read('data/2012_Impaired_Polygons_Final/2012_Impaired_Polys_Final.shp')
        # impaired_303d_transform_poly <- sf::st_transform(impaired_303d_poly, 4326)
        # impaired_303d_simple_poly <- rmapshaper::ms_simplify(impaired_303d_transform_poly)
        # saveRDS(object = impaired_303d_simple_poly, file = 'data/simplified_2012_303d_polygons.RDS')
        # Read the data from the saved RDS file
            impaired_303d_polygons <- readr::read_rds('data/simplified_2012_303d_polygons.RDS')
            # g <- ggplot2::ggplot() + ggplot2::geom_sf(data = dplyr::filter(impaired_303d_polygons, REGION_NUM == 9))
            
# 303d Waterbodies Pollutant Information (table)
    # These steps show how to access and transform the 303d tabular data, but they only need to be done once:
        # download.file(url = 'https://gispublic.waterboards.ca.gov/webmap/303d_2012/files/2012_USEPA_approv_303d_List_Final_20150807.xlsx', destfile = 'data/2012_USEPA_approv_303d_List_Final_20150807.xlsx', method = 'curl')
        # impaired_pollutants <- readxl::read_excel('data/2012_USEPA_approv_303d_List_Final_20150807.xlsx', sheet = 'Final 303(d) List')
        # names(impaired_pollutants) <- make.names(names(impaired_pollutants))
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

            
# 303d Pollutant Potential Sources (table 2)
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
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!       
            

# ----------------------------------------------------------------------------------------------------------------------------------------------- #       
# ----------------------------------------------------------------------------------------------------------------------------------------------- #
# Define UI --------------------------------------------------------------------
ui <- fluidPage(
    # Application title
    titlePanel("Stormwater Enforcement Tool"),
    
    sidebarLayout(
        # Sidebar with inputs     
        sidebarPanel(
            withMathJax(), # to create equations
            # Filters
            # h3('Filters:'),
            selectInput(inputId = 'region.selected', label = 'Select Water Board Region:', choices = c(1:4, '5R', '5S', '6A', '6B', 7:9), selected = '9'),            
            selectInput(inputId = 'standard',label = 'Select Standard:', choices = c('CTR', 'MSGP - Benchmark', 'NAL', 'Custom')),
            selectInput(inputId = 'monitoring.period', label = 'Select Monitoring Period:', choices = periods.list, selected = '2016 - 2017'),
            # htmlOutput('monitoring.period.selector'),
            selectInput(inputId = 'WDID.selected', label = 'Select Facility WDIDs (Optional):', choices = WDID.list, multiple = TRUE, selected = WDID.list[1]),
            sliderInput(inputId = 'score.range', label = 'Select WQI Score Range:', min = 0, max = 100, value = c(0,100)),
            # actionButton('refresh','Update')
            selectInput(inputId = 'ces.parameter', 'Select a CES Parameter:', choices = ces_choices$Name, selected = 'Pollution Burden'),
            sliderInput(inputId = 'ces.score.range', label = 'Filter by Score of Selected CES Parameter:', min = 0, max = 100, value = c(0,100)),
            textInput(inputId = 'dist.to.303', label = 'Filter for proximity to a 303d listed water body (ft):', placeholder = 'Enter a distance in feet'),
            checkboxInput(inputId = 'show.303d.buffer', label = 'Show 303d proximity buffer', value = FALSE),
            checkboxInput(inputId = 'show.excluded.points', label = 'Show excluded points', value = FALSE),
            checkboxInput(inputId = 'show.parameters', label = 'Show parameters included in WQI score for each facility', value = FALSE),
            hr(style="border: 1px solid darkgrey"),
            # Describe the WQI Calculations:
            tags$b(h4('Water Quality Index (WQI):')),
            p('Based on the San Diego Coastkeeper\'s WQI, this is an adapted version of the official Canadian WQI, which was adoped by
              the United Nations Environment Program Global Environmental Monitoring System in 2007 for evaluating global water quality. The WQI 
              score for an individual site is based on the number of tests exceeding basin plan water quality thresholds, and the magnitude 
              of those exceedances, as follows:'),
            # h5('Frequency:'),
            tags$li('Frequency:'),
            tags$ul(helpText('\\(F1=\\frac{\\text{Number of Samples Exceeding Standard}}{\\text{Total Number of Samples}}\\times{100}\\)')),
            # h5('Magnitude:'),
            tags$li('Magnitude:'),
            tags$ul(helpText('\\(Excursion_i=\\frac{\\text{Value of Sample Exceeding Standard}_i}{\\text{Standard Value}}-1\\)')),
            tags$ul(helpText('\\(NSE=\\frac{\\sum{Excursion}}{\\text{Total Number of Samples}}\\)')),
            tags$ul(helpText('\\(F2=\\frac{NSE}{0.01(NSE)+0.01}\\)')),
            # h5('WQI:'),
            tags$li('WQI:'),
            # tags$ul(helpText('\\(\\text{WQI=}100-\\frac{\\sqrt{F1^2+F2^2}}{1.4142}\\)')),
            tags$ul(helpText('\\(WQI=100-\\frac{\\sqrt{F1^2+F2^2}}{1.412}\\)')),
            hr(style="border: 1px solid darkgrey"),
            # Link to the code, etc...
            # p('For more information, contact: ', a(href = 'mailto:david.altare@waterboards.ca.gov', 'david.altare@waterboards.ca.gov')),
            tags$b(h4('Application Information:')),
            actionButton(inputId = 'github', label = 'Code on GitHub', icon = icon('github', class = 'fa-1x'),
                         onclick ="window.open('https://github.com/daltare/Stormwater_Enforcement_Tool')")
            ),
        
        # Show map and data table
        mainPanel(
            tags$head(tags$style(".buttonstyle{background-color:#f2f2f2;} .buttonstyle{color: black;}")), # define button style (background color and font color)
            leaflet::leafletOutput('monitoring.map',height = 450),
            hr(),
            DT::dataTableOutput('WQI.table'),
            hr(),
            h4('Enter / Edit Standards:'),
            rHandsontableOutput("hot"),
            br(),
            actionButton("reset", "Reset Standards",class = 'buttonstyle'),
            hr(),
            h4('Download Additional Data:'),
            downloadButton('downloadRawData', 'Sampling Data Used in WQI Calculations', class = "buttonstyle"), HTML('&emsp;'), # br(), br(),
            downloadButton('downloadStandards', 'Standards Used in WQI Calculations', class = "buttonstyle"), HTML('&emsp;'), # br(), br(),
            downloadButton('downloadFacilities', 'All Facility Information', class = "buttonstyle"),
            br(),br()
        )
        )
    )






# Define server logic required to draw map -------------------------------------
server <- function(input, output, session) {
    
    # 1. Get the standards---------------------------------------------------------------------------------------------------------------------------------------------------------
        # User Entered Standards ---------------------------------------------------------
            values <- reactiveValues()
            observe({
                DF <- standards %>% dplyr::select(-Standard.water.type) %>%  tidyr::spread(Standard.Type, Standard) %>% dplyr::mutate(Custom = '')
                DF$Custom <- as.numeric(DF$Custom)
                
                if (!is.null(input$hot)) {
                    DF = hot_to_r(input$hot)
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
            tf <- !is.na(standards.applied %>% select(input$standard))
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
            # g <- ggplot() + geom_sf(data = ces_poly_study_area, aes(fill = Poll_pctl))

    # 3. Then filter the 303d lines and polygons for the selected region ----------------------------------------------------------------------------------------------------------------------
        # first filter using the attributes in the 303d shapefile
            impaired_lines_region <- impaired_303d_lines %>% dplyr::filter(REGION_NUM == substr(x = input$region.selected, start = 1, stop = 1))
            impaired_polygons_region <- impaired_303d_polygons %>% dplyr::filter(REGION_NUM == substr(x = input$region.selected, start = 1, stop = 1))
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
            if (data.type == 'SMARTS') {
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
            
    # 4. Calculate the WQI scores --------------------------------------------------------------------------------------------------------------------------------------------------
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
                    if (data.type == 'SMARTS') {
                        facilities <- facilities %>% dplyr::rename(STATUS_CODE_NAME = STATUS, REGION = REGION_BOARD) 
                    }
                # Join the facilities info to the WQI scores, by WDID   
                    WQI.Scores <- WQI.Scores %>% dplyr::left_join(facilities %>% dplyr::select(WDID, STATUS_CODE_NAME, REGION, FACILITY_NAME, FACILITY_ADDRESS, FACILITY_CITY, FACILITY_STATE, FACILITY_ZIP, FACILITY_COUNTY, RECEIVING_WATER_NAME, PRIMARY_SIC, FACILITY_LATITUDE, FACILITY_LONGITUDE), by = 'WDID')
                # rename the lat/lon fields, to make mapping easier
                    WQI.Scores <- WQI.Scores %>% dplyr::rename(Latitude = FACILITY_LATITUDE, Longitude = FACILITY_LONGITUDE)
    # 5. Get a list of parameters in the WQI score
        if (input$show.parameters == TRUE) {
            # Create a column of data that lists the different parameters that were included in the WQI calculations for each WDID and monitoring period
            # Create a df of distinct combinations of WDIDs, Monitoring Periods, and Parameters
            grouped_mon_data <- monitoring.data.WQI %>% group_by(WDID, Parameter, Monitoring.Period) %>% select(WDID, Parameter, Monitoring.Period) %>% distinct()
            # Create a df of distinct WDIDs and Monitoring Periods
            distinct_WDIDs_MonPeriods <- as.data.frame(grouped_mon_data) %>% select(WDID, Monitoring.Period) %>% distinct()
            # To the df of distinct WDIDs and Monitoring Periods, append a character string of the Parameters for each combination of WDID and Monitoring Period
            for (i in 1:nrow(distinct_WDIDs_MonPeriods)) {
                temp_params <- grouped_mon_data %>% filter(WDID == distinct_WDIDs_MonPeriods$WDID[i] & Monitoring.Period == distinct_WDIDs_MonPeriods$Monitoring.Period[i]) # df filtered for given WDID and Monitoring Period
                temp_params <- paste0(temp_params$Parameter, collapse = ', ') # character string of parameters in the above df
                distinct_WDIDs_MonPeriods$Parameters.In.Score[i] <- temp_params # append the parameters string to the df of distinct WDIDs and Monitoring Periods
            }
            # join the list of parameters to the WQI.Scores df
            WQI.Scores <- WQI.Scores %>% dplyr::left_join(distinct_WDIDs_MonPeriods, by = c('WDID', 'Monitoring.Period'))
        } # else {WQI.Scores$Parameters.In.Score = ''}
        
    # 6. Create a sf object from the WQI Scores data
        WQI.Scores_sf <- sf::st_as_sf(WQI.Scores[!is.na(WQI.Scores$Latitude),], coords = c('Longitude', 'Latitude'), crs = 4326, agr = 'constant')
        
        
    # 7. Apply filters and create the map ----------------------------------------------------------------------------------------------------------------------------------
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
# !!!!!!!!!!! ERROR HERE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            ces.percentile.numeric <- tidyr::separate(as.data.frame(ces_poly_study_area), Percentile, sep = '-', into = c('Percentile', 'Perc2'), convert = TRUE)[,2]+1.5
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
                ces_poly_study_area_filtered <- ces_poly_study_area %>% dplyr::filter(fill.variable >= input$ces.score.range[1] & fill.variable <= input$ces.score.range[2])
        
        # create a list of WDIDs in the polygons that meet the selected criteria
            # points_ces_filter <- st_intersects(ces_poly_study_area_filtered, WQI.Scores_sf, sparse = TRUE) # lists the monitoring point(s) in each polygon 
            points_ces_filter <- sf::st_intersects(WQI.Scores_sf, ces_poly_study_area_filtered, sparse = TRUE) # lists the polygon number for each monitoring point; if no polygon number, the point is not in a polygon that meets the criteria
            tf_points <- as.logical(sapply(points_ces_filter, length)) # gives a logical vector where TRUE means the monitoring point meets the criteria (sapply and length argument needed because there may be more than 1 polygon in some cases)
            WDIDs_ces_filter <- (as.data.frame(WQI.Scores_sf)[tf_points,] %>% dplyr::select(WDID))[,1] # list of WDIDs in the polygons that satisfy the criteria
        
        # filter for points within the selected maximum distance of 303d waterbodies
        # check to see if a valid number is entered
        if (!is.na(as.numeric(input$dist.to.303))) {
            proximity_ft <- units::as_units(as.numeric(input$dist.to.303), ft)
            proximity_meters <- units::set_units(proximity_ft, m)
            # have to convert points and lines to Cartesian coordinate system (x,y rather than lat/lon) to do the distance check
            WQI.Scores_sf_mercator <- sf::st_transform(WQI.Scores_sf, 3857) 
            impaired_lines_study_area_mercator <- sf::st_transform(impaired_lines_study_area, 3857)
            # check for points within the given distance (returns a sparse matrix - i.e., a list the same length as the number of WQI points, that lists the stream segment(s), if any, that meet the criteria for each point)
            WQI_303d_dist_check <- sf::st_is_within_distance(WQI.Scores_sf_mercator, impaired_lines_study_area_mercator, dist = proximity_meters)
            tf_points_303_distance <- as.logical(sapply(WQI_303d_dist_check, length)) # gives a logical vector where TRUE means the monitoring point has at least one 303d stream segment that meets the proximity criteria (sapply and length argument needed because there may be more than 1 polygon in some cases)
            # create a variable with the points that meet the criteria
            WQI_303d_dist_points <- WQI.Scores_sf[tf_points_303_distance,] # points that satisfy the distance to 303d waters criteria
            WQI_303d_dist_points_list <- as.data.frame(WQI_303d_dist_points %>% dplyr::select('WDID'))[,1]
            # dataset of excluded points
            WQI_303d_excluded_points <- WQI.Scores_sf[!tf_points_303_distance,] # points that don't satisfy the distance to 303d waters criteria
            # create a variable with polygons representing the buffered region around the streams
            buffered_streams <- sf::st_buffer(x = impaired_lines_study_area_mercator, dist = proximity_meters)
            buffered_streams_geographic <- sf::st_transform(buffered_streams, 4326)
            # for testing only - this creates a map of the points (red = meets proximity criteria, black = doesn't meet proximity criteria) + streams + buffers
            #     g <- ggplot() + geom_sf(data = buffered_streams) + geom_sf(data = WQI.Scores_sf[!tf_points_303_distance,], color = 'black', aes(color = 'black')) + geom_sf(data = WQI_303d_dist_points, color = 'red', aes(color = 'red')) + geom_sf(data = impaired_lines_study_area)
            
        }
        
        
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
                                                 
                                                 group = '2012 303d Listed Waters')
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
                                                group = '2012 303d Listed Waters')
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
                if (input$show.303d.buffer == TRUE & !is.na(as.numeric(input$dist.to.303)) & input$show.excluded.points == TRUE) {
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
                if (input$show.parameters == TRUE) {
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
                }
                if (input$show.parameters == FALSE) {
                    l <- l %>% leaflet::addCircleMarkers(radius = 4,
                                                         stroke = TRUE, weight = 0.5, color = 'black', opacity = 1,
                                                         fill = TRUE, fillOpacity = 1, fillColor = ~wqi.leaflet.pal(WQI),
                                                         # clusterOptions = leaflet::markerClusterOptions(spiderfyDistanceMultiplier = 2),# freezeAtZoom = 13, maxClusterRadius = 10),#,#singleMarkerMode = TRUE),
                                                         popup = ~paste0('<b>', '<u>', 'Discharger WQI Score', '</u>', '</b>','<br/>',
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
                                                         group = 'WQI Scores')
                }
            # add the legend
                l <- l %>% leaflet::addLegend(position = 'bottomright', colors = 'blue', opacity = 1.0, labels = '2012 303d Listed Waterbodies', layerId = '303d.list')
                l <- l %>% leaflet::addLegend(position = 'bottomright', pal = ces.leaflet.pal, values = ces_poly_study_area$fill.variable, title = paste0('CES: ', input$ces.parameter), opacity = 1, layerId = 'ces.legend', bins = 4)
                l <- l %>% leaflet::addLegend(position = "bottomright", pal = wqi.leaflet.pal, values = WQI.Scores$WQI, title = "WQI", opacity = 1, layerId = 'wqi.legend', bins = 2)
            # Add controls to select the basemap
                l <- l %>% leaflet::addLayersControl(baseGroups = basemap.options,
                                                     overlayGroups = c('WQI Scores', 'CES Polygons', '303d Listed Waters', 'Regional Board Boundary', '303d Buffers', 'Excluded Points'),
                                                     options = leaflet::layersControlOptions(collapsed = TRUE))
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
                # filtered.data <- filtered.data %>% dplyr::group_by(WDID, Latitude, Longitude) %>% distinct(WDID)
            # fit to RB boundary - get RB bounds
                rb.boundary.selected <- sf::st_as_sf(RB_Boundaries %>% dplyr::filter(RB_OFF == input$region.selected))
                bounds <- attributes(sf::st_geometry(rb.boundary.selected))$bbox
            # update map to appropriate bounds
                leaflet::leafletProxy(mapId = 'monitoring.map') %>% 
                # leaflet::fitBounds(lng1 = min(filtered.data$Longitude, na.rm = TRUE), lat1 = min(filtered.data$Latitude, na.rm = TRUE), lng2 = max(filtered.data$Longitude, na.rm = TRUE), lat2 = max(filtered.data$Latitude, na.rm = TRUE))
                leaflet::fitBounds(lng1 = bounds[[1]], lat1 = bounds[[2]], lng2 = bounds[[3]], lat2 = bounds[[4]])
        })
    
    # Reset Standards
    observeEvent(input$reset, {
        DF <- standards %>% select(-Standard.water.type) %>%  tidyr::spread(Standard.Type, Standard) %>% dplyr::mutate(Custom = '')
        DF$Custom <- as.numeric(DF$Custom)
        values[["DF"]] <- DF
    })   
    
    output$hot <- renderRHandsontable({
        DF <- values[["DF"]]
        if (!is.null(DF))
            rhandsontable(DF, useTypes = TRUE, stretchH = "all")
    })    
    
    
    }

# Run the application 
shinyApp(ui = ui, server = server)
