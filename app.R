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
# get data from extracted tables ------------------------------------------------------------------------------------------------------------------
    # standards
        standards <- readr::read_tsv('data/Standards.txt')
        names(standards) <- make.names(names(standards))
    # monitoring data
        monitoring.data <- readr::read_tsv('data/SMARTS_monitoring_data.txt')
        names(monitoring.data) <- make.names(names(monitoring.data))
        monitoring.data <- tidyr::separate(data = monitoring.data, col = Date.time.of.sample.collection, into = c('Date.of.sample.collection','Time.of.sample.collection'), sep = ' ')
        monitoring.data <- monitoring.data %>% dplyr::mutate(Date.of.sample.collection = lubridate::mdy(Date.of.sample.collection))
    # facilities
        facilities <- readr::read_tsv('data/NOIs.txt')
        names(facilities) <- make.names(names(facilities))
    # receiving waters
        receiving.waters <- readr::read_tsv('data/Receiving_Waters.txt')
        names(receiving.waters) <- make.names(names(receiving.waters))

# Data transformations (monitoring data) --------------------------------------------------------------------------------------------------------
    # drop unreasonable dates !!!!! NOTE: MAY WANT TO REVISIT THESE
        monitoring.data <- monitoring.data %>% dplyr::filter(lubridate::year(Date.of.sample.collection) <= lubridate::year(Sys.Date()) & 
                                                                 lubridate::year(Date.of.sample.collection)>=2010)
        
    # Create a column for monitoring period
        monitoring.data <- monitoring.data %>% dplyr::mutate(Monitoring.Period = dplyr::if_else(
            lubridate::month(Date.of.sample.collection) >= 7, 
            paste0(lubridate::year(Date.of.sample.collection), ' - ', lubridate::year(Date.of.sample.collection) + 1), 
            paste0(lubridate::year(Date.of.sample.collection)-1, ' - ', lubridate::year(Date.of.sample.collection))
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
        
# Data transformations (standards) --------------------------------------------------------------------------------------------------------------
    # change water type column name
        standards <- standards %>% dplyr::rename(Standard.water.type = Water.type)

        
# CalEnviroScreen Polygons
        # These steps show how to access and transform the CES geospatial data, but they only need to be done once:
            # temp_zip <- tempfile()
            # ces_url <- 'https://oehha.ca.gov/media/downloads//ces3shp.zip'  # ALTERNATVIE: ces_url <- 'https://data.ca.gov/sites/default/files/CES3Results_SHP.zip'
            # download.file(url = ces_url, destfile = temp_zip, method = 'curl')
            # unzip(zipfile = temp_zip, exdir = 'data/CES3Results') #files = c('CES3Results.shp','CES3Results.shx','CES3Results.prj'),
            # unlink(temp_zip)
            # ces <- sf::st_read('data/CES3Results/CES3Results.shp')
            # ces_transform <- st_transform(ces, 4326)
            # ces_simple <- rmapshaper::ms_simplify(ces_transform)
            # saveRDS(object = ces_simple, file = 'data/simplified_CalEnvironScreen_poly.RDS')
        # Read the data from the saved RDS file
            ces_poly <- read_rds('data/simplified_CalEnvironScreen_poly.RDS')
            
        # create a polygon that bounds the monitoring data, then create sf variable with the CES polygons in the study 
            lat_min <- min(monitoring.data$Latitude, na.rm = TRUE)
            lat_max <- max(monitoring.data$Latitude, na.rm = TRUE)
            lon_min <- min(monitoring.data$Longitude, na.rm = TRUE)
            lon_max <- max(monitoring.data$Longitude, na.rm = TRUE)
            poly_bounds <- st_polygon(list(rbind(c(lon_min,lat_max),
                                                 c(lon_max,lat_max),
                                                 c(lon_max,lat_min),
                                                 c(lon_min,lat_min),
                                                 c(lon_min,lat_max))))
            # tf_poly_bounds <- st_within(ces_poly, poly_bounds, sparse = FALSE)
            tf_poly_bounds <- st_intersects(ces_poly, poly_bounds, sparse = FALSE)
            
            ces_poly_study_area <- ces_poly[tf_poly_bounds,]
            # plot
                # g <- ggplot() + geom_sf(data = ces_poly_study_area, aes(fill = Poll_pctl))
            
            
            
        # create a sf variable for the monitoring data
            monitoring_sf <- sf::st_as_sf(monitoring.data[!is.na(monitoring.data$Latitude),], coords = c('Longitude', 'Latitude'), crs = 4326, agr = 'constant')
                
        # make a sf variable with unique monitoring points (1 per WDID in monitoring data)
            unique_points <- monitoring_sf %>% distinct(WDID, .keep_all = TRUE)                
        
        # filter for CES polygons that contain monitoring points
            ces_monitoring_points <- st_within(unique_points, ces_poly, sparse = TRUE) # lists the polygon number for each monitoring point
            tf_ces_monitoring_points <- (1:nrow(ces_poly)) %in% ces_monitoring_points # checks to see if each polygon is in the list created above
            ces_poly_monitoring_points <- ces_poly[tf_ces_monitoring_points,] # filters for the polygons in the list
        
        # filter for CES polygons with high pollution load
            ces_highPol <- ces_poly %>% filter(Poll_pctl >= 80)                
            ces_study_area_highPol <- ces_poly_study_area %>% filter(Poll_pctl >= 80)
        
        # get the monitoring points in the high pollution polygons
            points_within_hipoll_polygon <- st_within(unique_points, ces_highPol, sparse = TRUE) # lists the polygon number for each monitoring point
            tf_points <- !is.na(as.logical(points_within_hipoll_polygon)) # creates a true/false list of the same lenth as the number of monitoring points (true = point in selected polygons)
            
        # map only polygons in the study area, along with the points
            g <- ggplot() + geom_sf(data = ces_poly_study_area)
            g <- g + geom_sf(data = unique_points)
            
        # map high pollution polygons in the study area, and monitoring points in those polygons
            g1 <- ggplot() + geom_sf(data = ces_study_area_highPol) + geom_sf(data = unique_points[tf_points,])
            
        # map high pollution polygons in the study area, and all monitoring points
            g2 <- ggplot() + geom_sf(data = ces_study_area_highPol) + geom_sf(data = unique_points)
            
        # map all high pollution polygons
            g <- ggplot() + geom_sf(data = ces_highPol)

            
        # CES parameter choices
            ces_choices <- data.frame(Name = c('CES Percentile', 'Pollution Burden', 'Impaired Water Bodies'), CES.Variable = c('Percentile', 'Poll_pctl', 'IWB_pctl'), stringsAsFactors = FALSE) #' CES Percentile' = 'Percentile'

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
                    selectInput(inputId = 'standard',label = 'Select Standard:', choices = c('CTR', 'MSGP - Benchmark', 'NAL', 'Custom')),
                    selectInput(inputId = 'monitoring.period', label = 'Select Monitoring Period:', choices = periods.list, selected = '2016 - 2017'),
                    # htmlOutput('monitoring.period.selector'),
                    selectInput(inputId = 'WDID.selected', label = 'Select Facility WDIDs (Optional):', choices = WDID.list, multiple = TRUE, selected = WDID.list[1]),
                    sliderInput(inputId = 'score.range', label = 'Select WQI Score Range:', min = 0, max = 100, value = c(0,100)),
                    # actionButton('refresh','Update')
                    selectInput(inputId = 'ces.parameter', 'Select a CES Parameter:', choices = ces_choices$Name, selected = 'Pollution Burden'),
                    sliderInput(inputId = 'ces.score.range', label = 'Filter by Score of Selected CES Parameter:', min = 0, max = 100, value = c(0,100)),
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
        
    ## User Entered Standards --------------------------------------------------
        values <- reactiveValues()
        observe({
            DF <- standards %>% select(-Standard.water.type) %>%  tidyr::spread(Standard.Type, Standard) %>% dplyr::mutate(Custom = '')
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
            
            # Get the standards to apply in calculations
                standards.applied <- as.data.frame(DF) %>% select(Parameter, input$standard)
                tf <- !is.na(standards.applied %>% select(input$standard))
                standards.applied <- standards.applied[tf,]
                names(standards.applied)[2] <- 'Standard'
                standards.applied <- standards.applied %>% dplyr::mutate(Standard.Type = input$standard)
            
            # Standards Data Download --------------------------------------------------
                output$downloadStandards <- downloadHandler(
                    filename = 'WQI_Standards.csv',
                    content = function(con) {
                        write.csv(as.data.frame(DF), con, row.names = FALSE)
                    },
                    contentType = 'text/csv'
                )
        # }) 
    
    
   # observe({     
       
       # STANDARDS (create a new table with relevant standards)
            # standards.applied <- standards %>% dplyr::filter(Standard.Type == input$standard)
      
        # MONITORING DATA
            # Filter for the relevant parameters, and compare the results to standards
                monitoring.data.WQI <- monitoring.data %>% dplyr::filter(Parameter %in% standards.applied$Parameter)
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
                
                # add a calculated column for the Lat/Lng - It looks like monitoring location lat/lng could be more accurate, but some are missing
                # after trying this, it looks like monitoring location lat/lng also has some errors, and it's hard to distinguish without looking on a site-by-site basis
                    # monitoring.data.WQI <- monitoring.data.WQI %>% dplyr::mutate(Map.Latitude = dplyr::if_else(!is.na(Monitoring.Location.Latitude), Monitoring.Location.Latitude, Latitude),
                    #                                                              Map.Longitude = dplyr::if_else(!is.na(Monitoring.Location.Longitude), Monitoring.Location.Longitude, Longitude))
    
        # Calculate the WQI Scores 
            # Calculate Exceedance (for F1) and Excursion (for F2)
                WQI.Scores <- monitoring.data.WQI %>% dplyr::group_by(WDID, Monitoring.Period, Latitude, Longitude, Standard.Type) %>% dplyr::summarize(Exceedances = sum(Exceedance), Sum.Excursion = sum(Excursion), Total.Samples = n()) # Total.Samples(Exceedance))
            # Calculate F1 (Exceedance)
                WQI.Scores <- WQI.Scores %>% dplyr::mutate(F1 = Exceedances / Total.Samples * 100)
            # Calculate F2 (Excursion)
                WQI.Scores <- WQI.Scores %>% dplyr::mutate(NSE = Sum.Excursion / Total.Samples)
                WQI.Scores <- WQI.Scores %>% dplyr::mutate(F2 = NSE / (0.01 * NSE + 0.01))
            # Final WQI Scores (!!!! NOTE: THE DATABASE USES 1.412, BUT THE POWERPOINT PRESENTATION USES 1.4142 - USED 1.412 HERE TO REPLICATE DATABASE TOOL RESULTS !!!)
                WQI.Scores <- WQI.Scores %>% dplyr::mutate(WQI = round((100 - sqrt(F1^2 + F2^2) / 1.412),1))
                # WQI.Scores <-  WQI.Scores %>% dplyr::mutate(Frequency = round(F1,0))
                # WQI.Scores <-  WQI.Scores %>% dplyr::mutate(Magnitude = round(F2,0))
                WQI.Scores <- WQI.Scores %>% dplyr::left_join(facilities %>% dplyr::select(WDID, STATUS_CODE_NAME, REGION, FACILITY_NAME, FACILITY_ADDRESS, FACILITY_CITY, FACILITY_STATE, FACILITY_ZIP, FACILITY_COUNTY, RECEIVING_WATER_NAME, PRIMARY_SIC), by = 'WDID')

            # Create a sf object from the WQI Scores data
                WQI.Scores_sf <- sf::st_as_sf(WQI.Scores[!is.na(WQI.Scores$Latitude),], coords = c('Longitude', 'Latitude'), crs = 4326, agr = 'constant')
                
   
    # Map ------------------------------------------------------------------
        # Create the color palette for the WQI scores
            wqi.leaflet.pal <- leaflet::colorNumeric(
                palette = colorRamp(c('olivedrab2', 'red3'), interpolate='spline'),
                domain = WQI.Scores$WQI,
                reverse = TRUE
            )
                
        # Create the color palette for the CES polygons
            param <- ces_choices[ces_choices$Name == input$ces.parameter,2]
            # create a numeric variable for the CES percentile that is in the middle of the values of the given 5% range
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
                ces.pal.color <- 'Blues' #'YlOrBr'
                # if (param == 'Percentile') {
                #     ces.leaflet.pal <- leaflet::colorFactor(
                #         palette = ces.pal.color,
                #         domain = ces_poly_study_area$Percentile
                #     )
                # } else {
                    ces.leaflet.pal <- leaflet::colorNumeric(
                        palette = ces.pal.color,
                        domain = ces_poly_study_area$fill.variable
                    )
                # }
                    
            # for filtering, find the polygons that meet the selected criteria (range of values)
                ces_poly_study_area_filtered <- ces_poly_study_area %>% dplyr::filter(fill.variable >= input$ces.score.range[1] & fill.variable <= input$ces.score.range[2])
            # create a list of WDIDs in the polygons that meet the selected criteria
                # points_ces_filter <- st_intersects(ces_poly_study_area_filtered, WQI.Scores_sf, sparse = TRUE) # lists the monitoring point(s) in each polygon 
                points_ces_filter <- st_intersects(WQI.Scores_sf, ces_poly_study_area_filtered, sparse = TRUE) # lists the polygon number for each monitoring point; if no polygon number, the point is not in a polygon that meets the criteria
                tf_points <- as.logical(sapply(points_ces_filter, length)) # gives a logical vector where TRUE means the monitoring point meets the criteria (sapply and length argument needed because there may be more than 1 polygon in some cases)
                WDIDs_ces_filter <- (as.data.frame(WQI.Scores_sf)[tf_points,] %>% select(WDID))[,1] # list of WDIDs in the polygons that satisfy the criteria
                
        # Create the mapping data (filter for the selected monitoring period and WQI range, and for WDIDs if selected)
            map.data <- as.data.frame(WQI.Scores %>% dplyr::filter(Monitoring.Period == input$monitoring.period & WQI >= input$score.range[1] & WQI <= input$score.range[2] & WDID %in% WDIDs_ces_filter))
            map.data <- tryCatch(expr = if(input$WDID.selected != 'All WDIDs') {map.data <- map.data %>% dplyr::filter(WDID %in% input$WDID.selected)} else {map.data}, error = function(e) {map.data})
            shared.map.data <- crosstalk::SharedData$new(map.data)
            
        # Create the map
            output$monitoring.map <- leaflet::renderLeaflet({
                # create the empty map
                    l <- leaflet::leaflet(shared.map.data)
                    
                # enter the basemap options to allow the user to select
                    basemap.options <- c('Esri.WorldStreetMap', 'Esri.WorldTopoMap', 'Esri.WorldImagery',
                                         'Esri.WorldGrayCanvas', 'CartoDB.Positron') #'OpenStreetMap', 'OpenStreetMap.BlackAndWhite', 'Thunderforest.SpinalMap'
                    
                # add the basemaps listed above to the map (for options, see: http://leaflet-extras.github.io/leaflet-providers/preview/)
                    for (provider in basemap.options) {
                        l <- l %>% addProviderTiles(provider, group = provider)
                    }
                # add the min-map window
                    l <- l %>% addMiniMap(tiles = basemap.options[[1]], toggleDisplay = TRUE, position = "bottomleft")
                
                # code to make the basemap/min-map selector work (copied from: https://rstudio.github.io/leaflet/morefeatures.html)
                    l <- l %>% htmlwidgets::onRender("
                                      function(el, x) {
                                      var myMap = this;
                                      myMap.on('baselayerchange',
                                      function (e) {
                                      myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
                                      })
                                      }")
                
                # Set the bounds of the map dynamically - initial view is based on the full extent, after that the map is based on the most recent bounds when a new option (standard, period, etc) is selected
                    isolate(if (is.null(input$monitoring.map_bounds)) {
                        l <- l %>% leaflet::fitBounds(lng1 = min(monitoring.data$Longitude, na.rm = TRUE), lat1 = min(monitoring.data$Latitude, na.rm = TRUE), lng2 = max(monitoring.data$Longitude, na.rm = TRUE), lat2 = max(monitoring.data$Latitude, na.rm = TRUE))
                    } else {
                            l <- l %>% leaflet::setView(lng = mean(c(input$monitoring.map_bounds$west, input$monitoring.map_bounds$east)), lat = mean(c(input$monitoring.map_bounds$north, input$monitoring.map_bounds$south)), zoom = input$monitoring.map_zoom)                                
                        })
                
                # create a button to re-center the map
                    l <- l %>% leaflet::addEasyButton(leaflet::easyButton(
                        icon="fa-globe", title="Center Map",
                        onClick=JS(paste0('function(btn, map){ map.fitBounds([[',
                                          min(map.data$Latitude, na.rm = TRUE), ', ',
                                          min(map.data$Longitude, na.rm = TRUE), '],[',
                                          max(map.data$Latitude, na.rm = TRUE), ', ',
                                          max(map.data$Longitude, na.rm = TRUE), ']]); }'))))

                
                # Add the CalEnvironScreen Polygons
                    l <- l %>% leaflet::addPolygons(data = ces_poly_study_area, color = "#444444", weight = 1, smoothFactor = 0.5,
                                                    opacity = 1.0, fillOpacity = 0.5,
                                                    # fillColor = ~colorNumeric('YlOrBr', Poll_pctl)(Poll_pctl), # view RColorBrewer palettes with: RColorBrewer::display.brewer.all()
                                                    fillColor = ~ces.leaflet.pal(fill.variable),
                                                    highlightOptions = highlightOptions(color = "white", weight = 2),#,bringToFront = TRUE
                                                    popup = ~paste0('<b>', 'Tract: ', '</b>', Tract_1,'<br/>',
                                                                    '<b>', 'Location: ', '</b>', City,', ', County, ' ', ZIP,'<br/>',
                                                                    '<b>', 'Population: ', '</b>', Population,'<br/>','<br/>',
                                                                    '<b>','<u>', 'Scores: ', '</b>','</u>','<br/>',
                                                                    '<b>', 'CES Percentile: ', '</b>', Percentile,'<br/>',
                                                                    '<b>', 'Impaired Waterbodies Percentile: ', '</b>', IWB_pctl,'<br/>',
                                                                    '<b>', 'Pollution Burden Percentile: ', '</b>', Poll_pctl,'<br/>',
                                                                    '<b>', 'Pesticides Percentile: ', '</b>', Pest_pctl,'<br/>',
                                                                    '<b>', 'Toxic Releases Percentile: ', '</b>', TR_pctl,'<br/>',
                                                                    '<b>', 'Cleanups Percentile: ', '</b>', Clean_Pctl,'<br/>',
                                                                    '<b>', 'Groundwater Threats Percentile: ', '</b>', GW_pctl,'<br/>',
                                                                    '<b>', 'Hazardous Waste Generators Percentile: ', '</b>', Haz_pctl),
                                                    group = 'CES Polygons')#,'<br/>'
                    
                    
                # add in the selected WQI data
                    l <- l %>% leaflet::addCircleMarkers(
                        stroke = TRUE,
                        fill = TRUE,
                        group = 'WQI Scores',
                        radius = 2,
                        opacity = 1,
                        # clusterOptions = leaflet::markerClusterOptions(spiderfyDistanceMultiplier = 2),# freezeAtZoom = 13, maxClusterRadius = 10),#,#singleMarkerMode = TRUE),
                        color = ~wqi.leaflet.pal(WQI),
                        popup = ~paste0('<b>', '<u>','Facility Information','</u>','</b>','<br/>',
                                        '<b>', 'WDID: ', '</b>', WDID,'<br/>',
                                        '<b>', 'Facility Name: ', '</b>', FACILITY_NAME,'<br/>',
                                        '<b>', 'SIC: ', '</b>', PRIMARY_SIC,'<br/>',
                                        '<b>', 'Address: ', '</b>', FACILITY_ADDRESS, '<br/>',
                                        '<b>', 'City: ', '</b>', FACILITY_CITY, '<br/>',
                                        '<b>', 'Receiving Water: ', '</b>', RECEIVING_WATER_NAME,'<br/>',
                                        '<br/>',
                                        '<b>','<u>', 'Scoring','</u>','</b>','<br/>',
                                        '<b>', 'Monitoring Period: ', '</b>', Monitoring.Period,'<br/>',
                                        '<b>', 'Standard: ', '</b>', Standard.Type,'<br/>',
                                        '<b>', 'Exceedence Frequency: ', '</b>', round(F1,0),'<br/>',
                                        '<b>', 'Exceedence Magnitude: ', '</b>', round(F2,0),'<br/>',
                                        '<b>', 'WQI: ', '</b>', WQI, '<br/>'))
                    
                # add the legend
                    l <- l %>% leaflet::addLegend(position = 'bottomright', pal = ces.leaflet.pal, values = ces_poly_study_area$fill.variable, title = input$ces.parameter, opacity = 1, layerId = 'ces.legend', bins = 6)
                    l <- l %>% leaflet::addLegend(position = "bottomright", pal = wqi.leaflet.pal, values = WQI.Scores$WQI, title = "WQI", opacity = 1, layerId = 'wqi.legend')
                
                # Add controls to select the basemap
                    l <- l %>% leaflet::addLayersControl(baseGroups = basemap.options,
                                                         overlayGroups = c('WQI Scores', 'CES Polygons'),
                                                         options = layersControlOptions(collapsed = TRUE))
                    
                
                    
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
