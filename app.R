library(shiny)
library(DT)
library(crosstalk)
library(leaflet)

`%>%` <- magrittr::`%>%`

# get data from the database --------------------------------------------------------------------------------------------------------------------
    # NOTE: HAVE TO SET THE R VERSION TO 32 BIT FIRST (go to: Tools -> Global Options -> General -> R version)
    # Set up the connection
        dbPath <- 'data/Industrial_Stormwaterv24.accdb'
        # dbPath <- 'C:/David/Stormwater/Enforcement_AnalyticalTool/Industrial_Stormwaterv24.accdb'
        con <- DBI::dbConnect(drv = odbc::odbc(), .connection_string = paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)}; Dbq=", dbPath))
    # Read the data
        standards <- tibble::as_tibble(DBI::dbReadTable(conn = con, name = 'Standards'))
        monitoring.data <- tibble::as_tibble(DBI::dbReadTable(conn = con, name = 'SMARTS monitoring data'))
        facilities <- tibble::as_tibble(DBI::dbReadTable(conn = con, name = 'NOIs'))
        receiving.waters <- tibble::as_tibble(DBI::dbReadTable(conn = con, name = 'Recieving Waters'))
    # Close the connection
        DBI::dbDisconnect(con)

# Data transformations (monitoring data) --------------------------------------------------------------------------------------------------------
    # Create a column for monitoring period
        monitoring.data <- monitoring.data %>% dplyr::mutate(Monitoring.Period = dplyr::if_else(
            lubridate::month(Date.time.of.sample.collection) >= 7, 
            paste0(lubridate::year(Date.time.of.sample.collection), ' - ', lubridate::year(Date.time.of.sample.collection) + 1), 
            paste0(lubridate::year(Date.time.of.sample.collection)-1, ' - ', lubridate::year(Date.time.of.sample.collection))
        ))    
        
    # Create a column with ug/L Results converted to mg/L
        monitoring.data <- monitoring.data %>% dplyr::mutate(Result.Conv = dplyr::if_else(Units=='ug/L', Result / 1000, Result))
        monitoring.data <-  monitoring.data %>% dplyr::mutate(Unit.Conv = dplyr::if_else(Units=='ug/L', 'mg/L', Units))
        
# Data transformations (standards) --------------------------------------------------------------------------------------------------------------
    # change water type column name
        standards <- standards %>% dplyr::rename(Standard.water.type = Water.type)

        
        
# ----------------------------------------------------------------------------------------------------------------------------------------------- #       
# ----------------------------------------------------------------------------------------------------------------------------------------------- #
# Define UI --------------------------------------------------------------------
    ui <- fluidPage(
        # Application title
        titlePanel("Stormwater Enforcement Tool"),
            
        sidebarLayout(
            # Sidebar with inputs     
            sidebarPanel(
                withMathJax(),
                # h3('Filters:'),
                selectInput(inputId = 'standard',label = 'Select Standard:', choices = c('CTR', 'MSGP - Benchmark', 'NAL')),
                selectInput(inputId = 'monitoring.period', label = 'Select Monitoring Period:', choices = c('2016 - 2017', '2015 - 2016')),
                selectInput(inputId = 'WDID', label = 'Select a Facility WDID (Optional):', choices = c('All', '9 37I005157')),
                sliderInput(inputId = 'score.range', label = 'Select WQI Score Range:', min = 0, max = 100, value = c(0,100)),
                # actionButton('refresh','Update')
                hr(style="border: 1px solid darkgrey"),
                tags$b(h4('Water Quality Index (WQI):')),
                p('This is based on the San Diego Coastkeeper\'s WQI, an adapted version of the official Canadian WQI (CWQI), which was adoped by
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
                tags$ul(helpText('\\(WQI=100-\\frac{\\sqrt{F1^2+F2^2}}{1.4142}\\)')),
                hr(style="border: 1px solid darkgrey"),
                # p('For more information, contact: ', a(href = 'mailto:david.altare@waterboards.ca.gov', 'david.altare@waterboards.ca.gov')),
                tags$b(h4('Application Information:')),
                actionButton(inputId = 'github', label = 'Code on GitHub', icon = icon('github', class = 'fa-1x'),
                             onclick ="window.open('https://github.com/daltare/Stormwater_Enforcement_Tool')")
            ),
                
            # Show map
            mainPanel(
                leaflet::leafletOutput('monitoring.map'),
                # tags$br(), 
                tags$hr(),
                h3('Data:'),
                DT::dataTableOutput('WQI.table')
            )
        )
    )

# Define server logic required to draw map -------------------------------------
server <- function(input, output) {
    # reactive({
    # observeEvent(input$standard, {
    observe({
        # STANDARDS (create a new table with relevant standards)
            standards.applied <- standards %>% dplyr::filter(Standard.Type == input$standard)
            # standards.applied <- standards %>% dplyr::filter(Standard.Type == standard)
         
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
                    
    
        # Calculate the WQI Scores 
            # Calculate Exceedance (for F1) and Excursion (for F2)
                WQI.Scores <- monitoring.data.WQI %>% dplyr::group_by(WDID, Monitoring.Period, Latitude, Longitude, Standard.Type) %>% dplyr::summarize(Exceedances = sum(Exceedance), Sum.Excursion = sum(Excursion), Total.Samples = n()) # Total.Samples(Exceedance))
            # Calculate F1 (Exceedance)
                WQI.Scores <- WQI.Scores %>% dplyr::mutate(F1 = Exceedances / Total.Samples * 100)
            # Calculate F2 (Excursion)
                WQI.Scores <- WQI.Scores %>% dplyr::mutate(NSE = Sum.Excursion / Total.Samples)
                WQI.Scores <- WQI.Scores %>% dplyr::mutate(F2 = NSE / (0.01 * NSE + 0.01))
            # Final WQI Scores
                WQI.Scores <- WQI.Scores %>% dplyr::mutate(WQI = round((100 - sqrt(F1^2 + F2^2) / 1.412),1))
                # WQI.Scores <-  WQI.Scores %>% dplyr::mutate(Frequency = round(F1,0))
                # WQI.Scores <-  WQI.Scores %>% dplyr::mutate(Magnitude = round(F2,0))
                WQI.Scores <- WQI.Scores %>% dplyr::left_join(facilities %>% dplyr::select(WDID, STATUS_CODE_NAME, REGION, FACILITY_NAME, FACILITY_ADDRESS, FACILITY_CITY, FACILITY_STATE, FACILITY_ZIP, FACILITY_COUNTY, RECEIVING_WATER_NAME, PRIMARY_SIC), by = 'WDID')
                
        # Map ------------------------------------------------------------------
            # Create the color palette
                leaflet.pal <- leaflet::colorNumeric(
                    palette = colorRamp(c('olivedrab2', 'red3'), interpolate='spline'),
                    domain = WQI.Scores$WQI,
                    reverse = TRUE
                )
    
            # Draw the map (filter for the selected monitoring period and WQI range)
                map.data <- as.data.frame(WQI.Scores %>% dplyr::filter(Monitoring.Period == input$monitoring.period & WQI >= input$score.range[1] & WQI <= input$score.range[2]))
                shared.map.data <- crosstalk::SharedData$new(map.data)
                
                output$monitoring.map <- leaflet::renderLeaflet({
                    leaflet::leaflet(shared.map.data) %>% 
                        # leaflet::addTiles() %>% 
                        leaflet::addProviderTiles('Esri.WorldStreetMap') %>% 
                        # leaflet::addProviderTiles('CartoDB.PositronOnlyLabels') %>% 
                        leaflet::addCircleMarkers(
                            radius = 2, 
                            opacity = 1,
                            # clusterOptions = leaflet::markerClusterOptions(),
                            color = ~leaflet.pal(WQI),
                            popup = ~paste0('<b>', '<u>','Facility Information','</u>','</b>','<br/>',
                                            '<b>', 'WDID: ', '</b>', WDID,'<br/>',
                                            '<b>', 'Facility Name: ', '</b>', FACILITY_NAME,'<br/>',
                                            '<b>', 'SIC: ', '</b>', PRIMARY_SIC,'<br/>',
                                            '<b>', 'Address: ', '</b>', FACILITY_ADDRESS, '<br/>',
                                            '<b>', 'City: ', '</b>', FACILITY_CITY, '<br/>',
                                            '<b>', 'Receiving Water: ', '</b>', RECEIVING_WATER_NAME,'<br/>',
                                            '<br/>',
                                            # '<hr/>',
                                            '<b>','<u>', 'Scoring','</u>','</b>','<br/>',
                                            '<b>', 'Monitoring Period: ', '</b>', Monitoring.Period,'<br/>',
                                            '<b>', 'Standard: ', '</b>', Standard.Type,'<br/>',
                                            '<b>', 'Exceedence Frequency: ', '</b>', round(F1,0),'<br/>',
                                            '<b>', 'Exceedence Magnitude: ', '</b>', round(F2,0),'<br/>',
                                            '<b>', 'WQI: ', '</b>', WQI, '<br/>')
                        ) %>% 
                        leaflet::addEasyButton(leaflet::easyButton(
                            icon="fa-globe", title="Center Map",
                            # onClick=JS("function(btn, map){ map.fitBounds([[40.712, -74.227],[40.774, -74.125]]); }"))),
                            onClick=JS(paste0('function(btn, map){ map.fitBounds([[', min(map.data$Latitude), ', ', min(map.data$Longitude), '],[', max(map.data$Latitude), ', ', max(map.data$Longitude), ']]); }'))))
                            
                })
                # ~min(Longitude), ~min(Latitude), ~max(Longitude), ~max(Latitude))))
        # Data Table -----------------------------------------------------------
            output$WQI.table <- DT::renderDataTable(
                shared.map.data, 
                extensions = c('Buttons', 'Scroller'),
                options = list(dom = 'Bfrtip', 
                               buttons = list('colvis', list(
                                   extend = 'collection',
                                   buttons = list(list(extend='csv', filename = 'cedenData'),
                                                  list(extend='excel', filename= 'cedenData')),
                                   text = 'Download Data' )),
                               scrollX = TRUE,
                               scrollY = 300, 
                               scroller = TRUE, 
                               deferRender = TRUE),
                class = 'cell-border stripe',
                # class = 'compact', 
                server = FALSE,
                rownames = FALSE#,
                # style = 'bootstrap'
            )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
