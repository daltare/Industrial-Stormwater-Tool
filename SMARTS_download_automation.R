# Automate SMARTS Data Download (use the Chrome browser in this script)
        # Note - for more information / examples, see:
            # https://stackoverflow.com/questions/35504731/specify-download-folder-in-rselenium        
            # https://cran.r-project.org/web/packages/RSelenium/vignettes/RSelenium-basics.html
            # https://stackoverflow.com/questions/32123248/submitting-form-from-r-to-mixed-html-and-javascript
    
    # set the download directory (i.e., where to save any downloaded files)
        download.dir <- getwd() # setting it to the current working directory
        
    # Set up RSelenium 
        library(RSelenium)
        current_wd <- getwd()
        eCaps <- list( # this sets the chrome options for the Selenium session
            chromeOptions = 
                list(prefs = list(
                    "profile.default_content_settings.popups" = 0L,
                    "download.prompt_for_download" = FALSE,
                    "download.default_directory" = download.dir
                )
                )
        )
        rsD <- RSelenium::rsDriver(port = 4444L, browser = 'chrome', extraCapabilities = eCaps)
        remDr <- rsD$client
        # probably don't need these lines anymore:
            # remDr <- remoteDriver(browserName="chrome")
            # remDr$open()    
    
    # Navigate to the SMARTS homepage
        url <- "https://smarts.waterboards.ca.gov/smarts/SwPublicUserMenu.xhtml"
        remDr$navigate(url)
        # remDr$getTitle() # just to check
        # remDr$getCurrentUrl() # just to check
    
    # Follow the link to the data download page
        webElem <- remDr$findElement(using = 'id', value = 'publicMenuForm:noiDataLink')
        webElem$clickElement()
    
    # Switch to the new window (the data download page)
        currWin <- remDr$getCurrentWindowHandle()
        allWins <- unlist(remDr$getWindowHandles())
        otherWindow <- allWins[!allWins %in% currWin[[1]]]
        remDr$switchToWindow(otherWindow)
        # remDr$getTitle() # just to check
        # remDr$getCurrentWindowHandle() # just to check
    
    # find and select the 'Select Regional Board' dropdown box
        webElem <- remDr$findElement(using = 'id', value = 'intDataFileDowloaddataFileForm:intDataDumpSelectOne')
        
    # Set the dropdown value to 'State Board'
        webElem$sendKeysToElement(list('State Board', key = 'enter'))
        # NOTE: It's also possible to do this 'manually' (i.e., by recreating the mouse actions and button clicks), like this:
            # loc <- webElem$getElementLocation()
            # remDr$mouseMoveToLocation(webElement = webElem)
            # remDr$click()
            # # create a list with the actions (down arrow, enter, etc.)
            #     select_board_dropdown <- list()
            #     for (i in 1:13) {
            #         select_board_dropdown[i] = c('key' = 'down_arrow')
            #     }
            #     select_board_dropdown[i+1] = c('key' = 'enter')
            # remDr$sendKeysToActiveElement(
            #     select_board_dropdown
            # )    
        
    # select the 'Industrial Ad Hoc Reports - Parameter Data' link (to get the monitoring data)
    # this automatically downloads the file to the default download location for the browser, set above
            # NOTE: Files downloaed from SMARTS are automatically named file.txt, so check to make sure there isn't already an un-named file (file.txt) in 
            # this location  - if so, delete it (otherwise the newly downloaded file will be appended with a number, and the versions might get confused)
                if (file.exists('file.txt')) {
                    unlink('file.txt')
                }
            webElem <- remDr$findElement(using = 'id', value = 'intDataFileDowloaddataFileForm:industrialRawDataLink')
            webElem$clickElement()
        # Rename the file, and append with the date for easier identification (may want to add in the time too?)
            file.rename(from = 'file.txt', to = paste0('Industrial_Ad_Hoc_Reports_-_Parameter_Data_', Sys.Date(), '.txt'))
        
    # select the 'Industrial Application Specific Data' link (to get the facility information)
    # this automatically downloads the file to the default download location for the browser, set above
            # NOTE: Files downloaed from SMARTS are automatically named file.txt, so check to make sure there isn't already an un-named file (file.txt) in 
            # this location  - if so, delete it (otherwise the newly downloaded file will be appended with a number, and the versions might get confused)
                if (file.exists('file.txt')) {
                    unlink('file.txt')
                }
            webElem <- remDr$findElement(using = 'id', value = 'intDataFileDowloaddataFileForm:industrialAppLink')
            webElem$clickElement()
        # Rename the file, and append with the date for easier identification (may want to add in the time too?)
            file.rename(from = 'file.txt', to = paste0('Industrial_Application_Specific_Data_', Sys.Date(), '.txt'))
    
    
    # If the file is downloaded to the default location, find the file in the downloads folder, and load it into R
    # Note: need to change the file name here, or find a way to change it manually
        # download.location <- 'C:\\Users\\daltare\\Downloads'
        # download.file.name <- 'file (28).txt'
        # table1 <- readr::read_tsv(file = paste0(download.location, '\\', download.file.name))
        
        
    # To go to the SMARTS password-protected page (NOTE: Save your SMARTS username and password to Environment Variables for your account, named as SMARTS_username and SMARTS_password):
        # SMARTS.username <- Sys.getenv('SMARTS_username')
        # SMARTS.password <- Sys.getenv('SMARTS_password')
        # url <- "https://smarts.waterboards.ca.gov"
        # remDr$navigate(url)
        # webElem <- remDr$findElement(using = 'id', value = 'loginForm:userId')
        # webElem$sendKeysToElement(list(SMARTS.username))# , key = 'enter'))
        # webElem <- remDr$findElement(using = 'id', value = 'loginForm:password')
        # webElem$sendKeysToElement(list(SMARTS.password))
        # webElem <- remDr$findElement(using = 'id', value = 'loginForm:loginButton')
        # webElem$clickElement()