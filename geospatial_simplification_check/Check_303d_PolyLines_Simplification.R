library(magrittr)

# 303d PolyLines ------------------------------------------------------------------------------------------------------------------------------------
# NOTE: Not considering Region 1 lines here (they are very large) - this is just to get a sense of what level of simplification might be okay
    # These steps show how to access and transform the 303d geospatial polyline data, but they only need to be done once:
        # download the data from the original source (a zip file on the internet), then unzip the file, and save the
        # unzipped file in the 'data' folder
            # temp_zip_impaired <- tempfile()
            # impaired_url <- 'https://gispublic.waterboards.ca.gov/webmap/303d_2012/files/2012_Impaired_Lines_Final.zip'
            # download.file(url = impaired_url, destfile = temp_zip_impaired, method = 'curl')
            # unzip(zipfile = temp_zip_impaired, exdir = 'data/2012_Impaired_Lines_Final', junkpaths = TRUE)
            # unlink(temp_zip_impaired)
        # Read the shapefile into R
            impaired_303d_original <- sf::st_read('data/2012_Impaired_Lines_Final/2012_Impaired_Lines_Final.shp')
        # drop the region 1 lines - these are very big, and are handled by converting some to polygons (in a separate script)
            impaired_303d_original <- impaired_303d_original %>% dplyr::filter(REGION_NUM != 1) %>% sf::st_as_sf()
        # transform the data into a coordinate reference system
            impaired_303d_transform <- sf::st_transform(impaired_303d_original, 4326)
        # get the memory size of each polyline (can check how the smallest and largest are treated, ...)
            impaired_303d_memSize <- impaired_303d_transform %>% dplyr::mutate(memSize = apply(X = impaired_303d_transform, MARGIN = 1, FUN = object.size))
            # set the size to units of bytes
                impaired_303d_memSize <- impaired_303d_memSize %>% dplyr::mutate(memSize = units::set_units(memSize, b))
            # convert the size from bytes to megabytes (MB)
                impaired_303d_memSize <- impaired_303d_memSize %>% dplyr::mutate(memSizeMB = units::set_units(memSize, Mb))
            # drop unneeded columns
                impaired_303d_memSize <- impaired_303d_memSize %>% dplyr::select(OBJECTID, WBID, memSize, memSizeMB, WBNAME, REGION_NUM, WBTYPE, Shape_Leng)
        
        # simplify the polygons - try different levels, using different values of the keep parameter
            simple_0.05 <- rmapshaper::ms_simplify(impaired_303d_transform, keep = 0.05)  # this is the default value
                simple_0.05$simplification = 0.05
                simple_0.05$rowNumber <- seq.int(nrow(simple_0.05))
            simple_0.1 <- rmapshaper::ms_simplify(impaired_303d_transform, keep = 0.1)
                simple_0.1$simplification = 0.1
                simple_0.1$rowNumber <- seq.int(nrow(simple_0.1))
            simple_0.2 <- rmapshaper::ms_simplify(impaired_303d_transform, keep = 0.2)
                simple_0.2$simplification = 0.2
                simple_0.2$rowNumber <- seq.int(nrow(simple_0.2))
            simple_0.3 <- rmapshaper::ms_simplify(impaired_303d_transform, keep = 0.3)
                simple_0.3$simplification = 0.3
                simple_0.3$rowNumber <- seq.int(nrow(simple_0.3))
            simple_0.4 <- rmapshaper::ms_simplify(impaired_303d_transform, keep = 0.4)
                simple_0.4$simplification = 0.4
                simple_0.4$rowNumber <- seq.int(nrow(simple_0.4))
            simple_0.5 <- rmapshaper::ms_simplify(impaired_303d_transform, keep = 0.5)
                simple_0.5$simplification = 0.5
                simple_0.5$rowNumber <- seq.int(nrow(simple_0.5))
                

        # Make a plot function
            plot_lines <- function(row_number, line_size = c(6,1,1,1,1,3,1)) {
                plot_data <- impaired_303d_transform
                plot_data$simplification = 0
                plot_data$rowNumber <- seq.int(nrow(plot_data))
                plot_data <- rbind(plot_data, simple_0.05, simple_0.1, simple_0.2, simple_0.3, simple_0.4, simple_0.5)
                plot_data$simplification <- as.factor(plot_data$simplification)
                g <- ggplot2::ggplot() + ggplot2::geom_sf(data = plot_data[plot_data$rowNumber == row_number,], ggplot2::aes(color = simplification, fill = simplification), size = line_size) #, color = RColorBrewer::brewer.pal(n = 7, name = 'Accent'), show.legend = TRUE)
                # g <- g + ggplot2::scale_color_brewer(type = 'div', palette = 'Accent') + ggplot2::scale_fill_brewer(type = 'div', palette = 'Accent')
                g <- g + ggplot2::scale_color_manual(values = c('black', RColorBrewer::brewer.pal(6,name = 'Accent')))
                g <- g + ggplot2::scale_fill_manual(values = c('black', RColorBrewer::brewer.pal(6,name = 'Accent')))
                g
            }
            
        # Output some plots of different waterbodies at various simplification levels
            plot_lines(2)
            plot_lines(3)
            plot_lines(4) 
            plot_lines(100)
            plot_lines(200)
            plot_lines(300)
            plot_lines(400)
            plot_lines(500)
            plot_lines(665)
            plot_lines(667)
            plot_lines(667, line_size = rep(2,7))
            plot_lines(667, line_size = c(3, rep(1,6)))
            
            
            
    
