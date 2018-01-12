library(magrittr)

# Get the Region 1 PolyLine Data ----
    # get the size of each R1 polyline
        # read in the data and select Region 1 data
            R1_lines_memSizes <- readr::read_rds('data/simplified_2012_303d_lines.RDS') %>% dplyr::filter(REGION_NUM == 1) 
        # get the memory size of each waterbody
            R1_lines_memSizes <- R1_lines_memSizes %>% dplyr::mutate(memSize = apply(X = R1_lines_memSizes, MARGIN = 1, FUN = object.size))
        # set the size to units of bytes
            R1_lines_memSizes <- R1_lines_memSizes %>% dplyr::mutate(memSize = units::set_units(memSize, b))
        # convert the size from bytes to megabytes (MB)
            R1_lines_memSizes <- R1_lines_memSizes %>% dplyr::mutate(memSizeMB = units::set_units(memSize, Mb))
        # keep only certain columns
            R1_lines_memSizes <- R1_lines_memSizes %>% dplyr::select(OBJECTID, memSize, memSizeMB, WBID, EST_SIZE_A, SIZE_ASSES, WBTYPE, Shape_Leng)
    # read in the 303d polylines as an sf object, filter for R1 polylines, and convert to object type sp (SpatialLines object - data in this format is needed to work with the raster package)
        R1_303d_lines <- readr::read_rds('data/simplified_2012_303d_lines.RDS') %>% dplyr::filter(REGION_NUM == 1) %>% sf::st_as_sf()
    # transform to a projected coordinate system
        R1_303d_lines <- sf::st_transform(R1_303d_lines, 3857) # Transform to projected coordinate system; may also try 3310? 
    
    
# Check the polygon creation for an individual 303d waterbody, to compare resolution and simplification options ----

    # Define a function to create rasters for the selected waterbody and resolution level
        raster_check <- function(R1_WB_Numb, resolution_level) { #, plot_limits_x, plot_limits_y) {
            # create a list of lists to store the information - lists for: the polygons created (original and simplified versions), the size (memory) of the polygons created, plots of each polygon vs original polyline (including some zoomed in to show more detail), and the parameters passed to the function 
                z_test_data <- list('Polygons' = list(), 'Plots' = list(), 'Parameters' = list(), 'MemorySize' = list()) 
            # save parameter information passed to the function
                z_test_data$Parameters[['R1_WB_Numb']] <- R1_WB_Numb
                z_test_data$Parameters[['resolution_level']] <- resolution_level
            # save the size (memory) of the original polyline from which the polygon will be derived
                z_test_data$MemorySize[['original_polyline']] <- format(R1_lines_memSizes$memSizeMB[R1_WB_Numb], units = 'Mb', quote = FALSE, digits = 3L, standard = "auto")
            # get the selected 303d Polyline
                R1_303d_lines_test <- R1_303d_lines[R1_WB_Numb,]
            # convert the selected polyline to an sp (Spatial) object type
                R1_303d_lines_sp <- as(R1_303d_lines_test, 'Spatial') # convert to sp
            # create the empty raster - the resolution gives the cell size (e.g., resolution of 100 corresponds to 100 x 100 cell size) - data is in meters
                R1_Raster <- raster::raster(x = R1_303d_lines_sp, resolution = resolution_level)
            # the code below does the same thing as setting the resolution, so not needed. 
                # R1_extent <- raster::extent(R1_303d_lines_sp) # get the extent of the bounding box
                # x_dist <- R1_extent[2] - R1_extent[1] # get the x distance (in meters), i.e. width of bounding box
                # y_dist <- R1_extent[4] - R1_extent[3] # get the y distance (in meters), i.e. height of bounding box                  
                # R1_Raster <- raster::raster(x = R1_303d_lines_sp, nrows = round(y_dist / 100, 0), ncol = round(x_dist / 100, 0))
            # convert the lines to raster (rasterize)
                R1_Raster_rasterize <- raster::rasterize(x = R1_303d_lines_sp, y = R1_Raster, field = 'OBJECTID')
            # check (raster::plot)
                # raster::plot(R1_Raster_rasterize) # raster
                # raster::plot(R1_303d_lines_sp, add = TRUE) # add the polylines
                # raster::plot(R1_303d_lines_sp) # just the polylines
            # convert the raster to a polygon
                R1_RasterToPolygon <- raster::rasterToPolygons(R1_Raster_rasterize, dissolve = TRUE)
            # convert the polygons from sp to sf
                R1_RasterToPolygon <- sf::st_as_sf(R1_RasterToPolygon)
            # save the size (memory) of the new polygon
                z_test_data$MemorySize[['unsimplified_polygon']] <- format(object.size(R1_RasterToPolygon), units = 'Mb', quote = FALSE, digits = 3L, standard = "auto")
            # Create plots comparing the polyline to the original (unsimplified) polygon
                # full extent of the waterbody
                    g_unsimplified <- ggplot2::ggplot() + ggplot2::geom_sf(data = R1_RasterToPolygon) + 
                        ggplot2::geom_sf(data = R1_303d_lines_test, color = 'red') + 
                        ggplot2::coord_sf(xlim = NULL, ylim = NULL, datum = sf::st_crs(3857)) + 
                        ggplot2::theme_minimal(base_size = 8) + 
                        ggplot2::labs(title = paste0('Unsimplified (Size: ', z_test_data$MemorySize$unsimplified_polygon, ' | Waterbody: ', R1_WB_Numb, ' | Resolution: ', resolution_level, ')'), 
                                      subtitle = paste0('Original polyline size: ', z_test_data$MemorySize$original_polyline, ' | WBID: ', R1_303d_lines[R1_WB_Numb,]$WBID), 
                                      caption = 'NOTE: Red lines show original polyline')
            # store the original (un-simplified) data
                z_test_data$Polygons[['Unsimplified']] <- R1_RasterToPolygon
                z_test_data$Plots[['Unsimplified']] <- g_unsimplified
                # z_test_data$Plots[['Original_zoom']] <- g_original_zoom
            # Check what happens when simplifying the polygons - might loose too much detail?
                # Simplify the polygons at multiple levels of simplification, using keep = 0.5 
                    # (less simplificaiton, more detail retained) to keep = 0.1 (more simplificaiton, 
                    # least detail retained), in increments of 0.1        
                # Also create plots to show accuracy of coverage - where red is visible, it means that 
                    # the new polygon doesn't cover the extent of the original polyline
                # simplify and create plots
                    for (i in seq(0.5, 0.1, by = -0.1)) {
                        temp_poly <- rmapshaper::ms_simplify(R1_RasterToPolygon, keep = i)
                        temp_size <- format(object.size(temp_poly), units = 'Mb', quote = FALSE, digits = 3L, standard = "auto")
                        z_test_data$MemorySize[[paste0('simple_', i)]] <- temp_size
                        temp_plot_simple <- ggplot2::ggplot() + 
                            ggplot2::geom_sf(data = R1_303d_lines_test, color = 'red') + 
                            ggplot2::geom_sf(data = temp_poly) + 
                            ggplot2::coord_sf(datum = sf::st_crs(3857)) + 
                            ggplot2::theme_minimal(base_size = 8) +
                            ggplot2::labs(title = paste0('Simplified: ', i, ' (Size: ', temp_size, ' | Waterbody: ', R1_WB_Numb, ' | Resolution: ', resolution_level,')'), 
                                          subtitle = paste0('Original polyline size: ', z_test_data$MemorySize$original_polyline, ' | WBID: ', R1_303d_lines[R1_WB_Numb,]$WBID), 
                                          caption = 'NOTE: Where red lines are visible, the simplified polygon does not cover \nthe extent of the original polyline')
                        z_test_data$Polygons[[paste0('simplified_', i)]] <- temp_poly
                        z_test_data$Plots[[paste0('simplified_', i)]] <- temp_plot_simple
                        # z_test_data$Plots[[paste0('simple_', i, '_zoom')]] <- temp_plot_zoom
                        rm(list = c('temp_poly', 'temp_plot_simple', 'temp_size'))
                    } # end of for loop
                return(z_test_data)
        } # end of function

    # define functions for plotting ----
        # function - unsimplified plot 
            plot_unsimplified <- function(plot_data) {
                unsimplified_plot <- plot_data$Plots$Unsimplified
                unsimplified_plot
            }    
        # function - simplified plot
            plot_simplified <- function(plot_data, simple_level) {
                simplified_plot <- plot_data$Plots[paste0('simplified_', as.character(simple_level))][[1]]
                simplified_plot
            }
        # function - unsimplified vs simplified plot
            plot_unsimplified_v_simplified <- function(plot_data, simple_level) {
                unsimplified_plot <- plot_data$Plots$Unsimplified
                simplified_plot <- plot_data$Plots[paste0('simplified_', as.character(simple_level))][[1]]
                # output plots
                    cowplot::plot_grid(unsimplified_plot,
                                       simplified_plot)
            }
        # function - zoomed in plot
            plot_zoomed <- function(plot_data, simple_level, plot_limits_x, plot_limits_y) {
                # get the full extent plots
                    unsimplified_plot <- plot_data$Plots$Unsimplified
                    simplified_plot <- plot_data$Plots[paste0('simplified_', as.character(simple_level))][[1]]
                # create zoomed plots
                    unsimplified_plot_zoom <- suppressMessages(unsimplified_plot + ggplot2::coord_sf(xlim = plot_limits_x, ylim = plot_limits_y, datum = sf::st_crs(3857))) 
                    simplified_plot_zoom <- suppressMessages(simplified_plot + ggplot2::coord_sf(xlim = plot_limits_x, ylim = plot_limits_y, datum = sf::st_crs(3857))) 
                # output plots
                    cowplot::plot_grid(unsimplified_plot, 
                                       unsimplified_plot_zoom + ggplot2::labs(title = 'Unsimplified', subtitle = NULL),
                                       simplified_plot,
                                       simplified_plot_zoom + ggplot2::labs(title = 'Simplified', subtitle = NULL))
            }
            
# Run the analysis (examples) ----
    # run the function to create and simplify polygons
        wb13_res100 <- raster_check(R1_WB_Numb = 13, resolution_level = 100) #, plot_limits_x = c(-13741090, -13735000), c(5068000, 5075000))
    # show a plot of the unsimplified polygon, full extent
        plot_unsimplified(plot_data = wb13_res100)
    # show a plot of a simplified polygon, full extent
        plot_simplified(plot_data = wb13_res100, simple_level = 0.5)
    # show a plot of the unsimplified polygon versus a simplified polygon, full extent
        plot_unsimplified_v_simplified(plot_data = wb13_res100, simple_level = 0.5)
    # show a plot of the unsimplified polygon versus a simplified polygon, zoomed in to a selected extent
        plot_zoomed(plot_data = wb13_res100, simple_level = 0.5, plot_limits_x = c(-13740000, -13735000), plot_limits_y <- c(5070000, 5075000))

            