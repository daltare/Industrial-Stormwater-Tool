# reproduce the workflow to convert R1 lines to polygons, using R instead of ArcGIS

library(magrittr)    

    # for reference, get the size of each R1 polyline
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
            
# POLYLINE TO POLYGON CONVERSION --------------------------------------------------------------------------------------------------- #      
# ---------------------------------------------------------------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------------------------------------------------------------- #
# Convert the Region 1 303d polylines to polygons ----
    for (i in seq(nrow(R1_303d_lines))) { # step through 1 waterbody at a time 
        # check to make sure the output dataset doesn't already exist (delete if it does)
            if (i == 1) {
                if (exists('R1_lines_toPolygon')) {rm(list = 'R1_lines_toPolygon')}
            }
        # get the current waterbody, and convert to sp
            R1_303d_lines_sp <- as(R1_303d_lines[i,], 'Spatial') 
        # create an empy raster that bounds the waterbody. NOTE: This is where the resolution (grid size) is set !!!!!
            R1_Raster <- raster::raster(x = R1_303d_lines_sp, resolution = 250)
        # convert the polylines to raster (rasterize), based on the resolution (i.e., grid size) defined above. Give each cell the value of the 'OBJECTID' field so they can be grouped to the appropriate waterbody.
            R1_Raster_rasterize <- raster::rasterize(x = R1_303d_lines_sp, y = R1_Raster, field = 'OBJECTID')
        # convert the raster to a polygon
            R1_RasterToPolygon <- raster::rasterToPolygons(R1_Raster_rasterize, dissolve = TRUE) # dissolve means that if there is a disconnect in the polygon, it just creates one multi-part polygon
        # convert the polygon from object type sp to sf
            R1_RasterToPolygon <- sf::st_as_sf(R1_RasterToPolygon)
        # simplify the polygon - NOTE: set the simplification level here !!!!!
            R1_RasterToPolygon <- rmapshaper::ms_simplify(R1_RasterToPolygon, keep = 0.4)
        # join the polyline data to the new corresponding polygons
            R1_RasterToPolygon <- R1_RasterToPolygon %>% dplyr::left_join(R1_303d_lines %>% dplyr::select(-c(geometry)), by = c('layer' = 'OBJECTID')) %>% sf::st_as_sf()
        # append the polygon to the complete dataframe of polygons
            if (!exists('R1_lines_toPolygon')) {
                R1_lines_toPolygon <- R1_RasterToPolygon} else {
                    R1_lines_toPolygon[i,] <- R1_RasterToPolygon
                }
        # save the complete dataframe of polygons to an RDS file after each new polygon is added (in case it stops working before all polygons are done)
            saveRDS(object = R1_lines_toPolygon, file = paste0('data/Region1_linesToPolygons_Rworkflow_', i, '.RDS'))
        # delete the file created in the previous step of this loop
            if (i > 1) {
                file.remove(paste0('data/Region1_linesToPolygons_Rworkflow_', i-1, '.RDS'))
            }
        # clear variables
            rm(list = c('R1_RasterToPolygon', 'R1_303d_lines_sp', 'R1_Raster', 'R1_Raster_rasterize'))
        # after the last waterbody has been converted, rename the final file, then clear remaining variables
            if (i == max(seq(nrow(R1_303d_lines)))) {
                saveRDS(object = R1_lines_toPolygon, file = 'data/Region1_linesToPolygons_Rworkflow.RDS')
                file.remove(paste0('data/Region1_linesToPolygons_Rworkflow_', i, '.RDS'))
                rm(list = c('R1_lines_toPolygon', 'i'))
                }
    } #  End of for loop 
# END OF POLYLINE TO POLYGON COVNERSION SCRIPT ------------------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------------------------------------------------------------- #

        
        
        

# CHECK THE OUTPUT ----------------------------------------------------------------------------------------------------------------- #  
    # read in the new polygons (converted from polylines)
        R_polylineToPolygon <- readr::read_rds('data/Region1_linesToPolygons_Rworkflow.RDS')
    # plot
        ggplot2::ggplot() + ggplot2::geom_sf(data = R_polylineToPolygon[15,], fill = 'blue', alpha = 0.1, color = 'blue') + 
            ggplot2::theme_minimal()
            
    
    # check vs original polyline and vs the GIS version
        # get GIS data
            impaired_303d_R1_ArcGIS <- readr::read_rds('data/simplified_2012_303d_lines_R1toPoly.RDS')
        # enter a row number to check
            row_number <- 20
        # plot - NOTE: WHERE RED IS VISIBLE WITH NO GREY BACKGROUND, THE POLYGON DOESN'T COVER THE FULL EXTENT OF THE POLYLINES
            # get corresponding data
                wbid <- as.character(R_polylineToPolygon$WBID[row_number])
                arc_data <- impaired_303d_R1_ArcGIS[impaired_303d_R1_ArcGIS$WBID == wbid,]
                line_data <- R1_303d_lines[R1_303d_lines$WBID == wbid,]
            # create plot - with R-generated Polygons vs original PolyLines
                g_R <- ggplot2::ggplot() + 
                    ggplot2::geom_sf(data = line_data, color = 'red', size = 0.7) +
                    # ggplot2::geom_sf(data = arc_data, fill = 'grey', color = 'grey', alpha = 0.6) + # The ArcGIS Data
                    ggplot2::geom_sf(data = R_polylineToPolygon[row_number,], fill = 'grey', alpha = 0.2, color = 'grey') + # the R raster:: data
                    ggplot2::theme_minimal()
                g_R
            # create plot - with ArcGIS-generated Polygons vs original PolyLines
                g_Arc <- ggplot2::ggplot() + 
                    ggplot2::geom_sf(data = line_data, color = 'red', size = 0.7) +
                    ggplot2::geom_sf(data = arc_data, fill = 'grey', color = 'grey', alpha = 0.6) + # The ArcGIS Data
                    # ggplot2::geom_sf(data = R_polylineToPolygon[row_number,], fill = 'grey', alpha = 0.2, color = 'grey') + # the R raster:: data
                    ggplot2::theme_minimal()
                g_Arc
        