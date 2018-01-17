library(magrittr)

# reproduce the workflow to convert R1 lines to polygons, using R instead of ArcGIS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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


    
# Convert the Region 1 303d polylines to polygons
# step through 1 waterbody at a time --------------------------------
    runList <- 11:15 # enter the waterbodies to convert (i.e. the row numbers)
    for (i in runList) { #seq(nrow(R1_303d_lines))) {
        if (i == runList[1]) {counter <- 1} else {counter <- counter + 1}
        if (counter == 1) {
            if (exists('R1_lines_toPolygon')) {rm(list = 'R1_lines_toPolygon')}
        }
        R1_303d_lines_sp <- as(R1_303d_lines[i,], 'Spatial') # get the current waterbody, and convert to sp
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
                    R1_lines_toPolygon[counter,] <- R1_RasterToPolygon
                }
        # save the complete dataframe of polygons to an RDS file after each new polygon is added
            # saveRDS(object = R1_lines_toPolygon, file = paste0('data/Region1_linesToPolygons_Rworkflow_', runList[1],'_', i, '.RDS'))
            saveRDS(object = R1_lines_toPolygon, file = paste0('geospatial_simplification_check/R1_linesToPolygons/Region1_linesToPolygons_Rworkflow_', runList[1],'_', i, '.RDS'))
        # delete the file created in the previous step of this loop
            if (counter > 1) {
                file.remove(paste0('geospatial_simplification_check/R1_linesToPolygons/Region1_linesToPolygons_Rworkflow_', runList[1], '_', i-1, '.RDS'))
            }
        # clear variables
            rm(list = c('R1_RasterToPolygon', 'R1_303d_lines_sp', 'R1_Raster', 'R1_Raster_rasterize'))
            if (i == max(runList)) {rm(list = c('R1_lines_toPolygon', 'i', 'counter'))}
    } #  End of for loop  -------------------------------------------- #

    # check (plot)
        z <- readr::read_rds('geospatial_simplification_check/R1_linesToPolygons/Region1_linesToPolygons_Rworkflow_1_10.RDS')
            # ggplot2::ggplot() + ggplot2::geom_sf(data = z[12,])



# bind the datasets
    groups <- c('1_10', '11_12', '13_15', '16_20')
    for (i in seq(groups)) {
        if (i == 1) {
            z_data <- readr::read_rds(paste0('geospatial_simplification_check/R1_linesToPolygons/Region1_linesToPolygons_Rworkflow_', groups[i], '.RDS'))
        } else {
            temp <- readr::read_rds(paste0('geospatial_simplification_check/R1_linesToPolygons/Region1_linesToPolygons_Rworkflow_', groups[i], '.RDS'))
            z_data <- rbind(z_data, temp)
        }
    }


    # save the polygons as an RDS file
        # saveRDS(object = R1_RasterToPolygon_sf, file = 'data/simplified_2012_303d_lines_R1toPoly_Rworkflow.RDS')

    # read data from the saved RDS file
        # Read the data from the saved RDS file
            impaired_303d_R1_linesToPoly <- readr::read_rds('data/simplified_2012_303d_lines_R1toPoly.RDS')
        # Extract just those waterbodies that were dropped above (the large polylines, stored in variable R1_303d_LargePolylines)
            impaired_303d_R1_linesToPoly <- impaired_303d_R1_linesToPoly %>% dplyr::filter(WBID %in% R1_303d_LargePolylines$WBID)
        # drop some columns, to be consistent with the 303d polygons data
            impaired_303d_R1_linesToPoly <- impaired_303d_R1_linesToPoly %>% dplyr::select(-c(FID_, OBJECTID_1, WBID_1, Shape_Le_1))
        # convert back to sf (if needed)
            impaired_303d_R1_linesToPoly <- sf::st_as_sf(impaired_303d_R1_linesToPoly)
        # join these new R1 polygons to the original polygons file
            # impaired_303d_polygons <- impaired_303d_polygons %>% dplyr::bind_rows(impaired_303d_R1_linesToPoly)
            impaired_303d_polygons <- rbind(impaired_303d_polygons, impaired_303d_R1_linesToPoly)
