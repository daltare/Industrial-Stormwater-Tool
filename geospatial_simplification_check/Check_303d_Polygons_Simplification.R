# 303d Polygons ------------------------------------------------------------------------------------------------------------------------------------
    # These steps show how to access and transform the 303d geospatial polygon data, but they only need to be done once:
        # download the data from the original source (a zip file on the internet), then unzip the file, and save the 
        # unzipped file in the 'data' folder
            # temp_zip_impaired_poly <- tempfile()
            # impaired_poly_url <- 'https://gispublic.waterboards.ca.gov/webmap/303d_2012/files/2012_Impaired_Polys_Final.zip'
            # download.file(url = impaired_poly_url, destfile = temp_zip_impaired_poly, method = 'curl')
            # unzip(zipfile = temp_zip_impaired_poly, exdir = 'data/2012_Impaired_Polygons_Final', junkpaths = TRUE)
            # unlink(temp_zip_impaired)
        # Read the shapefile into R
            impaired_303d_polygons_original <- sf::st_read('data/2012_Impaired_Polygons_Final/2012_Impaired_Polys_Final.shp')
        # transform the data into a coordinate reference system
            impaired_303d_poly_transform <- sf::st_transform(impaired_303d_polygons_original, 4326)
        # get the memory size of each polygon (can check how the smallest and largest are treated, ...)
            impaired_303d_poly_transform <- impaired_303d_poly_transform %>% dplyr::mutate(memSize = apply(X = impaired_303d_poly_transform, MARGIN = 1, FUN = object.size))
            # set the size to units of bytes
                impaired_303d_poly_transform <- impaired_303d_poly_transform %>% dplyr::mutate(memSize = units::set_units(memSize, b))
            # convert the size from bytes to megabytes (MB)
                impaired_303d_poly_transform <- impaired_303d_poly_transform %>% dplyr::mutate(memSizeMB = units::set_units(memSize, Mb))
            # convert back to sf
                impaired_303d_poly_transform <- impaired_303d_poly_transform %>% sf::st_as_sf()
            
            
            
            
        # simplify the polygons - try different levels, using different values of the keep parameter
            simple_0.05 <- rmapshaper::ms_simplify(impaired_303d_poly_transform, keep = 0.05) # this is the default value
            simple_0.1 <- rmapshaper::ms_simplify(impaired_303d_poly_transform, keep = 0.1)
            simple_0.5 <- rmapshaper::ms_simplify(impaired_303d_poly_transform, keep = 0.5)
            simple_0.6 <- rmapshaper::ms_simplify(impaired_303d_poly_transform, keep = 0.6)
            simple_0.7 <- rmapshaper::ms_simplify(impaired_303d_poly_transform, keep = 0.7)
            simple_0.8 <- rmapshaper::ms_simplify(impaired_303d_poly_transform, keep = 0.8)
            
        # at higher levels of simplification (~keep < 0.6), some polygons are lost - get the dropped polygons
            tf <- impaired_303d_polygons_original_transform$OBJECTID %in% simple_0.5$OBJECTID
            dropped <- impaired_303d_polygons_original_transform[!tf,]
            ggplot2::ggplot() + ggplot2::geom_sf(data = dropped)
            # look at this polygon at levels of simplification where it's not dropped
                ggplot2::ggplot() + ggplot2::geom_sf(data = simple_0.7[simple_0.6$OBJECTID == dropped$OBJECTID, ])
            





