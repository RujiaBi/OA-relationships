#Load database
load ('data/FishGlob_public_std_clean.RData') #obtained from FishGlob repository (script merge.R)

res <- list ()
ct <- 1
for (i in 1:nrow(fg_keep)){
  #Retrieve info about stock
  survey_i <- fg_keep$survey [i]
  species <- fg_keep$accepted_name[i]
  first_year <- fg_keep$first_year [i]
  last_year <- fg_keep$last_year [i]
  match_key <- fg_keep$match_key [i]
  
  for (y in first_year:last_year) {
    
    #Filter FishGlob data
    data_stock_i <- data %>%
      filter (accepted_name == species, 
              survey == survey_i,
              year == y)
    
    if (nrow(data_stock_i) == 0){
      next
    }
    
    #Obtain lat,long and UTM reference for stock i
    latitude <- data_stock_i$latitude %>% median()
    longitude <- data_stock_i$longitude %>% median() 
    UTM_zone <- find_UTM_zone(longitude = longitude, latitude =  latitude)
    
    # convert the data frame to a SpatialPoints object
    df <- data_stock_i %>%
      dplyr::select (latitude, longitude)
    coordinates(df) <- c("longitude", "latitude")
    
    # set the projection of the SpatialPoints object to WGS84
    proj4string(df) <- CRS("+proj=longlat +datum=WGS84")
    
    # transform the SpatialPoints object to UTM
    crs <- paste0 ("+proj=utm +zone=", UTM_zone, " +datum=WGS84")
    df_utm <- spTransform(df, CRS(crs))
    
    # calculate the area of the convex hull of the points in square km
    ConvexHull <- gConvexHull(df_utm)
    area <- gArea(ConvexHull) / 1e+6
    
    res[[ct]] <- data.frame ( accepted_name = species,
                              year = y,
                              match_key = match_key,
                              CH_area = area,
                              survey_i = survey_i
                              )
    ct <- ct + 1
  }
}

stocks_hull <- do.call ("rbind", res)

rm(list=setdiff(ls(), c("fg_keep", "stocks_hull", "timeseries_values_views")))

write.table (stocks_hull, "data/stocks_hull.csv")