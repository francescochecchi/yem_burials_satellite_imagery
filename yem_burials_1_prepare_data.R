#..........................................................................................
### ++++ GEOSPATIAL AND STATISTICAL ANALYSIS OF CEMETERY DATA IN YEMEN (2011-2021) ++++ ###
#..........................................................................................

#..........................................................................................
## ------------------- R SCRIPT TO PREPARE DATA FOR FURTHER ANALYSIS ------------------- ##
#..........................................................................................

                                          # Written by Francesco Checchi, LSHTM (Dec 2022)
                                          # francesco.checchi@lshtm.ac.uk 



#.........................................................................................                            
### Preparing time series of cemeteries and days (as date and sequential day numbers)
    
  #...................................
  ## Construct generic time series

    # Construct time series
    dates <- seq(from = date_start, to = date_end , by = 1)
    days <- data.frame(dates, c(0:(length(dates) - 1) ) )
    colnames(days) <- c("date", "time_base")
    
  #...................................
  ## Construct time series for all included cemeteries
    
    # Construct generic time series
    ts <- expand.grid(subset(cemeteries, final_decision == "include")$cemetery_id, dates)
    colnames(ts) <- c("cemetery_id", "date")
    ts <- merge(ts, days, by ="date")
    ts <- ts[order(ts[, "cemetery_id"], ts[, "date"]), ]
    ts[, "cemetery_id"] <- as.character(ts[, "cemetery_id"])

    # Create additional period variables for added growth model, if desired later
      # backward version - baseline time
      x1 <- rep(seq(-as.integer(date_end - date_start), 0, by = 1 ), times = length(unique(ts$cemetery_id)) )
      ts[, "time_base_rev"] <- x1        

      # forward version - crisis time
      x1 <- rep(c(rep(0, times = (date_knots[1] - date_start) ),
        seq(1, as.integer(date_end - date_knots[1] + 1), by = 1 )), times = length(unique(ts$cemetery_id)) )
      ts[, "time_crisis"] <- x1
      
      # backward version - crisis time
      x1 <- rep(c(rep(-as.integer(date_end - date_knots[1]), times = (date_knots[1] - date_start) ),
        seq(-as.integer(date_end - date_knots[1]), 0, by = 1 )), times = length(unique(ts$cemetery_id)) )
      ts[, "time_crisis_rev"] <- x1
        
      # forward version - pandemic time
      x1 <- rep(c(rep(0, times = (date_knots[2] - date_start) ),
        seq(1, as.integer(date_end - date_knots[2] + 1), by = 1 )), times = length(unique(ts$cemetery_id)) )
      ts[, "time_covid"] <- x1
      
      # backward version - pandemic time
      x1 <- rep(c(rep(-as.integer(date_end - date_knots[2]), times = (date_knots[2] - date_start) ),
        seq(-as.integer(date_end - date_knots[2]), 0, by = 1 )), times = length(unique(ts$cemetery_id)) )
      ts[, "time_covid_rev"] <- x1

    # Define periods
    ts[, "period_base"] <- ifelse(ts$date < dates_analysis[2], "yes", "no")
    ts[, "period_base"] <- as.factor(ts$period_base)  

    ts[, "period_crisis"] <- ifelse(ts$date %in% c(dates_analysis[2]: (dates_analysis[3] - 1)), "yes", "no")
    ts[, "period_crisis"] <- as.factor(ts$period_crisis)  

    ts[, "period_covid"] <- ifelse(ts$date >= dates_analysis[3], "yes", "no")
    ts[, "period_covid"] <- as.factor(ts$period_covid)  

                        
#.........................................................................................                            
### Preparing the main observations dataset
    
  #...................................    
  ## Modify and add variables
    
    # Select only observations from included cemeteries and taken when the cemetery was active  
    obs <- subset(obs, analyse_cemetery == "yes" & (is.na(graves) | graves > 0))
    
    # Count the number of observations requiring imputation of grave counts
    table(obs$analyse_graves)
    prop.table(table(obs$analyse_graves))

    # Add 1 grave or 1 square metre to graves or area observations that equal 0 (so as to enable logging)
    obs[, "graves"] <- ifelse(obs$graves == 0, 1, obs$graves)  
    obs[, "area"] <- ifelse(obs$area == 0, 1, obs$area)  
      
    # Compute days since previous observation (image)
    obs <- obs[order(obs[, "cemetery_id"], obs[, "date"]), ]
    x1 <- c()    
    for (i in sort(unique(obs$cemetery_id)) ) {
      x3 <- subset(obs, cemetery_id == i)
      x1 <- c(x1, 0, as.integer(diff(x3$date)) )
    }
    obs[, "days_since"] <- x1

    # Add starting number of graves (temporary - will be updated after imputation step below)
    x1 <- c()
    for (i in sort(unique(obs$cemetery_id)) ) {
      x1 <- c(x1, min(subset(obs, cemetery_id == i)$graves, na.rm = TRUE) )
    }
    x1 <- data.frame(sort(unique(obs$cemetery_id)), x1)
    colnames(x1) <- c("cemetery_id", "graves_start")
    obs <- merge(obs, x1, by = "cemetery_id", x.all = TRUE)

    # Compute new graves and new area since the last observation
    obs <- obs[order(obs[, "cemetery_id"], obs[, "date"]), ]
    x1 <- c()
    x2 <- c()
    for (i in sort(unique(obs$cemetery_id)) ) {
      x3 <- subset(obs, cemetery_id == i)
      x1 <- c(x1, NA, diff (x3$graves ) )
      x2 <- c(x2, NA, diff (x3$area ) )

    }
    obs[, "new_graves"] <- x1
    obs[, "new_area"] <- x2

    
  #...................................    
  ## Merge dataset with time series
    
    # Merge dataset with time series
    obs <- merge(ts, obs, by = c("cemetery_id", "date"), all = TRUE)
    obs[, "date"] <- as.Date(obs$date)

    # Factorise variables as needed
      for (i in colnames(obs)) {
        if (class(obs[, i]) == "character" ) { obs[, i] <- as.factor(obs[, i])  }
      }


#.........................................................................................                            
### Preparing OCHA shape file for merging of predictor datasets
#.........................................................................................

  #...................................      
  ## Prepare OCHA shape file for subdistricts
    # Column names
    colnames(ocha_shape) <- tolower(colnames(ocha_shape))
    colnames(ocha_shape) <- gsub("adm1", "gov", colnames(ocha_shape))
    colnames(ocha_shape) <- gsub("adm2", "dis", colnames(ocha_shape))
    colnames(ocha_shape) <- gsub("adm3", "subdis", colnames(ocha_shape))
    colnames(ocha_shape)[colnames(ocha_shape) != "geometry"] <- paste(colnames(ocha_shape)[colnames(ocha_shape) != "geometry"], "_ocha_shape", sep = "")
    
    # Delete unnecessary columns
    ocha_shape <- ocha_shape[, c("gov_en_ocha_shape", "gov_ar_ocha_shape", "gov_pcode_ocha_shape", "dis_en_ocha_shape", "dis_ar_ocha_shape", "dis_pcode_ocha_shape",
                                 "subdis_ar_ocha_shape", "subdis_pcode_ocha_shape", "subdisref_en_ocha_shape", "geometry")]
    colnames(ocha_shape)[colnames(ocha_shape) == "subdisref_en_ocha_shape"] <- "subdis_en_ocha_shape"
    # like LSHTM, ocha_shape has dealt with instances of sub-districts with the same name by renaming them as 
    # "subdistrict - district" thereby creating a unique instance: keep this version
    
    # Check for duplicates
    ocha_shape[duplicated(ocha_shape[, c("gov_en_ocha_shape", "dis_en_ocha_shape", "subdis_en_ocha_shape")]), ]
    # (none identified)
    
  #...................................      
  ## Prepare OCHA shape file for districts
    # Column names
    colnames(ocha_shape_dis) <- tolower(colnames(ocha_shape_dis))
    colnames(ocha_shape_dis) <- gsub("adm2", "dis", colnames(ocha_shape_dis))
    colnames(ocha_shape_dis)[colnames(ocha_shape_dis) != "geometry"] <- paste(colnames(ocha_shape_dis)[colnames(ocha_shape_dis) != "geometry"], "_ocha_shape_dis", sep = "")
    
    # Delete unnecessary columns
    ocha_shape_dis <- ocha_shape_dis[, c("dis_en_ocha_shape_dis", "geometry")]
    colnames(ocha_shape_dis)[colnames(ocha_shape_dis) == "subdisref_en_ocha_shape_dis"] <- "subdis_en_ocha_shape_dis"
    
    # Check for duplicates
    ocha_shape_dis[duplicated(ocha_shape_dis[, "dis_en_ocha_shape_dis"]), ]
    # (none identified)
    
  #...................................      
  ## Prepare OCHA shape file for governorates
    # Column names
    colnames(ocha_shape_gov) <- tolower(colnames(ocha_shape_gov))
    colnames(ocha_shape_gov) <- gsub("adm1", "gov", colnames(ocha_shape_gov))
    colnames(ocha_shape_gov)[colnames(ocha_shape_gov) != "geometry"] <- paste(colnames(ocha_shape_gov)[colnames(ocha_shape_gov) != "geometry"], "_ocha_shape_gov", sep = "")
    
    # Delete unnecessary columns
    ocha_shape_gov <- ocha_shape_gov[, c("gov_en_ocha_shape_gov", "geometry")]
    colnames(ocha_shape_gov)[colnames(ocha_shape_gov) == "subdisref_en_ocha_shape_gov"] <- "subdis_en_ocha_shape_gov"
    
    # Check for duplicates
    ocha_shape_gov[duplicated(ocha_shape_gov[, "gov_en_ocha_shape_gov"]), ]
    # (none identified)
    

#.........................................................................................
### Predictors: Converting health facility shape file into a data frame by subdistrict
#.........................................................................................
    
  #...................................      
  ## Prepare health facility shape file

    # Delete unnecessary columns
    health_shape <- health_shape[, c("osm_id", "amenity", "geometry")]
    
    # Check for duplicates
    health_shape[duplicated(health_shape[, c("amenity", "geometry")]), ]
      # delete duplicates
      health_shape <- health_shape[! duplicated(health_shape[, c("amenity", "geometry")]), ]

  #...................................      
  ##  Map coordinates to OCHA subdistrict and clean up
    # from https://gis.stackexchange.com/questions/282750/identify-polygon-containing-point-with-r-sf-package 
    # also see https://spatialreference.org/ref/epsg/?search=Yemen&srtext=Search for chosen spatial projection
    
    # Prepare OCHA boundary shape file
      # apply planar transformation
      ocha_trans <- st_transform(ocha_shape, 4981)
      
    # Prepare health facility coordinate points
      # prepare a points collection
      pnts <- matrix(unlist(health_shape[, "geometry"]), byrow= TRUE, nrow = nrow(health_shape))
      pnts <- as.data.frame(pnts)
      colnames(pnts) <- c("x", "y")
      pnts_sf <- do.call("st_sfc", c(lapply(1:nrow(pnts), 
        function(i) {st_point(as.numeric(pnts[i, ]))}), list("crs" = 4326))) 

      # apply planar transformation
      pnts_trans <- st_transform(pnts_sf, 4981)

    # Find intersection of points and polygons, and extract OCHA subdistrict pcode
    pnts$subdis_pcode <- apply(st_intersects(ocha_trans, pnts_trans, sparse = FALSE), 2, 
      function(col) { ocha_trans[which(col), ]$subdis_pcode})
        
    # Merge with other OCHA variables
      # manage points dataset for merging
      x1 <- tidyr::unnest(pnts, cols = c(subdis_pcode))
      x1 <- as.data.frame(x1)
      colnames(x1) <- c("longitude", "latitude", "subdis_pcode")

    # Create predictor dataset (number of health facilities per subdistrict)
    x1[, "n_hf"] <- 1
    health_pred <- aggregate(x1[, "n_hf"], by = list(x1$subdis_pcode), FUN = sum)
    colnames(health_pred) <- c("subdis_pcode", "n_hf")
      
      # add subdistricts with zero mapped health facilities
      health_pred <- merge(health_pred, ocha[, c("subdis_en_ocha", "subdis_pcode")], by = "subdis_pcode", all.y = TRUE)
      health_pred[which(is.na(health_pred[, "n_hf"])), "n_hf"] <- 0
      
    # Remove unnecessary columns and objects
    health_pred <- health_pred[, c("subdis_pcode", "n_hf")]
    rm(pnts, pnts_sf, pnts_trans, ocha_trans, health_shape) 


#.........................................................................................
### Predictors: Converting road network shape file into a data frame by subdistrict
#.........................................................................................
    
  #...................................      
  ## Prepare road network shape file

    # Delete unnecessary columns
    road_shape <- road_shape[, c("OBJECTID", "geometry")]
    colnames(road_shape) <- c("id", "geometry")
        
  #...................................      
  ##  Map road segments to OCHA subdistrict and calculate road length
    # from https://stackoverflow.com/questions/56993193/road-length-within-polygons-in-r
    # also see https://spatialreference.org/ref/epsg/?search=Yemen&srtext=Search for chosen spatial projection
    
    # Prepare OCHA boundary shape file
      # apply planar transformation
      ocha_trans <- st_transform(ocha_shape, 4981)
      
    # Prepare road facility coordinate points
      # apply planar transformation
      road_trans <- st_transform(road_shape, 4981)
      
    # Find intersection between two
    x1 <- sf::st_intersection(ocha_trans, road_trans)
    
    # Calculate road distance (in Km) of each subdistrict segment
    x1[, "distance"] <- unlist(sf::st_length(x1[, "geometry"]) ) / 1000
    
    # Sum total road distance by subdistrict
    x2 <- sf::st_drop_geometry(x1)
    x2 <- aggregate(x2$distance, by = list(x2$subdis_pcode_ocha_shape), FUN = sum)
    colnames(x2) <- c("subdis_pcode_ocha_shape", "distance")
    x2[, "distance"] <- as.numeric(x2[, "distance"])  

  #...................................      
  ##  Compute surface area of subdistrict and calculate road density
    
    # Subdistrict surface area in Km^2
    ocha_trans[, "area"] <- as.numeric(unlist(sf::st_area(ocha_trans)) / (1000^2) )
    
    # Calculate density (Km road per Km^2 land area)
    x1 <- sf::st_drop_geometry(ocha_trans)
    x2 <- merge(x2, x1[, c("subdis_pcode_ocha_shape", "area")], by = "subdis_pcode_ocha_shape", all.y = TRUE)
    x2[which(is.na(x2[, "distance"])), "distance"] <- 0
    x2[, "road_density"] <- x2[, "distance"] / x2[, "area"]
    
    # Clean up and remove unnecessary columns and objects
    road_pred <- x2[, c("subdis_pcode_ocha_shape", "area", "distance", "road_density")]
    colnames(road_pred) <- c("subdis_pcode", "subdis_area", "road_distance", "road_density")
    rm(x1, x2, ocha_trans, road_trans) 
     

#.........................................................................................
### Predictors: Preparing ACLED insecurity dataset
#.........................................................................................

  #...................................      
  ## Check completeness and validity of latitude and longitude data 
    
    # To numeric
  	acled[, "latitude"] <- as.numeric(acled[, "latitude"])  
  	acled[, "longitude"] <- as.numeric(acled[, "longitude"])  
  	
  	# Range and distribution
  	range(acled$latitude, na.rm = TRUE)
  	hist(acled$latitude)
  	range(acled$longitude, na.rm = TRUE)
  	hist(acled$longitude)
  	
    # Eliminate missing or impossible coordinates
    acled[, "latitude"] <- ifelse(acled[, "latitude"] < 12 | acled[, "latitude"] > 19, NA, acled[, "latitude"])
    acled[, "longitude"] <- ifelse(acled[, "longitude"] < 41 | acled[, "longitude"] > 55, NA, acled[, "longitude"])
  
    # Check completeness
    table(is.na(acled[, "latitude"]) | is.na(acled[, "longitude"]))
      # 99.99% complete, barring three observations to delete
        acled <- subset(acled, ! is.na(latitude) & ! is.na(longitude))
  
  #...................................      
  ## Manage variables

    # Restrict to variables of interest
    table(is.na(acled[, "event_date"]))
    acled <- acled[, c("event_date", "event_type", "latitude", "longitude", "fatalities")]   
        
    # Create date variables
    acled[, "date"] <- as.Date.POSIXct(acled[, "event_date"])

    # Create / change aggregation variables
    colnames(acled)[colnames(acled) == "fatalities"] <- "n_fatalities"
    acled[, "n_events"] <- 1
    
  #...................................      
  ##  Map coordinates to OCHA subdistrict and clean up
    # from https://gis.stackexchange.com/questions/282750/identify-polygon-containing-point-with-r-sf-package 
    # also see https://spatialreference.org/ref/epsg/?search=Yemen&srtext=Search for chosen spatial projection
    
    # Prepare OCHA boundary shape file
      # apply planar transformation
      ocha_trans <- st_transform(ocha_shape, 4981)
      
    # Prepare ACLED coordinate points
      # prepare a points collection
      pnts <- acled[, c("longitude", "latitude")]
      colnames(pnts) <- c("x", "y")
      pnts_sf <- do.call("st_sfc", c(lapply(1:nrow(pnts), 
        function(i) {st_point(as.numeric(pnts[i, ]))}), list("crs" = 4326))) 

      # apply planar transformation
      pnts_trans <- st_transform(pnts_sf, 4981)

    # Find intersection of points and polygons, and extract OCHA subdistrict pcode
    pnts$subdis_pcode <- apply(st_intersects(ocha_trans, pnts_trans, sparse = FALSE), 2, 
      function(col) { ocha_trans[which(col), ]$subdis_pcode_ocha_shape})
        
    # Merge with ACLED dataset
      # manage points dataset for merging
      x1 <- tidyr::unnest(pnts, cols = c(subdis_pcode))
      x1 <- as.data.frame(x1)
      colnames(x1) <- c("longitude", "latitude", "subdis_pcode")
      x1 <- unique(x1)

      # merge into ACLED
      acled <- merge(acled, x1, by = c("longitude", "latitude"), all.x = TRUE )

      # double-check completeness
      table(is.na(acled[, "subdis_pcode"]))
        # remove 90 out of 64,507 records with missing subdistrict pcode
        acled <- subset(acled, ! is.na(subdis_pcode))
        nrow(acled)
          
    # Remove unnecessary columns
    acled <- acled[, ! colnames(acled) %in% c("longitude", "latitude", "event_date", "event_type")]
    rm(pnts, pnts_sf, pnts_trans, ocha_trans) 
            
    # Aggregate by subdistrict and date
    acled <- aggregate(acled[, c("n_events", "n_fatalities")], by = acled[, c("subdis_pcode", "date")],
      FUN = sum, na.rm = TRUE)
    
      
#.........................................................................................
### Predictors: Preparing wheat price dataset
#.........................................................................................

  #...................................      
  ## Select data
  x1 <- wheat[, c("date", "market", "usd_per_kg")]
  x1$date <- as.Date.POSIXct(x1$date)
  x1 <- subset(x1, date %in% c(date_start: date_end))
  
  #...................................      
  ## Convert prices to historical 2011 USD
  x1[, "year"] <- year(x1$date)
  x1 <- merge(x1, inflation, by = "year", all.x = TRUE)
  x1[, "usd_2011"] <- x1$usd_per_kg / x1$`2011_equiv`
    
  wheat <- x1[, c("date", "market", "usd_2011")]
  

#.........................................................................................                            
### Adding model predictors and population to main analysis dataset
   
  #...................................
  ## Add subdistrict- or cemetery-level static predictors
    
    # Add governorate under 5 mortality ratios to subdistrict dataset
    subdistricts <- merge(subdistricts, u5mr[, c ("gov_en", "u5mr")], by = "gov_en", all.x = TRUE)
    
    # Add health facility coverage
    subdistricts <- merge(subdistricts, health_pred, by = "subdis_pcode", all.x = TRUE)
    
    # Add road coverage
    subdistricts <- merge(subdistricts, road_pred, by = "subdis_pcode", all.x = TRUE)
    
    # Add predictors from subdistrict dataset to cemetery dataset
    cemeteries <- merge(cemeteries, subdistricts[, c("subdis_id", "subdis_pcode", "region", "market", "u5mr", "n_hf", "road_density")],
      by = "subdis_id", all.x = TRUE)    
    
    # Add predictors from cemetery dataset to observations dataset
    obs <- merge(obs, cemeteries[, c("subdis_pcode", "subdis_id", "region", "market", "u5mr", "n_hf", "road_density",
      "cemetery_id", "urban_rural", "soil_type")], by = "cemetery_id", all.x = TRUE)
    
      # recategorise some predictors
      table(obs$urban_rural)
      obs$urban_rural <-ifelse(obs$urban_rural != "urban", "non_urban", "urban")
      table(obs$urban_rural)

      table(obs$soil_type)
      # not usable, as almost all sandy


  #...................................
  ## Add time-varying predictors
    
    # Add ACLED insecurity data
    acled <- subset(acled, subdis_pcode %in% unique(obs$subdis_pcode))
    acled <- subset(acled, date %in% c(date_start: date_end))
    obs <- merge(obs, acled, by = c("subdis_pcode", "date"), all.x = TRUE)
    
      # if any data has NA values, convert to zero's
      obs[is.na(obs$n_events), "n_events"] <- 0  
      obs[is.na(obs$n_fatalities), "n_fatalities"] <- 0  
    
    # Add wheat prices
    obs <- merge(obs, wheat, by = c("market", "date"), all.x = TRUE)
      
      
  #...................................
  ## Add population and IDP denominators
  
    # Prepare population dataset
      # select data
      x1 <- subset(pop, scenario == "mid")  
      x1 <- x1[, c("subdis_pcode", "year", "month", "pop", "n_idp_in")]
      
      # add population before Jun 2014 (assume 3% annual growth rate and no displacement)
        # calculate monthly rate and difference in months
        monthly_rate <- 0.03/12
        months_diff <- round(as.integer(ymd(paste("2014", "6", "15", sep = "-")) -
          ymd(paste("2011", "1", "15", sep = "-"))) / 30.41, digits = 1)
        
        # back-calculate Jan 2011 population, by subdistrict
        x2 <- subset(x1, year == 2014 & month == 6)
        x2 <- by(x2, x2$subdis_pcode, function(x) {data.frame(x$subdis_pcode, 
          round(x$pop / ((1 + monthly_rate)^months_diff), 0))})
        x2 <- do.call(rbind, x2)
        colnames(x2) <- c("subdis_pcode", "pop")
        
        # add to population dataset       
        x2[, "month"] <- 1
        x2[, "year"] <- 2011
        x2[, "n_idp_in"] <- 0
        x1 <- rbind(x1, x2)
        
      # add October 2021 (missing as population reconstruction only went to September - assume equal to September)
      x2 <- subset(x1, year == 2021 & month == 9)
      x2$month <- 10
      x1 <- rbind(x1, x2)
      
      # deal with negative population in one subdistrict (a few months) -> set minimum population to 1000 arbitrarily
      x1[x1$pop < 1000, "pop"] <- 1000
      
      # create additional variables
      x1[, "date"] <- as.Date.character(paste(x1$year, x1$month, 1, sep = "-"))
      x1[, "prop_idp"] <- x1$n_idp_in / x1$pop
        # set about a dozen instances of proportion of IDPs > 1 to 1
              x1[x1$prop_idp > 1, "prop_idp"] <- 1
      
    # Add population dataset to main dataset  
    obs <- merge(obs, x1[, c("subdis_pcode", "date", "pop", "prop_idp")], by = c("subdis_pcode", "date"), 
      all.x = TRUE)
      
            
#.........................................................................................                            
### Finalising computation of predictor values
   
  #...................................
  ## Interpolate time-varying predictor values that are only available on a monthly basis
    
    # Sort dataset
    obs <- obs[order(obs$cemetery_id, obs$date), ]
     
    # For each cemetery time series, interpolate population and wheat price values
    x1 <- by(obs, obs$cemetery_id, function (x) {data.frame(x$cemetery_id, x$date,
      na.approx(x$pop, rule = 2, na.rm = FALSE),
      na.approx(x$prop_idp, rule = 2, na.rm = FALSE),
      na.approx(x$usd_2011, rule = 2, na.rm = FALSE))} )
    x1 <- do.call(rbind, x1)
    colnames(x1) <- c("cemetery_id", "date", "pop_ipol", "prop_idp_ipol", "usd_2011_ipol")
    
    # Merge back into observations
    obs <- merge(obs, x1, by = c("cemetery_id", "date"), all.x = TRUE)
    

  #...................................
  ## Add missing image quality score values for observations coming from Aden study (cemetery ID sd_09.03)
  obs[obs$cemetery_id == "sd_09.03" & ! is.na(obs$image_id), "image_score"] <- 0.5 
        
  
#.........................................................................................                            
### ENDS
#.........................................................................................                            
  