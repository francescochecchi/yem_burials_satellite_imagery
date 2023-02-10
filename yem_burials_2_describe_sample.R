#..........................................................................................
### ++++ GEOSPATIAL AND STATISTICAL ANALYSIS OF CEMETERY DATA IN YEMEN (2011-2021) ++++ ###
#..........................................................................................

#..........................................................................................
## ------------------- R SCRIPT TO DESCRIBE STUDY SAMPLE COMPOSITION ------------------- ##
#..........................................................................................

                                          # Written by Francesco Checchi, LSHTM (Dec 2022)
                                          # francesco.checchi@lshtm.ac.uk 



#.........................................................................................                            
### Describing sampled subdistricts
#.........................................................................................

  #...................................
  ## Tabulate subdistricts by key characteristics
    
    # Sort
    x1 <- subdistricts[order(subdistricts$gov_en, subdistricts$dis_en, subdistricts$subdis_en), ]

    # Format population and surface area figures
    x2 <- c("pop_jun2014", "pop_may2021", "n_idp_in_jun2014", "n_idp_in_may2021")
    x1[, x2] <- round(x1[, x2] , digits = -2)
    x1[, x2] <- format(x1[, x2], big.mark = ",")
    x1[, "area_m2"] <- format(x1[, "area_m2"], big.mark = ",")
    
    # Select and rename variables and save table
    x2 <- c("gov_en", "dis_en", "subdis_en", "region", "pop_jun2014", "pop_may2021", "n_idp_in_may2021", "area_m2")
    x3 <- c("Governorate", "District", "Subdistrict", "Region of control", "Population (June 2014)", "Population (May 2021)", "Number of IDPs (May 2021)", "Surface area (m^2)")
    x1 <- x1[ x2]
    colnames(x1) <- x3
    write.csv(x1, "out_tab_subdistrict_characteristics.csv", row.names = FALSE)
            
  #...................................
  ## Tabulate subdistricts by cemetery attrition

    # Aggregate cemeteries dataframe by subdistrict and final inclusion decision
    x1 <- cemeteries
    x1[, "n"] <- 1
    x1 <- aggregate(x1[, "n"], by = cemeteries[, c("subdis_id", "final_decision")], FUN = sum, na.rm = TRUE)
    colnames(x1)[3] <- "n"
    
    # Add all possible decisions
    x2 <- expand.grid(unique(x1$subdis_id), unique(x1$final_decision))
    colnames(x2) <- c("subdis_id", "final_decision")
    x1 <- merge(x2, x1, by = c("subdis_id", "final_decision"), all.x = TRUE)
    x1[, "n"] <- ifelse(is.na(x1[, "n"]), 0, x1[, "n"])

    # Reshape wide
    x2 <- reshape(x1, direction = "wide", idvar = "subdis_id", timevar = "final_decision")
    colnames(x2) <- sapply(colnames(x2), function(x) {gsub("n.exclude", "exclude", x)})
    colnames(x2) <- sapply(colnames(x2), function(x) {gsub("n.include", "include", x)})    
    
    # Compute sub-totals
      # all cemeteries listed
      x2[, "listed"] <- rowSums(x2[, -1])
    
      # cemeteries that aren't duplicates, closed before 2011 or outside subdistrict boundaries
      x2[, "listed_ok"] <- x2[, "listed"] - rowSums(x2[, c("exclude after triage - duplicate", "exclude after triage - closed before 2011",
        "exclude after triage - outside boundaries")])
    
      # cemeteries that are in the 'listed_ok' category and did have visible graves      
      x3 <- subset(cemeteries, ! final_decision %in% c("exclude after triage - duplicate", "exclude after triage - closed before 2011",
        "exclude after triage - outside boundaries"))      
      x3 <- subset(x3, any_graves_visible == "yes")
      x3[, "n"] <- 1
      x3 <- aggregate(x3$n, by = list(x3$subdis_id), FUN = sum, na.rm = TRUE)
      colnames(x3) <- c("subdis_id", "listed_ok_graves_visible")  
      x2 <- merge(x2, x3, by = "subdis_id", all.x = TRUE)
      x2$listed_ok_graves_visible <- ifelse(is.na(x2$listed_ok_graves_visible), 0, x2$listed_ok_graves_visible)
        
    # Add subdistrict pcodes, names and completion data
    x3 <- subdistricts[, c("subdis_id", "subdis_en", "subdis_pcode", "cemetery_search", "geolocalisation")]
    x3 <- merge(x3, x2, by = "subdis_id", all.x = TRUE)
    x3[is.na(x3)] <- 0
      
    # Compute additional statistics and reorder columns logically
    x3[, "percent_included_out_of_ok"] <- round(x3$include * 100 / x3$listed_ok, digits = 1)  
    x3[, "percent_included_out_of_ok_graves_visible"] <- round(x3$include * 100 / x3$listed_ok_graves_visible, digits = 1)  
    x3[, "sent_to_1715"] <- rowSums(x3[, grep("exclude after geo analysis", colnames(x3))]) + x3[, "include"]
    x3 <- x3[, c("subdis_id", "subdis_en", "subdis_pcode", "cemetery_search", "geolocalisation", "listed",
      "exclude after triage - duplicate", "exclude after triage - outside boundaries", "exclude after triage - closed before 2011",
      "listed_ok", "listed_ok_graves_visible",
      "exclude after triage - cannot geolocate", "exclude after triage - cannot see cemetery", "exclude after triage - cannot demarcate",
      "exclude after triage - cannot see graves", "exclude after triage - covered by vegetation", "exclude after triage - other reason",
      "sent_to_1715",
      "exclude after geo analysis - no analysable images", "exclude after geo analysis - only one analysable image", "exclude after geo analysis - unresolvable bias",
      "exclude after geo analysis - probably closed", "include", "percent_included_out_of_ok", "percent_included_out_of_ok_graves_visible")]
    
    # Write table
    write.csv(x3, "out_tab_subdistrict_attrition.csv", row.names = FALSE)    
      

#.........................................................................................                            
### Mapping subdistricts and show data availability in each
#.........................................................................................

  #...................................      
  ## Prepare data to be mapped  
    
    # Select data
    x1 <- x3[, c("subdis_en", "subdis_pcode", "include")]
    colnames(x1)[3] <- "N cemeteries included"
    x1[, "included"] <- 1

    # Merge with shape file
    colnames(ocha_shape) <- sapply(colnames(ocha_shape) , function(x) {gsub("_ocha_shape", "", x)})
    colnames(ocha_shape)[colnames(ocha_shape) == "subdis_en"] <- "subdis_en_ocha"
    x1 <- merge(ocha_shape, x1, by = c("subdis_pcode"), all.y = TRUE)
    # x1[is.na(x1$included), "included"] <- 0
    
    # Prepare governorate names
    pos_gov_x <- rep(0, times <- length(ocha_shape_gov$gov_en_ocha_shape_gov))
    pos_gov_y <- rep(0, times <- length(ocha_shape_gov$gov_en_ocha_shape_gov))
    names(pos_gov_x) <- ocha_shape_gov$gov_en_ocha_shape_gov
    names(pos_gov_y) <- ocha_shape_gov$gov_en_ocha_shape_gov
    pos_gov_y["Sana'a"] <- -0.4
    pos_gov_y["Al Mahwit"] <- -0.3
    pos_gov_y["Al Hodeidah"] <- 0.3
    pos_gov_x["Al Hodeidah"] <- -0.3    
    pos_gov_y["Aden"] <- -0.5
    pos_gov_x["Ibb"] <- -0.2
    pos_gov_x["Dhamar"] <- 0.2    
    pos_gov_x["Raymah"] <- -0.2  
    
  #...................................      
  ## Create and save map
  map1 <- tm_shape(ocha_shape_gov) +
    tm_text("gov_en_ocha_shape_gov", col = "grey20", size = 0.6, xmod = pos_gov_x, ymod = pos_gov_y, fontface = "bold") +
    tm_borders(col = "grey20", lwd = 2, alpha = 0.5) +
    tm_shape(x1) +
    tm_borders(col = palette_cb[7], lwd = 0.5) +
    tm_fill("included", col = palette_cb[7], alpha = 1, legend.show = FALSE) +
    tm_bubbles(size = "N cemeteries included", col = palette_cb[6], alpha = 0.2, border.col = palette_cb[6]) +
    tm_layout(legend.title.size = 0.8, legend.position = c("left", "top"))
  map1
  tmap_save(map1, "out_map_subdistricts.png", height = 15, width = 20, units = "cm", dpi = 300)
    

#.........................................................................................                            
### Describing characteristics of included and excluded cemeteries
#.........................................................................................

  #...................................      
  ## Compare included vs. excluded cemeteries
  
    # Select cemeteries that met initial triage criteria and could be geolocated
    x1 <- cemeteries
    x1 <- subset(x1, ! final_decision %in% c("exclude after triage - duplicate", "exclude after triage - outside boundaries", "exclude after triage - closed before 2011",
      "exclude after triage - cannot geolocate"))
    nrow(x1)
    
    # Classify cemeteries into included and excluded
    x1[, "include_bin"] <- ifelse(x1$final_decision == "include", "yes", "no")
    table(x1$include_bin)
  
    # Compare cemeteries included and excluded in terms of rural versus urban
    x1[, "urban_bin"] <- ifelse(x1$urban_rural == "urban", "urban", "not_urban")
    table(x1[, c("include_bin", "urban_bin")])
    prop.table(table(x1[, c("include_bin", "urban_bin")]), margin = 1)
    chisq.test(table(x1[, c("include_bin", "urban_bin")]))

    # Compare cemeteries included and excluded in terms of soil type
    table(x1[, c("include_bin", "soil_type")])
    prop.table(table(x1[, c("include_bin", "soil_type")]), margin = 1)
    chisq.test(table(x1[, c("include_bin", "soil_type")]))

  #...................................      
  ## Describe characteristics of included cemeteries
    
    # Select only observations from included cemeteries and taken when the cemetery was active  
    x1 <- subset(obs, analyse_cemetery == "yes" & (is.na(graves) | graves > 0) ) 
    
    # Count and describe number of images per cemetery
    x1 [, "n_images"] <- 1
    x2 <- aggregate(x1$n_images, by = list(x1$cemetery_id), FUN = sum, na.rm = TRUE)
    colnames(x2) <- c("cemetery_id", "n_images")  
      # frequency table  
      table(x2$n_images)  
      # total number of images
      sum(x2$n_images)
      # ratio of images per cemetery
      sum(x2$n_images) / nrow(x2)

    # Graph dates of image availability per cemetery
    x2 <- sort(unique(x1$cemetery_id))
    plot1 <- ggplot(data = x1, aes(x = date, y = cemetery_id)) +
      geom_point(size = 3, fill = palette_cb[4], colour = palette_cb[4], alpha = 0.5) +
      scale_x_date("date", date_breaks = "1 year", date_labels = "%Y", expand = c(0.01, 0.01)) +
      scale_y_discrete("cemetery ID") +
      theme_bw() +
      theme(axis.title = element_text(colour = "grey20")) +
      annotate("rect", xmin = dates_analysis[1], xmax = dates_analysis[2], ymin = x2[1],
        ymax = x2[length(x2)], alpha = 0.1, fill = palette_cb[4]) +
      annotate("rect", xmin = dates_analysis[2], xmax = dates_analysis[3], ymin = x2[1],
        ymax = x2[length(x2)], alpha = 0.1, fill = palette_cb[2]) +
      annotate("rect", xmin = dates_analysis[3], xmax = dates_analysis[4], ymin = x2[1],
        ymax = x2[length(x2)], alpha = 0.1, fill = palette_cb[7]) +
      geom_vline(xintercept = date_knots, colour = "grey20", linetype = "longdash") +
      annotate(geom = "text", hjust = 0, x = date_knots + 30, y = "sd_22.51", label = c("crisis", "pandemic"))
      
    plot1
    ggsave("out_dates_images.png", units = "cm", dpi = "print", width = 20, height = 13)      
      
    # Describe surface area as of first analysable observation
    x2 <- subset(x1, analyse_area == "yes")
    x3 <- by(x2, x2$cemetery_id, function(x) {x[which.min(x$date), ]})
    x3 <- do.call(rbind, x3)
    nrow(x3)
    quantile(x3$area, c(0.50, 0.25, 0.75))
            
    # Describe number of graves as of first analysable observation (before imputation)
    x2 <- subset(x1, analyse_graves == "yes")
    x3 <- by(x2, x2$cemetery_id, function(x) {x[which.min(x$date), ]})
    x3 <- do.call(rbind, x3)
    nrow(x3)
    quantile(x3$graves, c(0.50, 0.25, 0.75))
    
    
#.........................................................................................                            
### ENDS
#.........................................................................................
    