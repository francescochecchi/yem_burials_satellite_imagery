#..........................................................................................
### ++++ GEOSPATIAL AND STATISTICAL ANALYSIS OF CEMETERY DATA IN YEMEN (2011-2021) ++++ ###
#..........................................................................................

#..........................................................................................
## ------------------- R SCRIPT TO IMPUTE MISSING GRAVE COUNT VALUES ------------------- ##
#..........................................................................................

                                          # Written by Francesco Checchi, LSHTM (Dec 2022)
                                          # francesco.checchi@lshtm.ac.uk 


#.........................................................................................      
### Visualising patterns in the data

  #...................................   
  ## Select data on which imputation model can be trained
    
    # Select observations from included cemeteries for which both area and graves are analysable
    obs_impute <- subset(obs, analyse_cemetery == "yes" & analyse_area == "yes" & analyse_graves == "yes")
    
    # Eliminate cemeteries for which there aren't at least two images left after the above
    x1 <- by(obs_impute, obs_impute$cemetery_id, function(x) {if (nrow(x) > 1) return(x)})
    obs_impute <- do.call(rbind, x1)  
    
    # Calculate days, new surface area and new graves since previous image
      # days since previous image date
      obs_impute <- obs_impute[order(obs_impute[, "cemetery_id"], obs_impute[, "date"]), ]
      x1 <- c()    
      for (i in sort(unique(obs_impute$cemetery_id)) ) {
        x3 <- subset(obs_impute, cemetery_id == i)
        x1 <- c(x1, 0, as.integer(diff(x3$date)) )
      }
      obs_impute[, "days_since"] <- x1

      # new surface area and new graves
      obs_impute <- obs_impute[order(obs_impute[, "cemetery_id"], obs_impute[, "date"]), ]
      x1 <- c()
      x2 <- c()
      for (i in sort(unique(obs_impute$cemetery_id)) ) {
        x3 <- subset(obs_impute, cemetery_id == i)
        x1 <- c(x1, NA, diff (x3$graves ) )
        x2 <- c(x2, NA, diff (x3$area ) )
  
      }
      obs_impute[, "new_graves"] <- x1
      obs_impute[, "new_area"] <- x2
        
      
    # Eliminate first image observations, as these do not provide values of new graves / new area
    obs_impute <- subset(obs_impute, days_since != 0)
    
    # Eliminate single outlier observation
    obs_impute <- obs_impute[-which.min(obs_impute$new_area), ]
    
    # Check how many there are
    nrow(obs_impute)
    
    # Add cemetery variables (urban vs. not urban, sandy vs. other soil type)
    obs_impute <- merge(obs_impute, cemeteries[, c("cemetery_id", "urban_rural", "soil_type")], 
      by = "cemetery_id", all.x = TRUE)
    table(obs_impute$urban_rural)
    table(obs_impute$soil_type) # all are on sandy soil, so this variable is not usable for modelling
    
    
  #...................................   
  ## Visualise correlation between graves and surface area, depending on whether infilling occurred
 
    # Correlation between new graves and new surface area, depending on whether infilling occurred
    plot <- ggplot(data = obs_impute ) +
      geom_point(mapping = aes(colour = infilling, x = new_area, y = new_graves), size = 3) +
      scale_colour_manual(values = palette_cb[c(4, 7)]) +
      scale_y_continuous("number of new graves", minor_breaks=NULL, trans = "log2",
        breaks = c(0,20,100,200,500,1000,2000)) +
      scale_x_continuous("new surface area (square metres)", trans = "log2",
        breaks = c(0,50,100,250,500,1000,2000,5000,10000), labels = scales::comma) +
      theme_bw() +
      theme(legend.position = "top", legend.direction = "horizontal") +
      labs(colour = "Infilling:  ") +
      theme(legend.title = element_text(color="grey20", size=11),
        legend.text = element_text(color="grey20", size=11),
        axis.title.x = element_text(color="grey20", size=11), 
        axis.text.x = element_text(color = "grey20", size=11, vjust=0),               
        axis.line.y = element_line(color = "grey20"),
        axis.ticks.y = element_line(color = "grey20"),
        axis.text.y = element_text(color = "grey20", size=11),
        axis.title.y = element_text(color="grey20", margin = margin(r = 10), size=11 ),
        plot.margin = unit(c(0.5,2,0.5,0.5), "cm")
      )
    plot
    ggsave("out_new_graves_vs_new_area.png", width = 22, height = 12, units = "cm", dpi = "print") 

    # Distributions of ew graves and new area    
      # linear scale
      f_hist("new_graves", obs_impute, c(NA, NA) )
      f_hist("new_area", obs_impute, c(NA, NA) ) 
      
      # log scale
      obs_impute[, "new_graves_ln"] <- log(obs_impute[, "new_graves"])
      f_hist("new_graves_ln", obs_impute, c(NA, NA) ) 
      obs_impute[, "new_area_ln"] <- log(obs_impute[, "new_area"])
      f_hist("new_area_ln", obs_impute, c(NA, NA) ) 
      
            
#...................................   
### Fitting a predictive model of graves as a function of surface area

  #...................................   
  ## Fit a discrete generalised linear mixed model; log(starting n of graves) as offset
    
    # Fit model of new graves (more robust than graves as it constrains total predicted graves to >= 0)
    fit <- f_glmm("new_graves", c("new_area_ln"), "(1 | cemetery_id)", "graves_start", obs_impute, "nbinom1")
    x1 <- f_do(fit, f_cv, k_folds, TRUE, TRUE)

    # Plot CV output
    plot <- ggplot(x1) +
      geom_point(aes(x = observations, y = predictions), size = 3, alpha = 0.5, colour = palette_cb[6]) + 
      theme_bw() +
      scale_x_continuous("observed", trans = "log2", breaks = c(0,5,10,25,50,100,250,500,1000), limits = c(NA, 1000)) +
      scale_y_continuous("predicted", trans = "log2", breaks = c(0,5,10,25,50,100,250,500,1000), limits = c(NA, 1000)) +  
      geom_abline(intercept = 0, slope = 1, colour = palette_cb[7] ) +
      theme(axis.title = element_text(colour="grey20")) +
      theme(plot.title = element_text(color = "grey20", size = 12, face = "plain") )
    
    plot
    ggsave("out_new_graves_imputation_loocv.png", width = 18, height = 12, units = "cm", dpi = "print")    
  
    # Save final model
    write.csv(tidy(fit), "out_graves_imputation_model.csv")  

    
#...................................   
### Using model predictions wherever grave counts are missing

  #...................................   
  ## Generate predictions for all observations
  obs[, "new_area_ln"] <- log(obs$new_area)
  obs[, "new_graves_pred"] <- predict(fit, newdata = obs, type = "response", allow.new.levels = TRUE)
  
  #...................................   
  ## Attribute predicted values to missing grave count observations
            
    # Attribute predicted values to missing observations
    obs[, "new_graves"] <- ifelse(is.na(obs[, "new_graves"]), round(obs[, "new_graves_pred"], 0), obs[, "new_graves"] )

    # Calculate total graves for missing observations, to the extent possible given new graves imputed above
      # process is iterative as newly imputed values in turn enable more imputation;
      # where both back- and forward-calculation are possible, a weighted average of both is applied
      
      # subset of observations with either graves, new graves or surface area non-missing
      x1 <- obs[! is.na(obs$graves) | ! is.na(obs$new_graves) | ! is.na(obs$area), 
        c("cemetery_id", "date", "graves", "new_graves")]
      x1[, "graves_imputed"] <- FALSE
        # sort
          x1 <- x1[order(x1[, "cemetery_id"], x1[, "date"]), ]
      
      # control parameters for loop
      control1 <- TRUE
      control2 <- 0
          
      # for each cemetery...
      for (i in sort(unique(x1$cemetery_id)) ) {

        # reset control parameter 1 (if FALSE for any cemetery after loop, replication stops)
        control1 <- TRUE # needs to be TRUE at start
        
        # while there are still potentially values to impute...
        while (control1 == TRUE) {
          # select data from cemetery
          x2 <- subset(x1, cemetery_id == i)
          
          # progress statement
          print(paste("now working on cemetery...", i, sep = "") )
          
          # reset control parameter 2
          control2 <- 0
            
          # for each of the observations...
          for (j in 1:nrow(x2) ) {
            x3 <- NA; x4 <- NA; x5 <- NA; x6 <- NA; x7 <- NA;
            
            # if the graves value is (still) missing...
            if ( is.na(x2[j, "graves"]) ) {
                  
              # calculate based on previous observation within the cemetery, if there is one
              if (j != 1) {x3 <- ifelse(is.na(x2[j-1, "graves"]), NA, x2[j-1, "graves"] + x2[j, "new_graves"] ) }
                  
              # calculate based on next observation within the cemetery, if there is one
              if (j != nrow(x2) ) {x4 <- ifelse(is.na(x2[j+1, "graves"]), NA, x2[j+1, "graves"] - x2[j+1, "new_graves"] ) }
                  
              # take a weighted mean of the alternative values based on previous and next observations
              if (! is.na(x3) | ! is.na(x4) ) {
                # calculate weights: whichever of the two is not available takes a zero weight
                x5 <- ifelse(is.na(x3), 0, x3 - x2[j-1, "graves"] )
                x6 <- ifelse(is.na(x4), 0, x2[j+1, "graves"] - x4 )
                x7 <- c(x5 / (x5 + x6), x6 / (x5 + x6) )
                x7[is.na(x7)] <- 0 # final weights
                # take weighted mean
                x1[x1$cemetery_id == i & x1$date == x2[j, "date"], "graves"] <- round(weighted.mean(c(x3, x4), x7), 0 )
                x1[x1$cemetery_id == i & x1$date == x2[j, "date"], "graves_imputed"] <- TRUE
                # update control parameter
                control2 <- control2 + 1
                
              }
            }
          }
        # update control parameter after each loop of 'while' statement
        if (control2 == 0) {control1 <- FALSE} else {control1 <- TRUE}
          
        }
    }

      
    # Attribute imputed values to missing observations
    x1 <- subset(x1, graves_imputed)
    x1[, "graves_imputed_values"] <- x1[, "graves"] 
    obs <- merge(obs, x1[, c("date", "cemetery_id", "graves_imputed_values", "graves_imputed")], by = c("date", "cemetery_id"), all.x = TRUE)
    obs[, "graves"] <- ifelse(is.na(obs[, "graves"]), obs[, "graves_imputed_values"], obs[, "graves"] )
      
    # Update starting number of graves variable
    obs <- obs[order(obs[, "cemetery_id"], obs[, "date"]), ]
    x1 <- c()
    for (i in sort(unique(obs$cemetery_id)) ) {
      x1 <- c(x1, min(subset(obs, cemetery_id == i)$graves, na.rm = TRUE) )
    }
    x1 <- data.frame(sort(unique(obs$cemetery_id)), x1)
    colnames(x1) <- c("cemetery_id", "graves_start")
    obs <- merge(obs[, colnames(obs) != "graves_start"], x1, by = "cemetery_id", x.all = TRUE)

    # Update new graves since the last observation
    obs <- obs[order(obs[, "cemetery_id"], obs[, "date"]), ]
    x1 <- c()
    for (i in sort(unique(obs$cemetery_id)) ) {
      x2 <- subset(obs, cemetery_id == i)
      x1 <- c(x1, NA, diff (x2$graves ) )

    }
    obs[, "new_graves"] <- x1


#.........................................................................................                            
### ENDS      