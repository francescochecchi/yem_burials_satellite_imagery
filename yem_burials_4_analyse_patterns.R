#..........................................................................................
### ++++ GEOSPATIAL AND STATISTICAL ANALYSIS OF CEMETERY DATA IN YEMEN (2011-2021) ++++ ###
#..........................................................................................

#..........................................................................................
## ------------------ R SCRIPT TO FIT A PREDICTIVE MODEL TO THE DATA ------------------- ##
#..........................................................................................

                                          # Written by Francesco Checchi, LSHTM (Dec 2022)
                                          # francesco.checchi@lshtm.ac.uk 


#.........................................................................................      
### Describing trends in burial rate, by cemetery and overall
#.........................................................................................

  #...................................   
  ## Prepare data

    # Read prepared dataset
    obs <- read.csv("out_obs.csv")  
    obs$date <- as.Date.character(obs$date)        

    # Data needed
    x1 <- obs[is.na(obs$graves) == FALSE, c("date", "cemetery_id", "graves")]
    
    # Generate burial rate since the previous observation
    x1 <- x1[order(x1[, "cemetery_id"], x1[, "date"]), ]
    x2 <- c()
    for (i in sort(unique(x1$cemetery_id) ) ) {
      x3 <- subset(x1, cemetery_id == i)
      x2 <- c(x2, NA, diff(x3$graves) / as.integer(diff(x3$date)) )
    }
    x1[, "burial_rate"] <- x2
    
    # Lag burial rate so as to prepare data for step graph
    x2 <- c()
    for (i in sort(unique(x1$cemetery_id) ) ) {
      x4 <- subset(x1, cemetery_id == i)
      x2 <- c(x2, x4[-1, "burial_rate"], NA)  
    }

    x1[, "burial_rate_lag"] <- x2
    x1[, "burial_rate_lag"] <- ifelse(is.na(x1[, "burial_rate_lag"]), x1[, "burial_rate"], x1[, "burial_rate_lag"])

    # Burial rate as a multiple of the previous burial rate within the same cemetery
      # non-lagged
      x2 <- by(x1, x1$cemetery_id, function(x) {return(data.frame(as.character(x[, "cemetery_id"]), x[, "date"],
        x[, "burial_rate"] / x[2, "burial_rate"]))})
      x2 <- do.call(rbind, x2)
      colnames(x2) <- c("cemetery_id", "date", "burial_rate_rr")
      x1 <- merge(x1, x2, by = c("cemetery_id", "date"), all.x = TRUE)
    
      # lagged
      x2 <- by(x1, x1$cemetery_id, function(x) {return(data.frame(as.character(x[, "cemetery_id"]), x[, "date"],
        x[, "burial_rate_lag"] / x[1, "burial_rate_lag"]))})
      x2 <- do.call(rbind, x2)
      colnames(x2) <- c("cemetery_id", "date", "burial_rate_lag_rr")
      x1 <- merge(x1, x2, by = c("cemetery_id", "date"), all.x = TRUE)

    # Save as possoble outcomes for model
    obs_outcomes <- x1

        
  #...................................   
  ## Describe trends in burial rate over time
    # Number of burial rate observations (i.e. inter-image periods)
    nrow(subset(x1, ! is.na(burial_rate)))
          
    # Range and IQR of burial rate
    range(x1$burial_rate, na.rm = TRUE)
    quantile(x1$burial_rate, c(0.50, 0.25, 0.75), na.rm = TRUE)
    
      
  #...................................   
  ## Plot trends in burial rate over time, by cemetery
    
  plot <- ggplot(x1, aes(x = date, y = burial_rate_lag) ) +
    geom_step(size = 1, colour = palette_cb[4] ) +
    scale_y_continuous("mean new graves per day", limits = c(0, NA), expand = c(0.1, 0) ) +
    theme_bw() +
    annotate("rect", xmin = dates_analysis[1], xmax = dates_analysis[2], ymin = 0,
      ymax = Inf, alpha = 0.2, fill = palette_cb[4]) +
    annotate("rect", xmin = dates_analysis[2], xmax = dates_analysis[3], ymin = 0,
      ymax = Inf, alpha = 0.2, fill = palette_cb[2]) +
    annotate("rect", xmin = dates_analysis[3], xmax = dates_analysis[4], ymin = 0,
      ymax = Inf, alpha = 0.2, fill = palette_cb[7]) +
    geom_vline(xintercept = date_knots, colour = "grey20", linetype = "longdash") +
    scale_x_date("", minor_breaks=NULL, date_breaks = "1 year", date_labels = "%Y", expand = c(0, 0),
      limits = c(date_start, date_end)) +
    facet_wrap(~cemetery_id, ncol=4, scales = "free_y") +       
    theme(strip.text.x = element_text(color="grey20", size=11),
      axis.title.x = element_text(color="grey20", size=11), 
      axis.text.x = element_text(color = "grey20", size=10, angle=315, hjust=0, vjust=0),               
      axis.line.y = element_line(color = "grey20"),
      axis.ticks.y = element_line(color = "grey20"),
      axis.text.y = element_text(color = "grey20", size=11),
      axis.title.y = element_text(color="grey20", margin = margin(r = 10), size=11 ),
      plot.margin = unit(c(0.5,2,0.5,0.5), "cm")
    )
  plot

  ggsave("out_burial_rate_over_time_wide.png", width = 26, height = 18, units = "cm", dpi = "print")    
  ggsave("out_burial_rate_over_time_long.png", width = 25, height = 25, units = "cm", dpi = "print")    

      
  #...................................   
  ## Plot trends in burial rate over time, as a multiple of the first-observed baseline rate
      
  plot <- ggplot(x1, aes(x = date, y = burial_rate_lag_rr, group = cemetery_id) ) +
    geom_step(size = 1, colour = palette_cb[4], alpha = 0.5 ) +
    scale_y_continuous("relative change in burial rate (as a multiplier of first observation)", limits = c(0, max(x1$burial_rate_lag_rr + 2)), trans = "sqrt", expand = c(0, 0),
      breaks = c(0, 0.01, 0.1, 0.25, 0.50, 1, 2, 5, 10, 20), minor_breaks = NULL) +
    theme_bw() +
    geom_hline(size = 1.5, colour = palette_cb[7], yintercept = 1, alpha = 0.7) +
    # annotate("rect", xmin = dates_analysis[1], xmax = dates_analysis[2], ymin = 0,
    #   ymax = Inf, alpha = 0.1, fill = palette_cb[4]) +
    # annotate("rect", xmin = dates_analysis[2], xmax = dates_analysis[3], ymin = 0,
    #   ymax = Inf, alpha = 0.1, fill = palette_cb[2]) +
    # annotate("rect", xmin = dates_analysis[3], xmax = dates_analysis[4], ymin = 0,
    #   ymax = Inf, alpha = 0.1, fill = palette_cb[7]) +
    geom_vline(xintercept = date_knots, colour = "grey20", linetype = "longdash") +
    annotate(geom = "text", hjust = 0, x = date_knots + 30, y = c(28, 28), label = c("crisis", "pandemic")) +
    scale_x_date("year", minor_breaks=NULL, date_breaks="1 year", date_labels = "%Y", expand = c(0, 0),
      limits = c(date_start, date_end)) +
    theme(strip.text.x = element_text(color="grey20", size=11),
      axis.title.x = element_text(color="grey20", size=11), 
      axis.text.x = element_text(color = "grey20", size=11, hjust=0, vjust=0),               
      axis.line.y = element_line(color = "grey20"),
      axis.ticks.y = element_line(color = "grey20"),
      axis.text.y = element_text(color = "grey20", size=11),
      axis.title.y = element_text(color="grey20", margin = margin(r = 10), size=11 ),
      plot.margin = unit(c(0.5,2,0.5,0.5), "cm")
    )
  plot
  
  ggsave("out_burial_rel_change_over_time_wide.png", width = 25, height = 15, units = "cm", dpi = "print")    
  ggsave("out_burial_rel_change_over_time_long.png", width = 20, height = 15, units = "cm", dpi = "print")    


  #...................................   
  ## Visualise trends in population denominators and key predictors, by subdistrict

    # Select data for plotting
    x1 <- aggregate(obs[, c("pop_ipol", "prop_idp_ipol", "n_events", "n_fatalities")], 
      by = obs[, c("date", "subdis_pcode")], na.rm = TRUE, FUN = mean)
    colnames(x1) <- c("date", "subdis_pcode", "population", "prop_idp_ipol", "n_events", "n_fatalities")
    x1 <- merge(x1, ocha[, c("subdis_pcode", "subdis_en_ocha")], by = "subdis_pcode", all.x = TRUE)
  
    # Plot data; save population graph only
    for (i in c("population", "prop_idp_ipol", "n_events", "n_fatalities")) {
      x1[, "y"] <- x1[, i]  
      
      plot <- ggplot(data = x1, aes(x = date, y = y) ) +
      geom_step(colour = palette_cb[4], alpha= 0.6, size = 1.5) +
      scale_x_date("year", minor_breaks = NULL, date_breaks = "1 year", date_labels = "%Y", expand = c(0, 0),
        limits = c(date_start, date_end)) +
      scale_y_continuous(i, labels = comma) +
      theme_bw() +
      facet_wrap(~subdis_en_ocha, ncol = 2, scales = "free_y")
    
      print(plot)
      if (i == "population") 
      {ggsave("out_trends_population_subdistrict.png", width = 25, height = 15, units = "cm", dpi = "print") }
    }
    


#.........................................................................................                            
### Preparing dataset of inter-image observations for modelling
#.........................................................................................

  #...................................
  ## Prepare data

    # For the model, select only observations with non-missing graves
    obs_model <- subset(obs, !is.na(graves))
    nrow(obs_model)
    
    # Add subdistrict name
    obs_model <- merge(obs_model, ocha[, c("subdis_pcode", "subdis_en_ocha")], by = "subdis_pcode", all.x = TRUE)
    
    # Add outcomes
    obs_model <- merge(obs_model, obs_outcomes[, colnames(obs_outcomes) != "graves"], 
      by = c("cemetery_id", "date"), all.x = TRUE)
    
    # Calculate days since previous image date
    obs_model <- obs_model[order(obs_model[, "cemetery_id"], obs_model[, "date"]), ]
    x1 <- c()    
    for (i in sort(unique(obs_model$cemetery_id)) ) {
      x3 <- subset(obs_model, cemetery_id == i)
      x1 <- c(x1, 0, as.integer(diff(x3$date)) )
    }
    obs_model[, "days_since"] <- x1

    # Create inter-image period variable
    x1 <- by(obs_model, obs_model$cemetery_id, function(x) {data.frame(x$cemetery_id, x$days_since, 
      c(0:(nrow(x) -1)))})
    x1 <- do.call(rbind, x1)
    colnames(x1) <- c("cemetery_id", "days_since", "inter_period")
    obs_model <- merge(obs_model, x1, by = c("cemetery_id", "days_since"), all.x = TRUE)
    obs_model <- obs_model[order(obs_model[, "cemetery_id"], obs_model[, "date"]), ]
    
    # Attribute inter-image period variable to main observations database    
    obs <- merge(obs, obs_model[, c("cemetery_id", "date", "inter_period")], by = c("cemetery_id", "date"), all.x = TRUE)
    x1 <- by(obs, obs$cemetery_id, function(x){data.frame(x$cemetery_id, x$date, 
      na.approx(x$inter_period, rule = 1, na.rm = FALSE, method = "constant"))})
    x1 <- do.call(rbind, x1)
    colnames(x1) <- c("cemetery_id", "date", "inter_period_ipol")
    obs <- merge(obs, x1, by = c("cemetery_id", "date"), all.x = TRUE)
    

  #...................................   
  ## Generate population and proportion of IDPs since previous image, plus related predictors
  
    # Cumulate/average prop IDPs and population by cemetery and inter-image period (assume no insecurity before 2015)
    obs <- obs[order(obs$cemetery_id, obs$date), ]
    x1 <- subset(obs, !is.na(inter_period_ipol))
    x1 <- by(x1, x1[, c("cemetery_id", "inter_period_ipol")], function(x) {data.frame(x$cemetery_id, x$inter_period_ipol,
      x$date, sum(x$pop_ipol), mean(x$pop_ipol), mean(x$prop_idp_ipol) )})
    x1 <- do.call(rbind, x1)
    colnames(x1) <- c("cemetery_id", "inter_period_ipol", "date", "pop_ipol_cum", "pop_ipol_mean","prop_idp_ipol_mean")
    x1[, "pop_ipol_cum"] <- round(x1[, "pop_ipol_cum"], digits = 0)
    
    # Add population at end of time period (to calculate relative population size)
    x2 <- obs[obs$date == date_end, c("cemetery_id", "pop_ipol")]
    colnames(x2)[2] <- "pop_ipol_end"
    x1 <- merge(x1, x2, by = "cemetery_id", all.x = TRUE)
    
    # Merge back into model observations
    obs_model <- merge(obs_model, x1, by = c("cemetery_id", "date"), all.x = TRUE)
    
    # Generate relative population compared to end of time period
    obs_model[, "pop_change"] <- obs_model$pop_ipol_mean / obs_model$pop_ipol_end

                
  #...................................   
  ## Generate insecurity event and fatality numbers/rates since previous image
  
    # Cumulate events and fatalities by cemetery and inter-image period (assume no insecurity before 2015)
    obs <- obs[order(obs$cemetery_id, obs$date), ]
    x1 <- subset(obs, !is.na(inter_period_ipol))
    x1 <- by(x1, x1[, c("cemetery_id", "inter_period_ipol")], function(x) {data.frame(x$cemetery_id, x$inter_period_ipol,
      x$date, sum(x$n_events), sum(x$n_fatalities) )})
    x1 <- do.call(rbind, x1)
    colnames(x1) <- c("cemetery_id", "inter_period_ipol", "date", "n_events_cum", "n_fatalities_cum")

    # Merge back into model observations
    obs_model <- merge(obs_model, x1[, colnames(x1) != "inter_period_ipol"], by = c("cemetery_id", "date"), all.x = TRUE)

    # Compute events and fatality rate per 100,000 population
    obs_model[, "events_rate"] <- obs_model$n_events_cum * 100000 / obs_model$pop_ipol_cum
    obs_model[, "fatalities_rate"] <- obs_model$n_fatalities_cum * 100000 / obs_model$pop_ipol_cum
    
    # Compute events and fatalities per day
    obs_model[, "events_day"] <- obs_model$n_events_cum / obs_model$days_since
    obs_model[, "fatalities_day"] <- obs_model$n_fatalities_cum / obs_model$days_since
    
    
  #...................................   
  ## Generate mean wheat price since previous image
    # Mean wheat price by cemetery and inter-image period    
    obs <- obs[order(obs$cemetery_id, obs$date), ]
    x1 <- by(obs, obs[, c("cemetery_id", "inter_period_ipol")], function(x) {data.frame(unique(x$cemetery_id), 
      unique(x$inter_period_ipol), mean(x$usd_2011_ipol))})
    x1 <- do.call(rbind, x1)
    colnames(x1) <- c("cemetery_id", "inter_period_ipol", "mean_wheat_price")
      # merge back into model observations
      obs_model <- merge(obs_model, x1, by = c("cemetery_id", "inter_period_ipol"), all.x = TRUE)

    # Mean wheat price in the absence of a crisis (= counterfactual level) 
      # first, compute mean wheat price pre-crisis, by cemetery
      obs <- obs[order(obs$cemetery_id, obs$date), ]
      x1 <- subset(obs, date < dates_analysis[2])
      x1 <- by(x1, x1$cemetery_id, function(x) {data.frame(unique(x$cemetery_id),
        mean(x$usd_2011_ipol))})
      x1 <- do.call(rbind, x1)
      colnames(x1) <- c("cemetery_id", "base_mean_wheat_price")
      
      # then, attribute the pre-crisis baseline to all dates since the crisis started
      obs <- merge(obs, x1, by = "cemetery_id", all.x = TRUE)
      obs[, "cf_wheat_price"] <- ifelse(obs$date < dates_analysis[2], obs$usd_2011_ipol, obs$base_mean_wheat_price)

      # lastly, compute the mean counterfactual over each inter-image period
      obs <- obs[order(obs$cemetery_id, obs$date), ]
      x1 <- by(obs, obs[, c("cemetery_id", "inter_period_ipol")], function(x) {data.frame(unique(x$cemetery_id), 
        unique(x$inter_period_ipol), mean(x$cf_wheat_price))})
      x1 <- do.call(rbind, x1)
      colnames(x1) <- c("cemetery_id", "inter_period_ipol", "cf_mean_wheat_price")
        # merge back into model observations
        obs_model <- merge(obs_model, x1, by = c("cemetery_id", "inter_period_ipol"), all.x = TRUE)
    

  #...................................
  ## Calculate mean health facility density per 100,000 population, as a ratio to the end population in the series
  obs_model[, "hf_density"] <- obs_model$n_hf * 100000 / obs_model$pop_ipol_end
    
  
  #...................................   
  ## Generate proportion of period spent in pandemic
  obs <- obs[order(obs$cemetery_id, obs$date), ]
  obs$period_covid <- factor(obs$period_covid)
  x1 <- by(obs, obs[, c("cemetery_id", "inter_period_ipol")], function(x) {data.frame(unique(x$cemetery_id), 
    unique(x$inter_period_ipol), prop.table(table(x$period_covid))["yes"])})
  x1 <- do.call(rbind, x1)
  colnames(x1) <- c("cemetery_id", "inter_period_ipol", "prop_period_covid")

    # Merge back into model observations
    obs_model <- merge(obs_model, x1, by = c("cemetery_id", "inter_period_ipol"), all.x = TRUE)

    
  #...................................   
  ## Compute new graves since previous observation (one of the two possible model outcomes)
    
    # Transform graves by offsetting all cemeteries by the starting number of graves
    if (graves_offset == TRUE) {obs_model[, "graves"] <- obs_model[, "graves"] - obs_model[, "graves_start"]}

    # Calculate new graves since the previous observation
    x1 <- by(obs_model, obs_model$cemetery_id, function(x) {data.frame(x$cemetery_id, x$date,
      c(NA, diff(x$graves)))} )
    x1 <- do.call(rbind, x1)  
    colnames(x1) <- c("cemetery_id", "date", "new_graves")
    obs_model <- merge(obs_model[, colnames(obs_model) != "new_graves"], x1, by = c("cemetery_id", "date"), all.x = TRUE)  
          

#.........................................................................................                            
### Carrying out pre-processing steps ahead of fitting models
#.........................................................................................

  #...................................
  ## Specify variables

    # Possible outcomes
    outcomes <- c("burial_rate", "new_graves")
    
    # Possible exposures: insecurity, wheat price, proportion of IDPs
    exposures <- c("events_rate", "events_day", "fatalities_rate", "fatalities_day", 
      "mean_wheat_price", "prop_idp_ipol_mean")
    
    # Possible covariates
    covariates <- c("graves_start", "hf_density", "image_score", "infilling", 
      "pop_change", "prop_period_covid", "region", "road_density", "u5mr", "urban_rural")
    
    # Random effect
    ranef <- "cemetery_id"
    
    # Offset
    offset <- "days_since"

  #...................................
  ## Select data and pre-process them
        
    # Select data
    obs_fit <- obs_model[, c(outcomes, exposures, covariates, offset, ranef)]
    obs_fit <- na.omit(obs_fit)
    nrow(obs_fit)
    
    # Create a cemetery weight variable = proportion of all burials accounted for by any given cemetery
      # so as to improve fit in the cemeteries with highest number of burials
    x1 <- aggregate(obs_fit[, "new_graves"], by = list(obs_fit$cemetery_id), FUN = sum)
    colnames(x1) <- c("cemetery_id", "new_graves_cum")
    x1[, "wt"] <- x1$new_graves_cum / sum(x1$new_graves_cum)
    obs_fit <- merge(obs_fit, x1[, c("cemetery_id", "wt")], by = "cemetery_id", all.x = TRUE)
      # log the weight to make it more moderate
      obs_fit$wt <- sqrt(obs_fit$wt)
    
    # Log burial rate outcome
    obs_fit[, outcomes[1]] <- log(obs_fit[, outcomes[1]] + 0.0001)
    
    # Center and scale continuous exposures and covariates
      # which variables are continuous (numeric)
      x1 <- unlist(lapply(obs_fit[, c(exposures, covariates)], is.numeric), use.names = FALSE)
      x1 <- colnames(obs_fit[, c(exposures, covariates)])[x1]
      
      # center (i.e. substract the mean from each observation) and scale (i.e. divide by the SD) each
      for (i in x1) { obs_fit[, i] <- scale(obs_fit[, i], scale = TRUE) }

      # store means and SDs used in centering/scaling (for counterfactual setting)
      cs_par <- data.frame("variable" = x1, "mean" = NA, "sd" = NA)
      for (i in x1) { cs_par[cs_par$variable == i, c("mean", "sd")] <- 
        attributes(obs_fit[, i])[c(2,3)] }  
      
    # Transform all continuous exposures and covariates into quartiles
    for (i in x1) {obs_fit[, paste(i, "_cat", sep = "")] <- dplyr::ntile(obs_fit[, i], 4)}
    obs_fit[, paste(x1, "_cat", sep = "")] <- lapply(obs_fit[, paste(x1, "_cat", sep = "")], factor) 
      # store quartile boundaries of mean wheat price (for counterfactual setting)    
      mean_wheat_price_qts <- data.frame("level" = levels(obs_fit$mean_wheat_price_cat), "min" = NA, "max" = NA)
      for (i in 1:nrow(mean_wheat_price_qts)) {
        mean_wheat_price_qts[i, c("min", "max")] <- 
          range(obs_fit[obs_fit$mean_wheat_price_cat == mean_wheat_price_qts[i, "level"], "mean_wheat_price"])
      }
    
    # Convert all character variables into factors
    obs_fit[, sapply(obs_fit, is.character)] <- lapply(obs_fit[, sapply(obs_fit, is.character)], factor)
      
  #...................................
  ## Generate counterfactual dataset

    # Create counterfactual dataset
    obs_cf <- obs_fit

    # Set counterfactual values
      # no insecurity events and deaths, no displacement
      for (i in c("events_rate", "events_day", "fatalities_rate", "fatalities_day", "prop_idp_ipol_mean")) {
        obs_cf[, i] <- (0 - cs_par[cs_par$variable == i, "mean"]) / cs_par[cs_par$variable == i, "sd"]
        obs_cf[, paste(i, "_cat", sep = "")] <- levels(obs_cf[, paste(i, "_cat", sep = "")])[1]
      }

      # wheat price at mean counterfactual level
      obs_cf <- merge(obs_cf, obs_model[, c("cemetery_id", "days_since", "cf_mean_wheat_price")],
        by = c("cemetery_id", "days_since"), all.x = TRUE)
      obs_cf$mean_wheat_price <- (obs_cf$cf_mean_wheat_price - 
        cs_par[cs_par$variable == "mean_wheat_price", "mean"]) / cs_par[cs_par$variable == "mean_wheat_price", "sd"]
      x1 <- c()
      for (i in 1:nrow(mean_wheat_price_qts)) {
        x1 <- cbind(x1, (obs_cf$mean_wheat_price >= mean_wheat_price_qts[i, "min"]  &
            obs_cf$mean_wheat_price <= mean_wheat_price_qts[i, "max"]) )
      }
      obs_cf$mean_wheat_price_cat <- levels(obs_cf$mean_wheat_price_cat)[apply(x1, 1, which)]
   
    # Convert all character variables into factors and retain original levels
    obs_cf[, sapply(obs_cf, is.character)] <- lapply(obs_cf[, sapply(obs_cf, is.character)], factor)
    for (i in grep("_cat", colnames(obs_cf), value = TRUE)) {
      levels(obs_cf[, i]) <- levels(obs_fit[, i])
    }

  #...................................   
  ## Explore univariate correlations for continuous variables, with burial rate as the dependent
  
    # Select data for exploration    
      # which variables are continuous (numeric)
      x1 <- unlist(lapply(obs_fit[, c(exposures, covariates)], is.numeric), use.names = FALSE)
      x1 <- colnames(obs_fit[, c(exposures, covariates)])[x1]

      # select data
      x2 <- obs_fit[, c("burial_rate", x1)]
      
    # Correlation plots for each continuous variable, with log(burial rate) as the outcome
    for (i in x1) {
        x2[, "x"] <- x2[, i]
        plot <- ggplot(data = x2, aes(x = x, y = burial_rate)) + geom_jitter() +
          scale_x_continuous(i, limits = quantile(x2$x, c(0.025, 0.975))) + 
          geom_smooth(method="loess") + theme_bw()
        print(plot)
    }  
      # which variables should be treated as categorical (quartiles)
      preds_cat <- c("u5mr", "pop_change", "hf_density", "graves_start", 
        "events_rate", "events_day", "fatalities_rate", "fatalities_day")
      
    # Correlations between pairs of predictors (= expected collinearity)
    ggcorr(data = obs_fit[, x1])

    
#.........................................................................................                            
### Fitting a Random Forest regression to the continuous burial rate
#.........................................................................................
    
  #...................................
  ## Specify model preferences

    # Outcome
    outcome_fit <- outcomes[1]
    
    # Exposures
    exposures_fit <- c("events_day", "mean_wheat_price", "prop_idp_ipol_mean")
    
    # Covariates
    covariates_fit <- covariates
    
    
  #...................................
  ## Fit a random forest model
    
    # Specify formula
    form <- as.formula(paste(outcome_fit, " ~ ", paste(c(exposures_fit, covariates_fit), collapse = " + "), sep = ""))
    
    # Grow forest
    fit_rf <- ranger(formula = form, data = obs_fit[, c(outcome_fit, exposures_fit, covariates_fit)],
      keep.inbag = TRUE, num.trees = 1000, min.node.size = 2, importance = "impurity", case.weights = obs_fit$wt)
    fit_rf
    
    # Visualise variable importance
    importance(fit_rf)

  #...................................
  ## Compare observations and predictions under observed and counterfactual conditions

    # Compute observations and predictions under observed and counterfactual conditions
    out_rf <- f_pred(fit_rf, outcome_fit, obs_fit, obs_cf, obs_model)
    write.csv(out_rf, "out_predictions_rf.csv", row.names = FALSE)
        
    # Plot predictions versus observations
    plot_rf1 <- f_plot_pred_vs_obs(out_rf, palette_cb[7], palette_cb[7])
    ggsave("out_burial_rate_prediction_rf.png", width = 18, height = 12, units = "cm", dpi = "print")
      
    # Plot observations, predictions and counterfactuals, by cemetery
    plot <- f_plot_pred_vs_cf(out_rf, palette_cb[7], palette_cb[7], palette_cb[4], dates_analysis)
    plot    
    ggsave("out_preds_and_cf_rf_long.png", width = 26, height = 35, units = "cm", dpi = "print")
    ggsave("out_preds_and_cf_rf_wide.png", width = 35, height = 23, units = "cm", dpi = "print")

    # Plot relative burial rates across cemeteries and overall
    plot_rf2 <- f_plot_burial_rr(out_rf, palette_cb[7])
    plot_rf2
    ggsave("out_burial_rr_rf_long.png", width = 20, height = 20, units = "cm", dpi = "print")
    ggsave("out_burial_rr_rf_wide.png", width = 25, height = 13, units = "cm", dpi = "print")


#.........................................................................................                            
### Fitting an elastic net Poisson regression model to new graves
#.........................................................................................
  # Also see https://glmnet.stanford.edu/articles/glmnet.html 
    
  #...................................
  ## Specify model preferences

    # Outcome
    outcome_fit <- outcomes[2]
      # add 1 grave to observations to avoid infinite predictions          
      obs_fit[, outcome_fit] <- obs_fit[, outcome_fit] + 1

    # Exposures
    exposures_fit <- c("events_day", "mean_wheat_price", "prop_idp_ipol_mean")
      # would be best to model these as categorical, but LASSO is not suited for categorical variables
      # GLM (Poisson) should be fairly robust to non-linear relationships
    
    # Covariates
    covariates_fit <- covariates    
    
  #...................................
  ## Fit an elastic net model (i.e. midway between LASSO and Ridge regression)
    
    # # Specify formula
    # form <- as.formula(paste(outcome_fit, " ~ ", paste(c(exposures_fit, covariates_fit), collapse = " + "), sep = ""))
    
    # Fit model for candidate values of alpha (1 = LASSO, 0 = ridge), track optimal lambda value 
      # and percent deviance explained for the best lambda under the given alpha value
    x1 <- data.frame("alpha" = seq(0, 1, 0.1), "lambda" = NA, "percent_deviance" = NA)
    for (i in 1:nrow(x1)) {
      # fit model
      fit_net <- glmnet(y = obs_fit[, outcome_fit], x = obs_fit[, c(exposures_fit, covariates_fit)],
        offset = log(obs_fit[, offset]), weights = obs_fit$wt, standardize = FALSE, family = "poisson",
        alpha = x1[i, "alpha"])
      
      # report best lambda and deviance explained
      x1[i, "lambda"] <- fit_net$lambda[length(fit_net$lambda)]
      x1[i, "percent_deviance"] <- fit_net$dev.ratio[length(fit_net$dev.ratio)]
    }
    
    # Select optimal alpha value and refit model with it
    alpha <- x1[which.max(x1$percent_deviance), "alpha"]
    fit_net <- glmnet(y = obs_fit[, outcome_fit], x = obs_fit[, c(exposures_fit, covariates_fit)],
      offset = log(obs_fit[, offset]), weights = obs_fit$wt, standardize = FALSE, family = "poisson",
      alpha = alpha)
      # track optimal lambda value
      lambda <- fit_net$lambda[length(fit_net$lambda)]
    
    # Visualise trajectory of coefficients (shrinkage = Ridge or vice versa = LASSO?)
    plot(fit_net)
    
    # Print model coefficients at optimal lambda value
    coef(fit_net, s = lambda)
      # difficult to interpret on their own

  #...................................
  ## Compare observations and predictions under observed and counterfactual conditions
    
    # Compute observations and predictions under observed and counterfactual conditions
    out_net <- f_pred(fit_net, outcome_fit, obs_fit, obs_cf, obs_model)
    write.csv(out_net, "out_predictions_net.csv", row.names = FALSE)
        
    # Plot predictions versus observations
    plot_net1 <- f_plot_pred_vs_obs(out_net, palette_cb[8], palette_cb[7])
    ggsave("out_burial_rate_prediction_net.png", width = 18, height = 12, units = "cm", dpi = "print")
      
    # Plot observations, predictions and counterfactuals, by cemetery
    plot <- f_plot_pred_vs_cf(out_net, palette_cb[8], palette_cb[8], palette_cb[4], dates_analysis)
    plot    
    ggsave("out_preds_and_cf_net_long.png", width = 26, height = 35, units = "cm", dpi = "print")
    ggsave("out_preds_and_cf_net_wide.png", width = 35, height = 23, units = "cm", dpi = "print")

    # Plot relative burial rates across cemeteries and overall
    plot_net2 <- f_plot_burial_rr(out_net, palette_cb[8])
      # fix y axis scale
      x1 <- 60 # ceiling(max(out_net$burial_rr, na.rm = TRUE))
      x2 <- c(seq(0, 1.5, by = 0.5), seq(2, 3, by = 1), seq(4, 10, by = 2), seq(15, 30, by = 5), seq(40, x1, by = 10))
    plot_net2 <- plot_net2 + scale_y_continuous("burial rate ratio (observed / counterfactual conditions)",
      breaks = x2, limits = c(0, ceiling(x1)),
      labels = paste(x2, "x", sep = ""), trans = "pseudo_log", expand = c(0, 0.1))
    plot_net2
    ggsave("out_burial_rr_net_long.png", width = 20, height = 20, units = "cm", dpi = "print")
    ggsave("out_burial_rr_net_wide.png", width = 25, height = 13, units = "cm", dpi = "print")
    
    # Reconstitute dataset
    obs_fit[, outcome_fit] <- obs_fit[, outcome_fit] - 1

    # Combined graph with random forest results
    plot <- ggarrange(
      NULL, 
      NULL, 
      plot_rf1 + theme(plot.margin = margin(0,15,1,1, "pt")), 
      plot_rf2 + theme(plot.margin = margin(0,15,1,1, "pt")) + scale_x_discrete("cemetery", expand = c(0.05, 0)), 
      NULL, 
      NULL, 
      plot_net1 + theme(plot.margin = margin(0,15,1,1, "pt")), 
      plot_net2 + theme(plot.margin = margin(0,15,1,1, "pt")) + scale_x_discrete("cemetery", expand = c(0.05, 0)), 
      ncol = 2, nrow = 4, widths = c(1, 2.5), heights = c(0.1, 1, 0.1, 1),
      labels = c("", "", "A: Random forest", "", "", "", "B: Elastic net regression", ""), 
      font.label = list(size = 11), vjust = -0.5, align = "v", label.x = c(0, 0, -0.1, 0, 0, 0, -0.2, 0) )
    plot
    ggsave("out_burial_combi_long.png", width = 28, height = 28, units = "cm", dpi = "print")
    ggsave("out_burial_combi_wide.png", width = 40, height = 25, units = "cm", dpi = "print")
    
    
#.........................................................................................                            
### Fitting a Bayesian kernel machine regression (BKMR) to the continuous burial rate
#.........................................................................................
  # Also see https://bookdown.org/andreabellavia/mixtures/bayesian-kernel-machine-regression.html#bkmr-extensions,
    # https://jenfb.github.io/bkmr/overview.html
    
  #...................................
  ## Specify model preferences

    # Outcome
    outcome_fit <- outcomes[1]

    # Exposures
    exposures_fit <- c("events_day", "mean_wheat_price", "prop_idp_ipol_mean")

    # Covariates
    covariates_fit <- covariates    

  #...................................
  ## Fit and visualise BKMR model (adapted from https://jenfb.github.io/bkmr/overview.html )

    # Fit model
    fit_km <- kmbayes(y = obs_fit[, outcome_fit], Z = obs_fit[, exposures_fit], 
      X = data.matrix(obs_fit[, covariates_fit]), group = c(1, 2, 2),
      id = obs_fit[, ranef], iter = 10000, verbose = FALSE, varsel = TRUE)    
    summary(fit_km)

    # Graph convergence of model parameters over iterations
    TracePlot(fit = fit_km, par = "beta")          
    TracePlot(fit = fit_km, par = "sigsq.eps")
    TracePlot(fit = fit_km, par = "r")
    
    # Generate univariate relationships between each exposure and the outcomes, while holding other exposures fixed
      # (default = fixed at their 50th percentile)
    pred_resp_univar <- PredictorResponseUnivar(fit = fit_km)
    
    # Plot univariate exposure-outcomes functions
    x1 <- c("events per day", "wheat price", "IDP proportion")
    names(x1) <- exposures_fit
    plot <- ggplot(pred_resp_univar, aes(x = z, y = est, ymin = est - 1.96 * se, ymax = est + 1.96 * se)) + 
      geom_smooth(stat = "identity", colour = palette_cb[6], fill = palette_cb[6]) + 
      facet_wrap(~ variable, scales = "free_x", labeller = labeller(variable = x1)) +
      theme_bw() +
      ylab("univariate change in the burial rate as a function of the exposure") +
      xlab("level of the exposure (centred and scaled)") +
      theme(strip.text = element_text(size = 11) )
    plot
    ggsave("out_bkrm_univariate_effects.png", width = 25, height = 15, units = "cm", dpi = "print")
    
    # # Compute and plot overall effect of the exposures when they are at a given percentile, 
    #   # compared to when they are at their 50th percentile    
    # risks_overall <- OverallRiskSummaries(fit = fit_km, y = obs_fit[, outcome_fit], Z = obs_fit[, exposures_fit], 
    #   X = data.matrix(obs_fit[, covariates_fit]), qs = seq(0.0, 1, by = 0.10), q.fixed = 0.5, method = "exact")
    # risks_overall
    # ggplot(risks_overall, aes(quantile, est, ymin = est - 1.96*sd, ymax = est + 1.96*sd)) + 
    #   geom_pointrange()
    
  #...................................
  ## Compare observations and predictions under observed and counterfactual conditions
    
    # Compute observations and predictions under observed and counterfactual conditions
    out_km <- f_pred(fit_km, outcome_fit, obs_fit, obs_cf, obs_model)
    write.csv(out_km, "out_predictions_km.csv", row.names = FALSE)
        
    # Plot predictions versus observations
    plot_km1 <- f_plot_pred_vs_obs(out_km, palette_cb[6], palette_cb[7])
    ggsave("out_burial_rate_prediction_km.png", width = 18, height = 12, units = "cm", dpi = "print")
      
    # Plot observations, predictions and countenetactuals, by cemetery
    plot <- f_plot_pred_vs_cf(out_km, palette_cb[6], palette_cb[6], palette_cb[4], dates_analysis)
    plot    
    ggsave("out_preds_and_cf_km_long.png", width = 26, height = 35, units = "cm", dpi = "print")
    ggsave("out_preds_and_cf_km_wide.png", width = 35, height = 23, units = "cm", dpi = "print")

    # Plot relative burial rates across cemeteries and overall
    plot_km2 <- f_plot_burial_rr(out_km, palette_cb[6])
    plot_km2
    ggsave("out_burial_rr_km_long.png", width = 20, height = 20, units = "cm", dpi = "print")
    ggsave("out_burial_rr_km_wide.png", width = 25, height = 13, units = "cm", dpi = "print")


        
#.........................................................................................
### ENDS
#.........................................................................................
