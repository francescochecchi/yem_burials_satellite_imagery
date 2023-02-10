#..........................................................................................
### ++++ GEOSPATIAL AND STATISTICAL ANALYSIS OF CEMETERY DATA IN YEMEN (2011-2021) ++++ ###
#..........................................................................................

#..........................................................................................
## ------------------ R SCRIPT TO SPECIFY BESPOKE ANALYSIS FUNCTIONS ------------------- ##
#..........................................................................................

                                          # Written by Francesco Checchi, LSHTM (Nov 2022)
                                          # francesco.checchi@lshtm.ac.uk 


#.........................................................................................
### Function to perform K-fold cross-validation and, if desired, plot predictions vs. observations for each fold
#.........................................................................................

f_cv <- function(f_fit, f_k_folds, f_plot) {
  
  #...................................  
  ## Access dataset used for fit 
  if (class(f_fit)[1] == "lm") {f_obs <- fit$data}
  if (typeof(f_fit) == "S4") {f_obs <- model.frame(fit)}
  if (class(f_fit) == "glmmTMB") {f_obs <- model.frame(fit)}
  if ("gamlss" %in% class(f_fit)) {
    f_obs <- model.frame(f_fit) ;
    colnames(f_obs) <- all.vars(formula(f_fit))
    }
 
    # If there is an offset, need to reconstitute it
    x1 <- grep("offset", colnames(f_obs), TRUE)
    if (length(x1) > 0) {
      x2 <- ! all.vars(formula(f_fit)) %in% colnames(f_obs)
      colnames(f_obs)[x1] <- all.vars(formula(f_fit))[x2]
      f_obs[, x1] <- exp(f_obs[, x1])
    }

  #...................................  
  ## Preparatory steps 
    # Select observations for which all the formula variables are non-missing
    f_obs <- f_obs[complete.cases(f_obs[, all.vars(formula(f_fit)) ] ), ] 
  
    # Determine dependent variable
    f_dep <- all.vars(formula(f_fit))[1]
  
    # Determine number of folds if f_k_folds = NA (i.e. LOOCV case)
    if (is.na(f_k_folds) == TRUE) { x1 <- nrow(f_obs) }
    if (is.na(f_k_folds) == FALSE) { x1 <- f_k_folds }
  
    # Is this a least-squares model?
    f_ols <- FALSE
    if (class(f_fit)[1] == "lm") {f_ols <- TRUE}
    if ( typeof(f_fit) == "S4") {f_ols <- ifelse("family" %in% names(f_fit@resp), FALSE, TRUE) }
    
    # Shuffle dataset
    f_obs <- f_obs[sample(nrow(f_obs), nrow(f_obs), replace=FALSE), ]
  
    # Split data into K folds
      # remove a few rows so as to come up with a n row divisible by K
      f_obs <- f_obs[1:(floor(nrow(f_obs)/x1) * x1), ]
      
      # split
      folds <- split(f_obs, (0:(nrow(f_obs)-1) %/% (nrow(f_obs)/x1)))

  #...................................  
  ## Fit model on all the unfolded sets and track square residuals of model fit on each fold, 
      # as well as predictions for each fold  
    # Vector to hold squared residuals and other statistics
    errors <- c()
    aic <- c()
    
    # Vector to hold observations and predictions for each fold
    observations <- c()
    predictions <- c()
    
  for (i in 1:length(folds) ) {	
    # Progress statement
    print(paste("now on fold ", i, " of ", length(folds), sep = "") )
    
    # Fit on all data but the fold
    data_now <- do.call("rbind", folds[-i])
    
    if (f_ols == FALSE) { cv.fit <- update(f_fit, formula=formula(f_fit),  family=family(f_fit)[[1]], data = data_now) }
    if (f_ols == TRUE) { cv.fit <- update(f_fit, formula=formula(f_fit),  data = data_now) }
    
    # Calculate squared residual of model when predicting fold data, add to other errors for single test observations
    x1 <- predict(cv.fit, newdata = folds[[i]], type = "response", allow.new.levels = TRUE)
    
    # Update output
    observations <- c(observations, folds[[i]][, f_dep])
    predictions <- c(predictions, x1)
    errors <- c(errors , (folds[[i]][, f_dep] -  x1 )^2 )
    aic <- c(aic, AIC(cv.fit))

  }

  #...................................  
  ## Return summary statistics and graphs
    # Return RMSE across all folds
    print("mean RMSE across all folds:")
    print(mean(errors, na.rm = TRUE)^0.5)
    
    # Return AIC across all folds
    print("mean AIC across all folds:")
    print(mean(aic, na.rm = TRUE))

    # If plot is desired...
    if (f_plot == TRUE) {
      # prepare data
      x1 <- as.data.frame(cbind(observations, predictions))
      colnames(x1) <- c("observations", "predictions")
        # if on log scale, back-transform to linear
        if (grepl("ln", f_dep) ) {x1[, c("observations", "predictions")] <- exp(x1[, c("observations", "predictions")]) }
  
      # plot
      plot <- ggplot(x1) +
        geom_point(aes(x = observations, y = predictions), size=2, colour = brewer_pal(palette = "Dark2")(2)[1]) + 
        theme_bw() +
        scale_x_continuous("observed") +
        scale_y_continuous("predicted") +  
        geom_abline(intercept = 0, slope = 1, colour = brewer_pal(palette = "Dark2")(2)[2] ) +
        theme(axis.title = element_text(colour="grey20")) +
       ggtitle(paste("accuracy of predictions on cross-validation; model to predict ", all.vars(formula(f_fit))[1] , sep="") ) +
       theme(plot.title = element_text(color = "grey20", size = 12, face = "plain") )
      
      print("plot shows accuracy of predictions on cross-validation")
      print(plot)
      
      # return plot data
      invisible(x1)
      
    }
}

     
#.........................................................................................
### Wrapper function to perform diagnostics and display fit results
#.........................................................................................

f_do <- function(f_fit, f_cv, f_k_folds, f_plot, f_do_cv) {
  #...................................      
  ## What kind of model is it?
    # Is it generalised linear?
    x1 <- TRUE
    if (class(f_fit) == "lm") {x1 <- FALSE}
    if ( typeof(f_fit) == "S4") {x1 <- ifelse("family" %in% names(f_fit@resp), TRUE, FALSE) }
    if ( class(f_fit) == "glmmTMB") {x1 <- ifelse(as.character((f_fit$modelInfo)$family)[1] == "gaussian", FALSE, TRUE) }
    
    # Is it mixed (i.e. does it include a random effect)?  
    x2 <- grepl("|", deparse1(formula(f_fit)) )
  
  #...................................      
  ## Fit model and print output 
  print(paste("#.......Output of ", ifelse(x1, "generalised ", ""),
    "linear ", ifelse(x2, "mixed ", ""), "model:" , ".............", sep = "") )
    
    # Summary
    print( summary(f_fit) )
    
    # Tidy summary (fixed effects only, exponentiated if generalised linear model)
    print( tidy(f_fit, conf.int = TRUE, exponentiate = x1, effects = "fixed") )
    print("/")
    
  #...................................      
  ## Do cross-validation if desired
  if (f_do_cv == TRUE) { 
    print("cross-validation:")
    out <- f_cv(f_fit, f_k_folds, f_plot)
    return(out)
    print("/")
  }
}

 
#.........................................................................................
### Function to fit a mixed general linear model and display clean results (using glmmMTB package)
#.........................................................................................

f_glmm <- function(f_dep, f_preds, f_reff, f_offset, f_data, f_family) {

  #...................................
  ## Preparatory steps
  
    # Specify the model formula
    form <- as.formula( paste(f_dep, " ~ ", paste(f_preds, collapse= " + "),
      ifelse(is.na(f_offset), "", paste(" + offset(log(", f_offset, ") )", sep = "") ), " + ", f_reff, sep="")  )

    # Select observations for which all the formula variables are non-missing or non-NA
    f_obs <- f_data[complete.cases(f_data[, all.vars(form) ] ), ]

  #...................................
  ## Fit model and return output
      
    # Fit model
    fit <- glmmTMB(form, data = f_obs, family = f_family)

    # Return fit
    return(fit)
}

 
#.........................................................................................
### Function to plot histograms of model variables
#.........................................................................................

f_hist <- function(f_var, f_data, f_lims) {

  plot <- ggplot(f_data)
  
  #...................................
  ## If the variable has >= 20 unique values... (is continuous)
  if (length(unique(na.omit(f_data[, f_var]))) >= 20) {
    plot <- plot + geom_histogram(aes(x = as.numeric(f_data[, f_var]) ), color="seagreen", fill="seagreen3" ) +
            theme_bw() + xlab(f_var) + scale_x_continuous(expand = c(0, 0), limits = f_lims )
  }
   
  #...................................
  ## Otherwise... (is categorical)
  if (length(unique(na.omit(f_data[, f_var]))) < 20) {
    plot <- plot + geom_histogram(aes(x = as.numeric(f_data[, f_var]) ), stat="count", color="seagreen", fill="seagreen3") +
            theme_bw() + xlab(f_var) + scale_x_continuous(expand = c(0, 0), limits = f_lims )
  }
          
  print(plot)
}


#.........................................................................................
### Function to plot burial rate ratios by cemetery and overall
#.........................................................................................

f_plot_burial_rr <- function(f_out, f_col_pts) {

  #...................................
  ## Prepare data for plotting
  
    # Select observations corresponding to final dates in each cemetery
    df <- by(f_out, f_out$cemetery_id, function(x) { 
      x[which.max(x$days_since), c("cemetery_id", "pred_cum", "cf_cum", "burial_rr", "wt")]})    
    df <- do.call(rbind, df)
    df <- data.frame(df)
  
    # Add mean over all cemeteries
    df$cemetery_id <- as.character(df$cemetery_id)
    df <- rbind(df, c("overall", sum(df$pred_cum), sum(df$cf_cum), sum(df$pred_cum) / sum(df$cf_cum), 2) )  
    df[, c("pred_cum", "cf_cum", "burial_rr", "wt")] <- 
      lapply(df[, c("pred_cum", "cf_cum", "burial_rr", "wt")], as.numeric)
    
  #...................................
  ## Plot
  plot <- ggplot(data = df, aes(x = cemetery_id, y = burial_rr, colour = cemetery_id, size = wt)) +
    geom_point(alpha = 0.6, shape = "square open", stroke = 3) +
    annotate("text", x = 1, y = df[df$cemetery_id == "overall", "burial_rr"], 
      label = round(df[df$cemetery_id == "overall", "burial_rr"], 1), color = "grey20", fontface = "bold") +
    theme_bw() +
    scale_x_discrete("cemetery", expand = c(0.03, 0)) +
    scale_colour_manual(values = c(palette_cb[3], rep(f_col_pts, nrow(df) - 1))) +
    scale_y_continuous("burial rate ratio (observed / counterfactual conditions)",
      breaks = seq(0, ceiling(max(df$burial_rr)), by = 0.5), limits = c(0, ceiling(max(df$burial_rr))),
      labels = paste(seq(0, ceiling(max(df$burial_rr)), by = 0.5), "x", sep = ""), expand = c(0, 0)) +
    scale_size(range = c(1, 10)) +
    geom_hline(yintercept = 1, size = 2, colour = palette_cb[2], alpha = 0.5) +
    theme(legend.position = "none",
      axis.title.x = element_text(color="grey20", size=11),
      axis.text.x = element_text(color = "grey20", size=10, angle=315, hjust=0, vjust=0),
      axis.line.y = element_line(color = "grey20"),
      axis.ticks.y = element_line(color = "grey20"),
      axis.text.y = element_text(color = "grey20", size=11),
      axis.title.y = element_text(color="grey20", margin = margin(r = 10), size=11 ),
      plot.margin = unit(c(0.5,2,0.5,0.5), "cm")
  )
  
  # Return plot
  return(plot)
}



#.........................................................................................
### Function to plot predictions versus observations for any model
#.........................................................................................

f_plot_pred_vs_obs <- function(f_out, f_col_pts, f_col_abline) {

  #...................................      
  ## Select plot parameters
  df <- subset(f_out, days_since > 0)
  lims <- range(df[, c("obs_link", "pred_link")])

  #...................................      
  ## Generate plot
  plot <- ggplot(data = df ) +
    geom_point(aes(x = obs_link, y = pred_link), size = 3, alpha = 0.5, colour = f_col_pts) +
    theme_bw() +
    scale_x_continuous("observed", limits = lims) +
    scale_y_continuous("predicted", limits = lims) +
    geom_abline(intercept = 0, slope = 1, colour = f_col_abline ) +
    theme(axis.title = element_text(colour="grey20")) +
    theme(plot.title = element_text(color = "grey20", size = 12, face = "plain") )
  print(plot)
  return(plot)
}


#.........................................................................................
### Function to plot predictions, observations and counterfactuals for any model, by cemetery
#.........................................................................................

f_plot_pred_vs_cf <- function(f_out, f_col_obs, f_col_pred, f_col_cf, f_dates_analysis) {
  plot <- ggplot(data = f_out) +
    geom_point(aes(x = date, y = obs_cum), colour = f_col_obs, alpha = 0.8, size = 2) +
    geom_line(aes(x = date, y = pred_cum), colour = f_col_pred, alpha = 0.5, size = 1.5) +
    # geom_ribbon(aes(x = date, ymin = pred_lci_cum, ymax = pred_uci_cum),
    #   fill = palette_cb[7], alpha = 0.2) +
    geom_line(aes(x = date, y = cf_cum), colour = f_col_cf, alpha = 0.5, size = 1.5) +
    # geom_ribbon(aes(x = date, ymin = cf_lci_cum, ymax = cf_uci_cum),
    #   fill = palette_cb[4], alpha = 0.2) +
    theme_bw() +
    facet_wrap(~cemetery_id, ncol = 6, scales = "free_y") +
    geom_vline(xintercept = f_dates_analysis[c(2,3)], colour = "grey20", linetype = "longdash") +
    # annotate(geom = "text", hjust = 0, x = date_knots + 30, y = c(1, 1), label = c("crisis", "pandemic")) +
    scale_x_date("year", minor_breaks = NULL, date_breaks = "1 year", date_labels = "%Y", expand = c(0, 0),
      limits = range(f_dates_analysis)) +
    scale_y_continuous("cumulative new graves") +
    theme(strip.text.x = element_text(color="grey20", size=11),
      axis.title.x = element_text(color="grey20", size=11),
      axis.text.x = element_text(color = "grey20", size=10, angle=315, hjust=0, vjust=0),
      axis.line.y = element_line(color = "grey20"),
      axis.ticks.y = element_line(color = "grey20"),
      axis.text.y = element_text(color = "grey20", size=11),
      axis.title.y = element_text(color="grey20", margin = margin(r = 10), size=11 ),
      plot.margin = unit(c(0.5,2,0.5,0.5), "cm")
  )
  return(plot)
}



#.........................................................................................
### Function to compute observations and predictions under observed and counterfactual conditions
#.........................................................................................

f_pred <- function(f_fit, f_outcome_fit, f_obs_fit, f_obs_cf, f_obs_model) {
  
  #...................................      
  ## Generate output dataframe
  x1 <- f_obs_fit[, c(f_outcome_fit, "days_since", "cemetery_id")] 
  x1[, "obs"] <- x1[, f_outcome_fit]
  if ("glmnet" %in% class(f_fit)) {if (family(f_fit) == "poisson") {x1[, "obs"] <- log(x1[, "obs"])} }
  
  #...................................      
  ## Predict under observed and counterfactual conditions, depending on the model type
    
    # Predict
    if ("ranger" %in% class(f_fit)) {
      x1[, "pred"] <- predict(f_fit, data = f_obs_fit)$predictions
      x2 <- abs(predict(f_fit, data = f_obs_fit, type = "se")$predictions)
      x1[, "cf"] <- predict(f_fit, data = f_obs_cf)$predictions
      x3 <- abs(predict(f_fit, data = f_obs_cf, type = "se")$predictions)
    }
    if ("mvr" %in% class(f_fit)) {
      nc <- data.frame(RMSEP(f_fit)$val)
      nc <- gsub(paste(f_outcome_fit, ".", sep = ""), "", names(which.min(nc["adjCV", ])) )
      nc <- as.integer(gsub(".comps", "", nc))
      x1[, "pred"] <- predict(f_fit, type = "response", ncomp = nc)
      x2 <- NA
      x1[, "cf"] <- predict(f_fit, newdata = f_obs_cf, type = "response", ncomp = nc)
      x3 <- NA
    }
    if ("glmnet" %in% class(f_fit)) {
      lambda <- f_fit$lambda[length(f_fit$lambda)]
      variables <- rownames(coef(fit_net, s = lambda))[-1]
      x1[, "pred"] <- as.vector(predict(f_fit, newx = data.matrix(f_obs_fit[, variables]), newoffset = log(f_obs_fit[, "days_since"]), 
        type = "link", s = lambda) )
      x2 <- NA
      x1[, "cf"] <- as.vector( predict(f_fit, newx = data.matrix(f_obs_cf[, variables]), newoffset = log(f_obs_cf[, "days_since"]), 
        type = "link", s = lambda))
      x3 <- NA
    }
    if ("bkmrfit" %in% class(f_fit)) {
      x1[, "pred"] <- predict(f_fit, ptype = "mean")
      x2 <- predict(f_fit, ptype = "sd.fit")
      x1[, "cf"] <- predict(f_fit, Znew = f_obs_cf[, colnames(f_fit$Z)], ptype = "mean" )
      x3 <- predict(f_fit, Znew = f_obs_cf[, colnames(f_fit$Z)], ptype = "sd.fit" )
    }  
    x1[, "pred_lci"] <- x1$pred - 1.96 * x2
    x1[, "pred_uci"] <- x1$pred + 1.96 * x2
    x1[, "cf_lci"] <- x1$cf - 1.96 * x3
    x1[, "cf_uci"] <- x1$cf + 1.96 * x3
  
    # Transform back to linear scale and compute predicted new graves
      # (but keep predictions in the original link metric, for plotting)
    x2 <- c("obs", "pred", "pred_lci", "pred_uci", "cf", "cf_lci", "cf_uci")
    x3 <- paste(c("obs", "pred", "cf"), "_link", sep = "")
    x1[, x3] <- x1[, c("obs", "pred", "cf")]
    if (any(class(f_fit)  %in% c("ranger", "mvr", "bkmrfit") ) ) {x1[, x2] <- exp(x1[, x2]) * x1$days_since}
    if ("glmnet" %in% class(f_fit)) {if (family(f_fit) == "poisson") {x1[, x2] <- exp(x1[, x2])} }
    
  #...................................      
  ## Compute cumulative new graves over time
    
    # Add dates
    x1 <- merge(x1, f_obs_model[, c("cemetery_id", "days_since", "date")], by = c("cemetery_id", "days_since"),
      all.x = TRUE)
    x1 <- x1[order(x1$cemetery_id, x1$date), ]

    # Cumulate observations, predictions and counterfactuals
    x2 <- by(x1, x1$cemetery_id, function(x) {data.frame(x$cemetery_id, x$date, cumsum(x$days_since), cumsum(x$obs),
      cumsum(x$pred), cumsum(x$pred_lci), cumsum(x$pred_uci),
      cumsum(x$cf), cumsum(x$cf_lci), cumsum(x$cf_uci))})
    x2 <- do.call(rbind, x2)
    colnames(x2) <- c("cemetery_id", "date", "days_since_cum", "obs_cum", "pred_cum", "pred_lci_cum", "pred_uci_cum",
      "cf_cum", "cf_lci_cum", "cf_uci_cum")
    x1 <- merge(x1, x2, by = c("cemetery_id", "date"), all.x = TRUE )

    # Add starting observations (== 0 at first date)
    x2 <- f_obs_model[f_obs_model$cemetery_id %in% unique(x1$cemetery_id) & f_obs_model$days_since == 0, ]
    x2 <- x2[, c("cemetery_id", "date")]
    x2[, c("days_since", "days_since_cum", f_outcome_fit, "obs", "obs_cum", "pred", "pred_lci", "pred_uci", "cf", "cf_lci", "cf_uci", 
      "pred_cum", "pred_lci_cum", "pred_uci_cum", "cf_cum", "cf_lci_cum", "cf_uci_cum", "obs_link", "pred_link", "cf_link")] <- 0
    x1 <- rbind(x1, x2)

    # Calculate ratio of cumulative predictions to counterfactuals = proxy of death rate relative risk
    x1[, "burial_rr"] <- x1$pred_cum / x1$cf_cum
    
    # Add model weights
    x1 <- merge(x1, unique(f_obs_fit[, c("cemetery_id", "wt")]), by = "cemetery_id", all.x = TRUE)
    
    # Return output
    x1 <- x1[order(x1$cemetery_id, x1$date), ]
    return(x1)
}





#.........................................................................................                            
### ENDS