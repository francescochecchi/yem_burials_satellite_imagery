#..........................................................................................
### ++++ GEOSPATIAL AND STATISTICAL ANALYSIS OF CEMETERY DATA IN YEMEN (2011-2021) ++++ ###
#..........................................................................................

#..........................................................................................
## ------------ R SCRIPT TO CALL OTHER SCRIPTS AND CARRY OUT ANALYSIS STEPS ------------ ##
#..........................................................................................

                                          # Written by Francesco Checchi, LSHTM (Dec 2022)
                                          # francesco.checchi@lshtm.ac.uk 


#..........................................................................................
### Preparatory steps
#.........................................................................................

  #...................................      
  ## Install or load required R packages
    
    # List of required packages
    x1 <- c("bkmr", "bkmrhat", "broom.mixed", "car", "contrast", "GGally", "ggbreak", "ggplot2", "ggpubr", 
      "glmmTMB", "glmnet", "jtools", "lme4", "lubridate", "MASS", "pls", "ranger", "RColorBrewer","readxl", 
      "scales", "sf", "tmap", "TMB", "zoo")
    
    # Install any packages not yet installed
    x2 <- x1 %in% row.names(installed.packages())
    if (any(x2 == FALSE)) { install.packages(x1[! x2]) }
    
    # Load all packages    
    lapply(x1, library, character.only = TRUE)
    
  #...................................      
  ## Starting steps

    # Clean up from previous code / runs
    rm(list=ls(all=TRUE) )
  
    # Set font
    windowsFonts(Arial=windowsFont("Arial"))

    # Set working directory to where this file is stored
    current_path = rstudioapi::getActiveDocumentContext()$path 
    setwd(dirname(current_path ))
    print( getwd() )
    
    # Initialise random numbers
    set.seed(123)
    
    # Colour-blind palette for graphing
    palette_cb <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
    

#.........................................................................................
### Specifying parameters
#.........................................................................................
    
    # # Time window for rolling means of predictors (in days)
    # roll <- 28
        
    # Dates at which baseline period starts, crisis period starts / COVID-19 starts / analysis ends
    dates_analysis <- as.Date(c("1Jan2011", "1Jun2014", "1Apr2020", "31Dec2021"), "%d%b%Y")
      
      # dates at which period changes
      date_knots <- dates_analysis[c(2, 3)]
 
      # start and end of analysis period
      date_start <- dates_analysis[1]
      date_end <- dates_analysis[length(dates_analysis)]

    # Number of folds for cross-validation
    k_folds <- NA   # NA means leave-one-out cross-validation
    
    # Whether an offset should be applied to the number of graves when fitting models (offset = starting number of graves)
    graves_offset <- TRUE
    
    # Number of bootstrap replicates for confidence interval estimation
    n_boot <- 1000
  

#.........................................................................................
### Specifying bespoke functions
#.........................................................................................
    
source("yem_burials_0_bespoke_functions.R")
        
    
#.........................................................................................
### Reading in required files
#.........................................................................................

  #...................................      
  ## Variable dictionary
  dict <- read_excel("yem_burials_data.xlsx", sheet = "dictionary")
    # remove tibble
    dict <- as.data.frame(dict)

  #...................................      
  ## Read in all the datasets needed
    # which datasets
    x1 <- c("obs", "cemeteries", "subdistricts", "ocha", "pop", "u5mr", "acled", "wheat", "inflation")
    
    # for each dataset...
    for (i in x1) {
      # read in
      assign(i, read_excel("yem_burials_data.xlsx", sheet = i) )
        # remove tibble
        assign(i, as.data.frame(get(i)) )
      # only keep needed columns
      x2 <- subset(dict, sheet == i)[, "use"]
      x2 <- which(! x2 %in% c("no") )
      x3 <- get(i)[, x2]
      assign(i, x3)
      rm(x3)
    }
    
  #...................................      
  ## Read in various shape files
    
    # OCHA/CSO shape file for sub-districts
    dir_maps <- paste(getwd( ), "/mapping", sep = "")
    ocha_shape <- sf::st_read(paste(dir_maps, "/yem_admbnda_adm3_govyem_cso_20191002.shp", sep = ""))
    
    # OCHA/CSO shape file for districts
    dir_maps <- paste(getwd( ), "/mapping", sep = "")
    ocha_shape_dis <- sf::st_read(paste(dir_maps, "/yem_admbnda_adm2_govyem_cso_20191002.shp", sep = ""))
    
    # OCHA/CSO shape file for governorates
    dir_maps <- paste(getwd( ), "/mapping", sep = "")
    ocha_shape_gov <- sf::st_read(paste(dir_maps, "/yem_admbnda_adm1_govyem_cso_20191002.shp", sep = ""))
    
    # OpenStreetMap shape file for location of health facilities
    dir_maps <- paste(getwd( ), "/health_data", sep = "")
    health_shape <- sf::st_read(paste(dir_maps, "/hotosm_yem_health_facilities_points.shp", sep = ""))
    
    # Shape file for road network
    dir_maps <- paste(getwd( ), "/transport_data", sep = "")
    road_shape <- sf::st_read(paste(dir_maps, "/Ymn-Roads.shp", sep = ""))
      
    
#.........................................................................................                            
### Preparing the data for analysis
#.........................................................................................
    
source("yem_burials_1_prepare_data.R")

            
#.........................................................................................      
### Describing the composition of the final sample [NO DEPENDENCY WITH SUBSEQUENT SCRIPTS]
#.........................................................................................
    
source("yem_burials_2_describe_sample.R")      

    
#.........................................................................................      
### Imputing missing burial count data
#.........................................................................................
   
source("yem_burials_3_impute_graves.R")                         
    
  # Write dataset
  write.csv(obs, "out_obs.csv", row.names = FALSE)
    

#.........................................................................................      
### Analysing burial patterns [IF SCRIPTS 1 AND 2 HAVE BEEN RUN ONCE, CAN SKIP TO HERE]
#.........................................................................................

source("yem_burials_4_analyse_patterns.R") 
  

#.........................................................................................
### ENDS
#.........................................................................................



  