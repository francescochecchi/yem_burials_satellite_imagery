## Population mortality before and during crisis in Yemen: a geospatial and statistical analysis of cemetery data
### Explanation of R scripts and input datasets

-----------------------

10 February 2022

Francesco Checchi (francesco.checchi@lshtm.ac.uk)

Funding: United Kingdom Foreign, Commonwealth and Development Office

Collaborators: Satellite Applications Catapult, 1715 Labs

### Background
The London School of Hygiene and Tropical Medicine (LSHTM) has been conducting a project to estimate crisis- and COVID-19-attributable mortality in Yemen. This repository contains datasets and R scripts required to replicate the statistical analysis for one of the studies featured under this project. Specifically, the study sought to collect longitudinal data on burials in a nationally representative sample of cemeteries within Yemen over the period 2011 to 2021, so as to infer trends in overall population mortality and the effect of crisis conditions. The study relied heavily on geospatial analysis methods to source, prepare and collect data from very high-resolution satellite imagery: images and Python scripts for geospatial analysis steps are not included in this repository.

### Description of input datasets
The repository contains the following input data files:

* `yem_burials_data.xslx` , which contains datasets of all potential cemeteries identified, image quality scoring, metadata on subdistricts (administrative level 3) containing the cemeteries, population denominator data and several model predictor datasets, each in a separate tab. A variable dictionary is also provided and is read by the R script when selecting which datasets and columns to read in.
* `yem_burials_additional_datasets.zip`, which should be unzipped to the same directory as all other files / scripts. It contains the following:
  * a `health_data` subdirectory, which contains a shape file of crowd-sourced health facility locations, published by the Humanitarian OpenStreetMap Team;
  * a `transport_data` subdirectory, which contains a shape file of major roads in Yemen, published by Yemen's Central Statistical Office;
  * a `mapping` subdirectory, which contains UN OCHA shape files for administrative boundaries.

### Description of R scripts
The repository contains several R scripts, numbered in the order in which they should be run in order to replicate the analysis:
* `yem_burials_0_control_script.R` sets general parameters, loads or installs required packages, reads files and calls all other scripts;
* `yem_burials_0_user_functions.R` contains several user-defined functions used in later scripts;
* `yem_burials_1_prepare_data.R` performs cleaning and management for all input datasets;
* `yem_burials_2_describe_sample.R` generates statistics and graphs for the sample of cemeteries and subdistricts in this analysis; 
* `yem_burials_3_impute_graves.R` fits, validates and applies a generalised linear mixed model to impute missing data on incident burials;
* `yem_burials_4_analyse_patterns.R` prepares a single dataset for model fitting, graphs trends in burial and other key predictors, fits three alternative models to the data and generates related output graphs.

As R scripts are run, they produce intermediate and final output datasets and graphs, saved onto the working directory.

### How to replicate the analysis
Analysts should make sure all files, scripts and subdirectories are saved to the same folder, where output files will also appear. The directory for reading files is set automatically when `yem_burials_0_control_script.R` is run. An updated version of R software should be installed (https://www.r-project.org/). It is also recommended to run the code from the open-source RStudio interface (https://www.rstudio.com/products/rstudio/download/). Both R and RStudio are free and open-source.
