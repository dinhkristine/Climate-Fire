#### Load memories ---- 

## load all the packages  
source("release-R-files/libraries.R")

## load all the parameters 
source("release-R-files/parameters.R")


#### Load and prepare data ---- 

## Data intake, merging, and cleaning
## Only run if has nc temperature files and complete wild fire data
source("release-R-files/data-processing.R")

## Flatten data into single unit of analysis
source("release-R-files/data-flat-files.R")


#### Exploratory ---- 

source("release-R-files/exploratory.R")


#### Model development ---- 

## the 2nd order analysis 
source("release-R-files/2nd-order-analysis.R")

## building spatial and spatial-temporal models using GLM and GAM 
source("release-R-files/model-development.R")

## split models into seasons (Summer, Fall, Winter, Spring)
source("release-R-files/model-development-seasonal.R")