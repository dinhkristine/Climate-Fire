#### load packages ----- 

library(tidyverse)
library(magrittr)
library(raster)
library(sp)
library(tigris)


#### parameters ---- 

my_crs <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

min_year <- 2000 

max_year <- 2015 


#### load CA tracts with countries ---- 

CA_tracts <-  counties(state = "CA", resolution = "5m")


#### Load fire data ---- 

fire_data <- read.csv(paste0("data/fires/us_fires_1.csv"))

for (i in 2:7){
  fire <- read.csv(paste0("data/fires/us_fires_", i, ".csv"))
  fire_data <- rbind(fire_data, fire)
}

## filter data to min and max year range 

fire_data %<>% 
  filter(fire_year %in% c(min_year:max_year) & state == "CA")

## rename long and lat 

fire_data %<>% rename(x = longitude, y =  latitude)


## convert fire data into spatialdataframe

coordinates(fire_data) <- c("x", "y")

proj4string(fire_data) <- CRS(my_crs)


## add county tract in fire data 

CA_fire <- sp::over(fire_data, CA_tracts)

fire_data@data %<>% 
  cbind(COUNTYFYP = CA_fire$COUNTYFP)


#### Load MIN temp data ---- 

min_temp <- brick("data/temp/MINT.nc")

## rotate long and lat to have -180 to 180 instead of -360 to 360  

min_rotate <- rotate(min_temp)

## convert raster into dataframe 

min <- data.frame(rasterToPoints(min_rotate))

## transform 

min %<>% gather("date", "temp", -x, -y)

## edit date 

min$date %<>% 
  gsub("X", "", .) %>% 
  lubridate::as_date()

min$year <- lubridate::year(min$date)

## filter to year range 

min %<>% filter(year %in% c(min_year:max_year))

## convert to spatial dataframe

coordinates(min) <- c("x", "y")

proj4string(min) <- CRS(my_crs)

## add CA tract into min temp

CA_temp <- sp::over(min, CA_tracts)

min@data %<>% 
  cbind(COUNTYFYP = CA_temp$COUNTYFP)
 
min@data$COUNTYFYP %<>% as.character()

CA_min_temp <- min@data %>% 
  group_by(date, COUNTYFYP) %>% 
  summarise(avg_min_temp = median(temp))


#### Load MAX temp data ---- 

max_temp <- brick("data/temp/MAXT.nc")

## rotate long and lat to have -180 to 180 instead of -360 to 360  

max_rotate <- rotate(max_temp)

## convert raster into dataframe 

max <- data.frame(rasterToPoints(max_rotate))

## transform 

max %<>% gather("date", "temp", -x, -y)

## edit date 

max$date %<>% 
  gsub("X", "", .) %>% 
  lubridate::as_date()

max$year <- lubridate::year(max$date)

## filter to year range 

max %<>% filter(year %in% c(min_year:max_year))

## convert to spatial dataframe

coordinates(max) <- c("x", "y")

proj4string(max) <- CRS(my_crs)

## add CA tract into max temp

CA_temp <- sp::over(max, CA_tracts)

max@data %<>% 
  cbind(COUNTYFYP = CA_temp$COUNTYFP)

max@data$COUNTYFYP %<>% as.character()

CA_max_temp <- max@data %>% 
  group_by(date, COUNTYFYP) %>% 
  summarise(avg_max_temp = median(temp))


#### Load prec temp data ---- 

prec_temp <- brick("data/temp/PREC.nc")

## rotate long and lat to have -180 to 180 instead of -360 to 360  

prec_rotate <- rotate(prec_temp)

## convert raster into dataframe 

prec <- data.frame(rasterToPoints(prec_rotate))

## transform 

prec %<>% gather("date", "temp", -x, -y)

## edit date 

prec$date %<>% 
  gsub("X", "", .) %>% 
  lubridate::as_date()

prec$year <- lubridate::year(prec$date)

## filter to year range 

prec %<>% filter(year %in% c(min_year:max_year))

## convert to spatial dataframe

coordinates(prec) <- c("x", "y")

proj4string(prec) <- CRS(my_crs)

## add CA tract into prec temp

CA_temp <- sp::over(prec, CA_tracts)

prec@data %<>% 
  cbind(COUNTYFYP = CA_temp$COUNTYFP)

prec@data$COUNTYFYP %<>% as.character()

CA_prec_temp <- prec@data %>% 
  group_by(date, COUNTYFYP) %>% 
  summarise(avg_prec = median(temp))


#### Join all Temp and Prec data ---- 

CA_temp <- CA_min_temp %>% 
  full_join(CA_max_temp) %>% 
  full_join(CA_prec_temp)

## remove NA because NA is not in CA 

CA_temp %<>% 
  filter(!is.na(COUNTYFYP))


## save temp data ---- 

write_rds(CA_temp, "data/temp/CA_temp.RDS")


#### Join Temp & Prec and Fire data ---- 

fire_data %<>% as.data.frame()

## prepare fire data 

fire_data$COUNTYFYP %<>% as.character()

fire_data %<>% 
  filter(!is.na(COUNTYFYP))

fire_data$discovery_date %<>% as.Date()

fire_data$cont_date %<>% as.Date()

## save fire_data 

write_rds(fire_data, "data/fires/CA_fire.RDS")


