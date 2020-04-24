

library(tidyverse)
library(magrittr)
library(lubridate)
library(sp)

## load climate and fire data ---- 

CA_fire <- read_rds("data/fires/CA_fire.RDS")

CA_temp <- read_rds("data/temp/CA_temp.RDS")

## prepare temp data ---- 
## edit column to merge in with fire data discovery date 

colnames(CA_temp) <- c("discovery_date", "COUNTYFYP", "discovery_min_temp", "discovery_max_temp", "discovery_prec")

## join with fire data 

CA_fire %<>% left_join(CA_temp)

## edit column to merge in with fire data cont date 

colnames(CA_temp) <- c("cont_date", "COUNTYFYP", "cont_min_temp", "cont_max_temp", "cont_prec")

## join with fire data 

CA_fire %<>% left_join(CA_temp)


#### select columns ---- 

data <- CA_fire %>%  
  select(objectid,
         fire_year, 
         discovery_date, 
         discovery_doy, 
         discovery_time, 
         stat_cause_code, 
         stat_cause_descr, 
         cont_date, 
         cont_doy, 
         cont_time, 
         fire_size, 
         fire_size_class, 
         lat = y, 
         lon = x, 
         state, 
         COUNTYFP = COUNTYFYP, 
         discovery_min_temp, 
         discovery_max_temp, 
         discovery_prec, 
         cont_min_temp, 
         cont_max_temp, 
         cont_prec)

#### compute time ---- 

data$discovery_time_num <- data$discovery_time

data$discovery_time %<>% sprintf("%04d", .)

data$cont_time %<>% sprintf("%04d", .)

data$discovery_time %<>% 
  strptime(., "%H%M") %>% 
  strftime(., "%H:%M:%S")

data$cont_time %<>% 
  strptime(., "%H%M") %>% 
  strftime(., "%H:%M:%S")

data$discovery_datetime <- as_datetime(paste(data$discovery_date, data$discovery_time))

data$cont_datetime <- as_datetime(paste(data$cont_date, data$cont_time))

data$fire_duration <- time_length(difftime(data$cont_datetime, data$discovery_datetime), "minutes")


#### filter ----- 

data$fire_duration[data$fire_duration < 0] <- 0

data %<>% 
  filter(fire_duration >= 15 & !is.na(discovery_max_temp))


#### feature enginner ---- 

data %<>% 
  mutate(fire_volume = fire_size * fire_duration)


#### save data ---- 

write_rds(data, "data/data.RDS")


#### group by ---- 

# data_group <- data %>% 
#   group_by(discovery_date,  
#            county_fyp) %>% 
#   summarise(discovery_min_temp = mean(discovery_min_temp), 
#             discovery_max_temp = mean(discovery_max_temp), 
#             discovery_prec = mean(discovery_prec), 
#             cont_min_temp = mean(cont_min_temp), 
#             cont_max_temp = mean(cont_max_temp), 
#             cont_prec = mean(cont_prec), 
#             fire_volume = mean(fire_volume), 
#             fire_freq = n())


#### spatial object ---- 

# coordinates(data) <- c("lon", "lat")
# 
# proj4string(data) <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
# 
# plot(data)





