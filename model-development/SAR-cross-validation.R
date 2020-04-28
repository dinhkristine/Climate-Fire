


library(tidyverse)
library(magrittr)
library(sp)
library(tigris)
library(spdep)
library(spatialreg)
library(spacetime)


fire <- read_rds("data/data.RDS")

CA <- counties(state = "CA", resolution = "5m")

# fire %<>%
#   group_by(COUNTYFP) %>%
#   summarise(fire_volume = sum(fire_volume)/1000000,
#             fire_freq = n(),
#             discovery_max_temp = mean(discovery_max_temp))
# 
# CA_fire <- inner_join(as.data.frame(CA), fire, by = "COUNTYFP")
# 
# CA_fire %<>% modify_at(c("INTPTLON", "INTPTLAT"), as.numeric)
# 
coordinates(fire) <- c("lon", "lat")

proj4string(fire) <- CA@proj4string

fire_new <- over(fire, CA)
# 
# fire@data$lat <- fire_new$INTPTLAT
# 
# fire@data$lon <- fire_new$INTPTLON

fire@data %<>% left_join(fire_new %>% select(COUNTYFP, INTPTLAT, INTPTLON) %>% distinct)


fire@data %<>% 
  group_by(fire_year, fire_month, COUNTYFP, lat = INTPTLAT, lon = INTPTLON) %>% 
  summarise(fire_freq = length(unique(objectid)), 
            min_temp = median(min_temp, na.rm = TRUE), 
            max_temp = median(max_temp, na.rm = TRUE), 
            prec = median(prec, na.rm = TRUE))

fire <- fire@data %>% as.data.frame()




write_rds(fire, "data/data_new.RDS")

# 
# fire@data

fire$lat %<>% as.numeric()
fire$lon %<>% as.numeric()

coordinates(fire) <- c("lon", "lat")

proj4string(fire) <- CA@proj4string

# fire <- sapply(CA@polygons, function(x) x@area)

# u <- union(fire$COUNTYFP, CA$COUNTYFP)

# CA@data %<>% filter(COUNTYFP %in% c(intersect(fire$COUNTYFP, CA$COUNTYFP)))

# CA <- CA[CA$COUNTYFP %in% intersect(fire$COUNTYFP, CA$COUNTYFP), ]

# fire@data %<>% filter(fire_month == 1 & fire_year == 2010)

CA$min_temp <- sapply(as.character(unique(fire$COUNTYFP)), function(x){
  for (i in 2000:2015){
    median(fire$min_temp[fire$COUNTYFP == x & fire$fire_year == i], na.rm = T)
  }
})
# 
CA$discovery_max_temp <- sapply(as.character(unique(fire$COUNTYFP)), function(x){
  median(fire$discovery_max_temp[fire$COUNTYFP == x], na.rm = T)
})

nb_CA <- poly2nb(CA)

plot(CA)

plot(nb_CA, coordinates(CA), add = T)

listW <- nb2listw(nb_CA, style = "B")

W <- listw2mat(listW)

SAR_fire <- spautolm(fire_freq ~ max_temp, 
                     data = fire,
                     listw = listW, family = "SAR")

