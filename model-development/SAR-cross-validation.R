


library(tidyverse)
library(magrittr)
library(sp)
library(tigris)
library(spdep)
library(spatialreg)


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
# coordinates(fire) <- c("lon", "lat")
# 
# proj4string(fire) <- CA@proj4string
# 
# fire_new <- over(CA, fire)
# 
# fire@data





# u <- union(fire$COUNTYFP, CA$COUNTYFP)

# CA@data %<>% filter(COUNTYFP %in% c(intersect(fire$COUNTYFP, CA$COUNTYFP)))

CA <- CA[CA$COUNTYFP %in% intersect(fire$COUNTYFP, CA$COUNTYFP), ]

CA$fire_volume <- sapply(as.character(unique(fire$COUNTYFP)), function(x){
    sum(fire$fire_volume[fire$COUNTYFP == x], na.rm = T)
})

CA$discovery_max_temp <- sapply(as.character(unique(fire$COUNTYFP)), function(x){
  median(fire$discovery_max_temp[fire$COUNTYFP == x], na.rm = T)
})

nb_CA <- poly2nb(CA)

plot(CA)

plot(nb_CA, coordinates(CA), add = T)

listW <- nb2listw(nb_CA, style = "B", zero.policy = T)

W <- listw2mat(listW)

SAR_fire <- spautolm(fire_volume ~ discovery_max_temp, 
                     data = CA,
                     listw = listW, family = "SAR")

