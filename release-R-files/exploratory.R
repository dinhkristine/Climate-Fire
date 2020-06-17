
#### load data ---- 

### fire flat file 
fire <- read_rds("data/data_new.RDS")

### CA tract from tigris package
CA_tracts <-  counties(state = "CA", resolution = "5m")

#### change class ---- 

fire$lat %<>% as.numeric()
fire$lon %<>% as.numeric()


#### plots ---- 

## fire year histogram 

ggplot(fire, aes(x = fire_year)) + 
  geom_histogram(stat = "count", fill = color, col = "black") + 
  theme_minimal()  + 
  labs(title = "Fire Year Distribution", 
       y = "Count", 
       x = "Fire Year")


## fire spatial 

fire_group <- fire %>% 
  group_by(COUNTYFP, lon, lat) %>%  
  summarise(fire_freq = sum(fire_freq))

coordinates(fire_group) <- c("lon", "lat")

fire_group@proj4string <- CA_tracts@proj4string

fire_new <- over(CA_tracts, fire_group)

CA_tracts$fire_freq <- fire_new$fire_freq

# set color 

cols <- brewer.pal(9, "Blues")

pal <- colorRampPalette(cols)

CA_tracts$Col = findInterval(CA_tracts$fire_freq, sort(CA_tracts$fire_freq))


# plot map with color based on the number of fire  

plot(CA_tracts, col=pal(nrow(CA_tracts@data))[CA_tracts$Col], main = "Plot of Fire Frequency by County")

legend("topright", col=pal(9), pch=19,
       legend=c(round(seq(0, 12500, 1500), 1)))


## fire count by time 

fire_month <- fire %>% 
  group_by(fire_month) %>% 
  summarise(fire_freq = sum(fire_freq))

ggplot(fire_month, aes(x = factor(fire_month), y = fire_freq, group = 1)) + 
  geom_point(size = 2, color= color) + 
  geom_line(size = 1, color = color) + 
  theme_minimal() + 
  labs(x = "Fire Month", 
       y = "Fire Frequency", 
       title = "Fire Frequency by Month") + 
  scale_y_continuous(labels = scales::comma)


fire_year <- fire %>% 
  group_by(fire_year) %>% 
  summarise(fire_freq = sum(fire_freq))

ggplot(fire_year, aes(x = (fire_year), y = fire_freq)) + 
  geom_point(size = 2, color= color) + 
  geom_line(size = 1, color = color) + 
  theme_minimal() + 
  labs(x = "Fire Year", 
       y = "Fire Frequency", 
       title = "Fire Frequency by Year") + 
  scale_y_continuous(labels = scales::comma)


## fire count based on precipitation 

fire_prec_month <- fire %>% 
  group_by(fire_month) %>% 
  summarise(prec = mean(prec, na.rm = TRUE))

ggplot(fire_prec_month, aes(x = factor(fire_month), y = prec, group = 1)) + 
  geom_point(size = 2, color= color) + 
  geom_line(size = 1, color = color) + 
  theme_minimal() + 
  labs(x = "Fire Month", 
       y = "Precipitation", 
       title = "Precipitation by Month") + 
  scale_y_continuous(labels = scales::comma)


## fire count based on temperature 

ggplot(fire, aes(x = min_temp, y = fire_freq)) + 
  geom_point(size = 2, color= color, alpha = 0.2) + 
  theme_minimal() + 
  labs(x = expression("Minimum Temperature " ( degree*K)), 
       y = "Fire Frequency", 
       title = "Fire Frequency by Minimum Temperature")

ggplot(fire, aes(x = max_temp, y = fire_freq)) + 
  geom_point(size = 2, color= color, alpha = 0.2) + 
  theme_minimal() + 
  labs(x = expression("Maximum Temperature " ( degree*K)), 
       y = "Fire Frequency", 
       title = "Fire Frequency by Maximum Temperature")


## maximum and minimum temperature correlation plot 

ggplot(fire, aes(x = max_temp, y = min_temp)) + 
  geom_point(size = 2, color= color, alpha = 0.2) + 
  theme_minimal() + 
  labs(x = expression("Maximum Temperature " ( degree*K)), 
       y = expression("Minimum Temperature " ( degree*K)), 
       title = "Relationship between Minimum Temperature & Maximum Temperature")


## fire frequency based on precipitation

ggplot(fire, aes(x = prec, y = fire_freq)) + 
  geom_point(size = 2, color= color, alpha = 0.2) + 
  theme_minimal() + 
  labs(x = "Precipitation (mm/day)", 
       y = "Fire Frequency", 
       title = "Fire Frequency by Precipitation")



