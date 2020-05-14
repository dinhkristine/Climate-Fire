
library(tidyverse)
library(magrittr)
library(maps)
library(tigris)
library(sp)
library(RColorBrewer)
library(broom)
library(knitr)
library(kableExtra)

fire <- read_rds("data/data_new.RDS")

CA_tracts <-  counties(state = "CA", resolution = "5m")

fire$lat %<>% as.numeric()
fire$lon %<>% as.numeric()

color <- "#047094"

ggplot(fire, aes(x = fire_year)) + 
  geom_histogram(stat = "count", fill = color, col = "black") + 
  theme_minimal()  + 
  labs(title = "Fire Year Distribution", 
       y = "Count", 
       x = "Fire Year")

fire_group <- fire %>% 
  group_by(COUNTYFP, lon, lat) %>% 
  summarise(fire_freq = sum(fire_freq))

coordinates(fire_group) <- c("lon", "lat")

fire_group@proj4string <- CA_tracts@proj4string

fire_new <- over(CA_tracts, fire_group)

CA_tracts$fire_freq <- fire_new$fire_freq


#Create a function to generate a continuous color palette
rbPal <- colorRampPalette(c('red','blue'))

#This adds a column of color values
# based on the y values

# summary(CA_tracts$fire_freq)

# CA_tracts$Col <- rbPal(10)[as.numeric(cut(CA_tracts$fire_freq,breaks = 10))]

plot(CA_tracts, col=pal(nrow(CA_tracts@data))[CA_tracts$Col], main = "Plot of Fire Frequency by County")
# legend("topright",   
#        legend = as.numeric(cut(CA_tracts$fire_freq,breaks = 10)), 
#        fill = unique(CA_tracts$Col)) 


# df = data.frame(Year=1980:2010, Flow=rnorm(31, mean=150, sd=4),
#                 Temp=rnorm(31, mean=0, sd=2))

# Optionally set colours using RColorBrewer
# library(RColorBrewer)
cols = brewer.pal(9, "Blues")

# Define colour pallete
# pal = colorRampPalette(c("blue", "red"))
# Use the following line with RColorBrewer
pal = colorRampPalette(cols)

# Rank variable for colour assignment
CA_tracts$Col = findInterval(CA_tracts$fire_freq, sort(CA_tracts$fire_freq))

# Make plot
# plot(Flow ~ Year, df, pch=19, col=pal(nrow(df))[df$order])
# Add a simple legend
legend("topright", col=pal(9), pch=19,
       legend=c(round(seq(0, 12500, 1500), 1)))


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


fire$fire_month %<>% as.factor()
fire$fire_year %<>% as.factor()


glm(fire_freq ~ fire_month, fire, family = "poisson") %>% broom::tidy() %>%
  cbind(confint(glm(fire_freq ~ fire_month, fire, family = "poisson"))) %>% 
  kable(digits = 3, caption = "Summary Statistics of GLM model: fire_freq ~ fire_month", 
        row.names = FALSE) %>% 
  kable_styling("bordered", full_width = F)


glm(fire_freq ~ fire_year, fire, family = "poisson") %>% broom::tidy() %>%
  cbind(confint(glm(fire_freq ~ fire_year, fire, family = "poisson"))) %>% 
  kable(digits = 3, caption = "Summary Statistics of GLM model: fire_freq ~ fire_year", 
        row.names = FALSE) %>% 
  kable_styling("bordered", full_width = F)


glm(fire_freq ~ min_temp, fire, family = "poisson") %>% broom::tidy() %>%
  cbind(confint(glm(fire_freq ~ min_temp, fire, family = "poisson"))) %>% 
  kable(digits = 3, caption = "Summary Statistics of GLM model: fire_freq ~ min_temp", 
        row.names = FALSE) %>% 
  kable_styling("bordered", full_width = F)


glm(fire_freq ~ max_temp, fire, family = "poisson") %>% broom::tidy() %>%
  cbind(confint(glm(fire_freq ~ max_temp, fire, family = "poisson"))) %>% 
  kable(digits = 3, caption = "Summary Statistics of GLM model: fire_freq ~ max_temp", 
        row.names = FALSE) %>% 
  kable_styling("bordered", full_width = F)

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

ggplot(fire, aes(x = max_temp, y = min_temp)) + 
  geom_point(size = 2, color= color, alpha = 0.2) + 
  theme_minimal() + 
  labs(x = expression("Maximum Temperature " ( degree*K)), 
       y = expression("Minimum Temperature " ( degree*K)), 
       title = "Relationship between Minimum Temperature & Maximum Temperature")



glm(fire_freq ~ prec, fire, family = "poisson") %>% broom::tidy() %>%
  cbind(confint(glm(fire_freq ~ prec, fire, family = "poisson"))) %>% 
  kable(digits = 3, caption = "Summary Statistics of GLM model: fire_freq ~ prec", 
        row.names = FALSE) %>% 
  kable_styling("bordered", full_width = F)


ggplot(fire, aes(x = prec, y = fire_freq)) + 
  geom_point(size = 2, color= color, alpha = 0.2) + 
  theme_minimal() + 
  labs(x = "Precipitation (mm/day)", 
       y = "Fire Frequency", 
       title = "Fire Frequency by Precipitation")



