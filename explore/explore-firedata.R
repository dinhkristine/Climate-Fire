

library(tidyverse)
library(magrittr)

fire <- read_rds("data/data.RDS")

Explore <- function(df, group, order = TRUE, top = NULL){
  df %<>% 
    group_by(.dots = group) %>% 
    summarise(n = length(unique(objectid))/1000)
  
  if(!is.null(top)){
    df %<>% 
      arrange(desc(n)) %>% 
      top_n(top)
  }
  
  df %<>% rename(xvar = group)
  
  if(order == TRUE){
    p <- ggplot(df, aes(x = reorder(xvar, -n)))
  } else {
    p <- ggplot(df, aes(x = xvar))
  }
  
  p + 
    geom_bar(aes(y = n), stat = "identity") + 
    theme_minimal() + 
    labs(x = group, 
         y = "count fire (by 1K)", 
         title = paste("Histogram of", group))
}

Explore(fire, "stat_cause_descr") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4))
Explore(fire, "discovery_doy", order = FALSE)
Explore(fire, "fire_size_class")
Explore(fire, "fire_year", order = FALSE)
Explore(fire, "discovery_time_num", order = FALSE)


fire$discovery_date %<>% as.Date()

time <- fire %>% 
  group_by(discovery_date) %>% 
  tally()

ggplot(time, aes(x = discovery_date, y = n)) + 
  geom_line() + 
  theme_minimal() + 
  labs(y = "count fire")


#### fire duration vs date ---- 

duration <- fire %>%
  filter(fire_year == 2005) %>% 
  group_by(discovery_date) %>% 
  summarise(avg_duration = mean(fire_duration))

ggplot(duration, aes(x = discovery_date, y = avg_duration)) + 
  geom_line() + 
  theme_minimal()

#### fire duration vs temp ----

temp_min <- fire %>%
  filter(fire_year == 2005) %>% 
  group_by(discovery_min_temp) %>% 
  summarise(avg_duration = mean(fire_duration))


ggplot(temp_min, aes(x = discovery_min_temp, y = avg_duration)) + 
  geom_line()

temp_max <- fire %>%
  filter(fire_year == 2005) %>% 
  group_by(discovery_max_temp) %>% 
  summarise(avg_duration = mean(fire_duration))


ggplot(temp_max, aes(x = discovery_max_temp, y = avg_duration)) + 
  geom_line()

prec <- fire %>%
  filter(fire_year == 2005) %>% 
  group_by(discovery_prec) %>% 
  summarise(avg_duration = mean(fire_duration))


ggplot(prec, aes(x = discovery_prec, y = avg_duration)) + 
  geom_line()


