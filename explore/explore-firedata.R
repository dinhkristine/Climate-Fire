

fire <- fire_data@data

fire$fpa_id %<>% as.character()

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


theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4))




Explore(fire, "source_system_type")
Explore(fire, "source_system") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4))
Explore(fire, "nwcg_reporting_unit_name", top = 20) + coord_flip()
Explore(fire, "source_reporting_unit_name", top = 20) + coord_flip()
Explore(fire, "stat_cause_descr")
Explore(fire, "discovery_doy", order = FALSE)
Explore(fire, "discovery_time", order = FALSE)
Explore(fire, "fire_size_class")
Explore(fire, "owner_descr") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4))
Explore(fire, "state") + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4))
Explore(fire, "fire_year", order = FALSE)


fire$discovery_date %<>% as.Date()

time <- fire %>% 
  group_by(discovery_date) %>% 
  tally()

ggplot(time, aes(x = discovery_date, y = n)) + 
  geom_line() + 
  theme_minimal() + 
  labs(y = "count fire")



