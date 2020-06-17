#### load climate and fire data ---- 

# Go here for CA_fire.RDS data: https://github.com/dinhkristine/Climate-Fire/tree/master/data/fires
CA_fire <- read_rds("data/fires/CA_fire.RDS")

# Go here for CA_temp.RDS data: https://github.com/dinhkristine/Climate-Fire/tree/master/data/temp
CA_temp <- read_rds("data/temp/CA_temp.RDS")


#### prepare temp data ---- 
## edit column to merge in with fire data discovery date 

colnames(CA_temp) <- c("discovery_date", "COUNTYFYP", "discovery_min_temp", 
                       "discovery_max_temp", "discovery_prec")

## join with fire data 

CA_fire %<>% 
  left_join(CA_temp, by = c("discovery_date", "COUNTYFYP"))

## edit column to merge in with fire data cont date 

colnames(CA_temp) <- c("cont_date", "COUNTYFYP", "cont_min_temp", "cont_max_temp", "cont_prec")

## join with fire data 

CA_fire %<>% 
  left_join(CA_temp, by = c("cont_date", "COUNTYFYP"))


#### Create new column ---- 

data <- CA_fire %>% 
  mutate(fire_month = month(discovery_date))  


#### select necessary columns ----

data %<>% 
  dplyr::select(objectid,
         fire_year, 
         fire_month, 
         lat = y, 
         lon = x, 
         COUNTYFP = COUNTYFYP, 
         min_temp = discovery_min_temp, 
         max_temp = discovery_max_temp, 
         prec = discovery_prec)


#### save data ---- 

# write_rds(data, "data/data.RDS")




