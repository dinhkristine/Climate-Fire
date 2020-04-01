#### load packages ----- 

library(tidyverse)
library(magrittr)
library(rnoaa)

# update rnoaa package 
# remotes::install_github("ropensci/rnoaa")

#### Get coronavirus data ----

# get data from COVID-19 github repo, pull from master do update 
# https://github.com/CSSEGISandData/COVID-19
us_county_covid <- read.csv("C:/Users/kdinh.ARROW/Projects/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv") 


us_county_covid %<>% gather("confirm_date", "cases", 
                           -UID, -iso2, -iso3, -code3, -FIPS, -Admin2, -Province_State, -Country_Region, 
                           -Lat, -Long_, -Combined_Key)

us_county_covid <- quivR::make_names(us_county_covid)

us_county_covid$confirm_date %<>% 
  strptime(., format = "X%m.%d.%y") %>% 
  as.Date()


#### Get Climate data ---- 

options(noaakey = .NOAA_token)
# 
# 
# climate_by_station <- ncdc(datasetid='GHCND', startdate = '2020-01-01', enddate = '2020-03-20', add_units = TRUE)
# 
loc <- ncdc_locs_cats(datasetid = "GHCND")

location <- ncdc_locs(locationcategoryid='CNTRY', sortfield='name', sortorder='desc')
# 
# climate_by_station <- climate_by_station$data
# 
# location <- ncdc_locs(datasetid='GHCND', limit = 500)

station <- ncdc_stations(datasetid = "GHCND", limit = 1000, locationid='FIPS:US')

out <- ncdc(datasetid='GHCND', datatypeid = "TAVG", locationid = "FIPS:US",
            startdate = '2020-01-01', enddate = '2020-03-20', add_units = TRUE, limit = 25)





