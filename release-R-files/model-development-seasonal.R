
#### plit data into seasons ---- 

fire_spring <- fire[fire$fire_month < 3 | fire$fire_month == 12, ]
fire_summer <- fire[fire$fire_month < 6 & fire$fire_month > 2, ]
fire_fall <- fire[fire$fire_month < 9 & fire$fire_month > 5, ]
fire_winter <- fire[fire$fire_month < 12 & fire$fire_month > 8, ]


#### fit all the model based on their seasons ---- 

fit_gam_time_spring <- gam(fire_freq ~ min_temp + prec + s(lon, lat) + s(fire_year), 
                           data = fire_spring, family = "poisson", method="REML")

fit_gam_time_sm <- gam(fire_freq ~ min_temp + prec + s(lon, lat) + s(fire_year), 
                       data = fire_summer, family = "poisson", method="REML")

fit_gam_time_fa <- gam(fire_freq ~ min_temp + prec + s(lon, lat) + s(fire_year), 
                       data = fire_fall, family = "poisson", method="REML")

fit_gam_time_wt <- gam(fire_freq ~ min_temp + prec + s(lon, lat) + s(fire_year), 
                       data = fire_winter, family = "poisson", method="REML")


#### generate summary statistics for each fit ---- 

summary(fit_gam_time_spring)
summary(fit_gam_time_sm)
summary(fit_gam_time_fa)
summary(fit_gam_time_wt)
