#### load data ---- 

fire <- read_rds("data/data_new.RDS")


#### generalized linear model 

fit_glm <- glm(fire_freq ~ min_temp + prec, data = fire, family = "poisson")

summary(fit_glm)


#### change class ---- 

fire$lat %<>% as.numeric()
fire$lon %<>% as.numeric()


#### Generalized additive model

fit_gam_sp <- gam(fire_freq ~ min_temp + prec + s(lon, lat), 
                  data = fire, family = "poisson", method="REML")

fit_gam_time <- gam(fire_freq ~ min_temp + prec + s(lon, lat) + s(fire_month) + s(fire_year), 
                    data = fire, family = "poisson", method="REML")

summary(fit_gam_sp)
summary(fit_gam_time)
