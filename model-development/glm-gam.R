


library(tidyverse)
library(mgcv)
library(gamclass)

fire_new$fire_my <- as.yearmon(paste(fire_new$fire_year, fire_new$fire_month))

fit_glm <- glm(fire_freq ~ min_temp + prec, data = fire_new, family = "poisson")

summary(fit_fire)

fire_new$lat %<>% as.numeric()
fire_new$lon %<>% as.numeric()

# fit_gam <- gam(fire_freq ~ m_temp + prec + s(lon, lat) + s(fire_month), 
#                data = fire_new, family = "poisson")

fit_gam_sp <- gam(fire_freq ~ min_temp + prec + s(lon, lat), 
               data = fire_new, family = "poisson", method="REML")

fit_gam_time <- gam(fire_freq ~ min_temp + prec + s(lon, lat) + s(fire_month) + s(fire_year), 
               data = fire_new, family = "poisson", method="REML")


# fit_gam_sp_cv <- CVgam(fit_gam_sp, nfold = 10, debug.level = 0, method = "GCV.Cp",
#       printit = TRUE, cvparts = NULL, gamma = 1, seed = 29, method="REML")

# fire_newwithin(fire_new, Date <- sprintf("%d-%02d", fire_year, fire_month))
