
library(tidyverse)
library(janitor)
library(devtools)
library(here)


# Dataframe requires the following columns: date, month, year 
# read in data
climate_df <- read.delim(here('clim.txt'), sep = " ")

# make fxn

almond_anomaly = function(climate_df, coef_t1=-0.015, coef_t2=-0.0046, coef_p1=-0.07, coef_p2=0.0043, intercept=0.28){
  
 filt_clim_df <- climate_df %>%
   group_by(year, month) %>%
   summarize(min_t = min(tmin_c),
             mean_precip = mean(precip)) %>% 
   filter(month %in% c(1,2))
 
 t_feb <- filt_clim_df %>% 
   filter(month == 2) %>% 
   select(min_t)
 
 precip_jan <- filt_clim_df %>% 
   filter(month == 1) %>% 
   select(mean_precip)
 
 yeild_anom <- as.data.frame(coef_t1*t_feb$min_t + coef_t2*(t_feb$min_t^2) + coef_p1*precip_jan$mean_precip + coef_p2*(precip_jan$mean_precip^2) + intercept)
 names(yeild_anom) <- "Yeild Anomoly (tons/acre)"

 return(yeild_anom)
             
}

# test fxn

test <- almond_anomaly(climate_df)
