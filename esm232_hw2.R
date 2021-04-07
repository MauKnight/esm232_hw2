
library(tidyverse)


# Dataframe requires the following columns: date, month, year 

almond_anomaly = function(climate_df, mo=1,  ){
  
 climate_df %>% 
    filter(month== 1) %>% 
    group_by(year) %>% 
    summarize(mean_t=mean(tmin_c)) 
            
            
}