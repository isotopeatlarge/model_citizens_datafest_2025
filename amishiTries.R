library(tidyverse)

by_submarket <- Leases %>% group_by(internal_submarket)

q42024_by_submarket <- by_submarket %>% filter(quarter=="Q4",year==2024)

grouped_submarket_year_q <- Leases %>% group_by(internal_submarket, year, quarter)

grouped_zip_year_q <- Leases %>% group_by(zip, year, quarter)



summary <- summarise(grouped_zip_year_q, avg_SF = mean(leasing), 
                     percentA = mean(internal_class == "A", na.rm = TRUE))
summary
