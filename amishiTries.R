library(tidyverse)

by_submarket <- Leases %>% group_by(internal_submarket)

q42024_by_submarket <- by_submarket %>% filter(quarter=="Q4",year==2024)

grouped_submarket_year_q <- by_submarket %>% 
  group_by(year) %>%
  group_by(quarter)


summary <- summarise(grouped_submarket_year_q, avg_SF = mean(leasedSF))
summary
