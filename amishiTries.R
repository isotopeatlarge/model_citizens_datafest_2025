library(tidyverse)
library(stringr)
library(scales)

leases_clean$zip<-as.factor(leases_clean$zip)
leases_coords$yrq <- paste(leases_coords$year, gsub("Q", "", leases_coords$quarter), sep = "-")

by_submarket <- leases_clean %>% group_by(internal_submarket)

q42024_by_submarket <- by_submarket %>% filter(quarter=="Q4",year==2024)

grouped_submarket_year_q <- leases_clean %>% group_by(internal_submarket, year, quarter)

grouped_zip_year_q <- Leases %>% group_by(zip, year, quarter)

grouped_state_year_q <- leases_clean %>% group_by(state, year, quarter)


summary <- summarise(grouped_state_year_q, avg_SF = mean(total_leased_area), 
                     percentA = mean(internal_class == "A", na.rm = TRUE)*100,
                     rent = mean(overall_rent),
                     yrq = paste(year, gsub("Q", "", quarter), sep = "-"), 
                     region = region, 
                     average_prop = mean(available_prop, na.rm = TRUE))
summary

top5 <- summary %>%
  group_by(state) %>%
  summarise(avg_SF = mean(avg_SF, na.rm = TRUE)) %>%
  arrange(desc(avg_SF)) %>%
  slice_head(n = 5)

top5

filtered <- summary %>%
  filter(internal_submarket %in% top5$internal_submarket)


myplot <- ggplot(data = summary, aes(x=yrq, y=rent, group = state, color = region)) +
  geom_line()
myplot <- myplot + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x=element_text(angle=-45, hjust=0.001))
myplot

theme_set(theme_light()) # pre-set the bw theme.
g <- ggplot(leases_coords, aes(yrq, overall_rent))
g + geom_line(col = 'tomato3', show.legend=T) +
  geom_smooth() +
  labs(subtitle="mpg: city vs highway mileage",
       y="hwy",
       x="cty",
       title="Counts Plot") + 
  theme(axis.text.x=element_text(angle=-45, hjust=0.001))

summary(leases_coords)

ggplot(leases_coords, aes(x = available_prop, y = overall_rent)) +
  geom_point(aes(color = transaction_category), size = 0.5)
