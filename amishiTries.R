library(tidyverse)
library(stringr)
library(scales)
library(ggcorrplot)
# install.packages("ggcorrplot")

mycols <- c("#5fad56", "#f2c14e", "#f78154", "#4d9078")


leases_clean$zip<-as.factor(leases_clean$zip)
leases_coords$yrq <- paste(leases_coords$year, gsub("Q", "", leases_coords$quarter), sep = "-")
leases_clean$yrq <- paste(leases_clean$year, gsub("Q", "", leases_clean$quarter), sep = "-")
leases$yrq <- paste(leases$year, gsub("Q", "", leases$quarter), sep = "-")

by_submarket <- leases_clean %>% group_by(internal_submarket)

q42024_by_submarket <- by_submarket %>% filter(quarter=="Q4",year==2024)

grouped_submarket_year_q <- leases_coords %>% group_by(internal_submarket, year, quarter)

grouped_zip_year_q <- Leases %>% group_by(zip, year, quarter)

grouped_state_year_q <- leases_coords %>% group_by(state, year, quarter)


summary <- summarise(grouped_submarket_year_q, avg_SF = mean(total_leased_area), 
                     percentA = mean(internal_class == "A", na.rm = TRUE)*100,
                     rent = mean(overall_rent),
                     yrq = paste(year, gsub("Q", "", quarter), sep = "-"), 
                     region = region, 
                     average_prop = mean(available_prop, na.rm = TRUE),
                     percentGo = mean(transaction_category == "Go", na.rm = TRUE)*100, 
                     percentGo = mean(transaction_category == "Stay", na.rm = TRUE)*100)
summary

top5 <- summary %>%
  group_by(state) %>%
  summarise(avg_SF = mean(avg_SF, na.rm = TRUE)) %>%
  arrange(desc(avg_SF)) %>%
  slice_head(n = 5)

top5

filtered <- summary %>%
  filter(internal_submarket %in% top5$internal_submarket)


myplot <- ggplot(data = summary, aes(x=yrq, y=percentGo, group = 1)) +
   geom_point(size = 1.5)
myplot <- myplot + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x=element_text(angle=-45, hjust=0.001)) +
  scale_color_manual(values = mycols)
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

nums <- unlist(lapply(leases_clean, is.numeric), use.names = FALSE)  
num_only <- leases_clean[ , nums]
summary(leases_clean)
df$Ozone[is.na(df$Ozone)] <- mean(df$Ozone, na.rm = TRUE)


corr <- round(cor(num_only), 1)



# Plot
ggcorrplot(corr, hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           lab_size = 3,
           method="circle",
           colors = c("tomato2", "white", "springgreen3"),
           title="Correlogram of mtcars",
           ggtheme=theme_bw)

g <- ggplot(leases_coords, aes(transaction_category))
g + geom_bar(aes(fill=internal_class), width = 0.5) +
  labs(title="Bar Chart",
       subtitle="Manufacturer of vehicles",
       caption="Source: Frequency of Manufacturers from 'mpg' dataset") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))

unique(leases_coords$transaction_category)

consumerSentiment$observation_date <- str_replace(consumerSentiment$observation_date, "01-01", "1")
consumerSentiment$observation_date <- str_replace(consumerSentiment$observation_date, "04-01", "2")
consumerSentiment$observation_date <- str_replace(consumerSentiment$observation_date, "07-01", "3")
consumerSentiment$observation_date <- str_replace(consumerSentiment$observation_date, "10-01", "4")
consumerSentiment$CSINFT02USQ460S <- as.numeric(consumerSentiment$CSINFT02USQ460S)

newplot <- ggplot(data = consumerSentiment, aes(x=observation_date, y=CSINFT02USQ460S, group = 1)) +
  geom_line()
 newplot <- newplot + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x=element_text(angle=-45, hjust=0.001))

newplot

leases_coords %>%
  filter(transaction_category=="Stay"|transaction_category=="Go")%>%
  group_by(region, transaction_category)%>%
  summarise(count=n())%>%
  ggplot(mapping = aes(y=transaction_category, fill = region, x=count))+
  geom_col(position="fill")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Proportion of", x = "Year", y = "Rent")

leases_coords <- leases_coords %>%
  mutate(transaction_category = recode(transaction_type, New='Go', Relocation='Go', Renewal='Stay',`Renewal and Expansion`='Stay', Restructure='Stay', Extension='Stay', Expansion='Stay', `Sale - Leaseback`='Stay'))

unique(leases_coords$transaction_category)


leases_coords %>%
  filter(transaction_category=="Stay"|transaction_category=="Go")%>%
  group_by(market, transaction_category)%>%
  summarise(count=n())%>%
  ggplot(mapping = aes(y=market, fill = transaction_category, x=count))+
  geom_col(position="fill")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Proportion of", x = "Year", y = "Rent")

percent_stay<- leases_coords %>% group_by(yrq) %>% 
  summarise(percentStay = mean(transaction_category == "Stay", na.rm = TRUE)*100, year = year)
percent_stay <- percent_stay %>% left_join(consumerSentiment, by=join_by(yrq == observation_date))
percent_stay

aplot <- ggplot(percent_stay, aes(CSINFT02USQ460S, percentStay, color = year)) + 
  geom_point(size = 1.5) + theme(panel.grid.major = element_blank(), 
                                panel.grid.minor = element_blank(), 
                                panel.background = element_blank(), 
                                axis.line = element_line(colour = "black")) + 
  scale_x_continuous(limits = c(0, NA)) +
  scale_y_continuous(limits = c(0, NA))
aplot

market_grouped <- leases %>% group_by(market) %>% 
  filter(space_type == "New") %>%
  arrange(yrq) %>%
  summarise(change = 100 *(last(RBA) - first(RBA)) / first(RBA), 
            region = region, 
            percentA = mean(internal_class == "A", na.rm = TRUE)*100 
  )

graphthis <- unique(market_grouped) %>% arrange(desc(change))
graphthis

daplot <- ggplot(graphthis, aes(reorder(market, -change), change, fill = region)) + 
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
daplot

submarket_grouped <- leases_coords %>% group_by(internal_submarket) %>% 
  arrange(yrq) %>%
  summarise(change = 100 *(last(RBA) - first(RBA)) / first(RBA), 
            region = region
  )

graphthis <- head(unique(submarket_grouped) %>% arrange(desc(change)), 10)

daplot <- ggplot(graphthis, aes(reorder(market, -change), change, fill = region)) + 
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = mycols)

daplot

byYearQ <- leases_clean %>% group_by(year, quarter) %>%
  summarise(percentA = mean(internal_class == "A", na.rm = TRUE)*100, yrq = yrq)
byYearQ

nextPlot<- ggplot(data = byYearQ, aes(x=yrq, y=percentA, group = 1)) +
  geom_line()
nextPlot <- nextPlot + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x=element_text(angle=-45, hjust=0.001))
nextPlot
newplot

chisqData <- leases_coords %>% select(transaction_category, internal_class)

contingency_table <- table(chisqData$transaction_category, chisqData$internal_class)


print(contingency_table)

chi_square_test <- chisq.test(contingency_table)
chi_square_test

chisqData <- leases_coords %>% select(region, internal_class)

contingency_table <- table(chisqData$region, chisqData$internal_class)


print(contingency_table)

chi_square_test <- chisq.test(contingency_table)
chi_square_test

industry <- leases_coords %>% select(internal_industry, internal_class, market) %>%
  filter(internal_industry != is.na(internal_industry))
summary(industry)

tableInd <- table(industry$internal_industry, industry$internal_class)
print(tableInd)

cst <- chisq.test(tableInd)
cst

ggplot(industry, aes(internal_industry, fill =  internal_class)) + geom_bar() +
  theme(axis.text.x=element_text(angle=-20, hjust=0.001)) + 
  scale_fill_manual(values = mycols)
