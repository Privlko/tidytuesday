library(tidyverse)
library(here)
library(janitor)
library(ggbeeswarm)
library(ggthemes)
library(lubridate)
library(ggplot2)
library(showtext)
font_add_google("Roboto", "rob")



alc<- read_csv("C:/Users/00015/Desktop/tidytuesday/week13/week13.csv")

alc
alc$country <- as.factor(alc$country)



c <- alc %>%  
  mutate(servings = beer_servings + spirit_servings + wine_servings) %>%
  arrange(desc(servings)) %>%
  top_n(20, servings)
  


c
eire <-c %>%
filter(country=="Ireland")


c$country <- factor(c$country, levels = c$country[order(c$servings)])

ggplot(c, aes(x= country,
              y=servings))+
  geom_bar(stat="identity") +
  coord_flip()



d <- gather(c, key = type, value = num,
       beer_servings,  spirit_servings,  wine_servings)
  
d
z <- ggplot(d, aes(x= country,
              y=num))+
  geom_bar(stat="identity", aes(fill = type))+
  coord_flip()


z +
  scale_x_discrete(expand = c(.02, 0)) +
  labs(title = "Top 20 Alcohol Consumers, by Type",
       subtitle = "The largest consumers of alcohol differ in preference for beer, wine, or spirits",
       caption= "Data:FiveThirtyEight \nPlot: @privlko",
       x = "Top 20 Consumers",
       y = "Servings of Alcohol",
       fill = "Alcohol Type") +
  scale_fill_discrete(breaks = c("beer_servings", 
                                 "spirit_servings",
                                 "wine_servings"),
                      labels = c("Beer", "Spirits", "Wine")) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", 
                                  size = 20, 
                                  margin = margin(b = 10),
                                  colour= "black"),
        axis.text= element_text(size = 12, 
                                margin = margin(b = 10),
                                colour= "black"))

ggsave("week13.jpg")
  
  
