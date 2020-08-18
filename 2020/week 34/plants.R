library(tidytuesdayR)
library(tidyverse)
library(here)


# load the data  ----------------------------------------------------------

plant <- tt_load(2020, week = 34)


p1<- plant$plants
p2 <- plant$threats
p3 <- plant$actions






p1
  
p2 <- p1 %>% 
  group_by(country, continent) %>% 
  summarise(across(starts_with("threat"),
                   sum),
            n=n()) %>% 
  group_by(continent) %>% 
  mutate(total = sum(n),
         prop = n/total) %>%
  ungroup()  %>% 
  arrange(desc(prop))



p2
biggest <- max.col(p2[,3:14])

biggest

p2$main_threat = c("Agriculture", "Bio Resource Use", "Commercial",
                   "Invasive Species", "Energy Production" ,
                   "Climate Change", "Human Intrusions",
                   "Pollution" , "Transportation Corridor",
                   "Natural System Modifications",
                   "Geological Events", "Unknown")[biggest] 



p2 %>% 
  ggplot(aes(x= fct_reorder(country,prop),
             y=prop))+
  geom_col(aes(fill=main_threat))+
  coord_flip()+
  facet_wrap(~continent, scales="free")+
  labs(fill="",
       x="Country",
       y="Proportion of plants at risk",
       title= "Countries differ in the proportion of plants at risk",
       subtitle = "They also differ in the main types of risk faced by plants",
       caption = "Data: github.com/tidytuesday \nPlot: @privlko")+
  theme(#axis.text.x = element_text(size = 6),
        #axis.text.y = element_text(size = 5),
        legend.position="bottom",
        #plot.title = element_text(size=12,
                                  #colour = "grey20"),
        #plot.subtitle = element_text(size = 9,
                                    # colour = "grey20"),
        legend.text=element_text(size=7),
        plot.caption = element_text(size = 10)) 

ggsave(width = 11,
       height = 10,
       filename = here("2020", "week 34", "plants.png"))

here("week 34", "plants.png")
