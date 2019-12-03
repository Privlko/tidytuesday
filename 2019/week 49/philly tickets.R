library(tidyverse)
library(lubridate)
library(ggthemes)
library(viridis)
library(patchwork)
library(extrafont)



tickets <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-03/tickets.csv")


tickets


View(tickets)



t1 <- tickets %>% 
  mutate(day_of_week = weekdays(issue_datetime),
         hour = lubridate::hour(issue_datetime)) %>% 
  group_by (day_of_week, hour) %>% 
  summarise(mean_fine = mean(fine, na.rm=T),
            count_fine = n()) %>% 
  ungroup() %>% 
  mutate(day_of_week = ordered(day_of_week, levels = c("Sunday","Saturday","Friday", "Thursday", "Wednesday","Tuesday",  "Monday"))) 



p1 <- ggplot(t1, aes(x = hour,y= day_of_week, fill =mean_fine))+
  geom_tile(color = "white", size = 0.1)+
  coord_equal()+
  scale_fill_viridis(name = "Average fine")+
  labs(y=NULL,
       x=NULL,
       title="Parking ticket fines are higher at night than during the day",
       subtitle="Saturday morning has some of the lowest average fines, \ncompared to other days and times")
  
p2 <- ggplot(t1, aes(x = hour,y= day_of_week, fill =count_fine))+
  geom_tile(color = "white", size = 0.1)+
  coord_equal()+
  scale_fill_viridis(name = "Number of fines")+
  #scale_x_continuous(breaks = seq(0,23,1))#+
  labs(y=NULL,
       x="Hour of day",
       title="Number of fines peaks at noon Tuesday to Thursday",
       subtitle = "Saturday morning fines are not uncommon",
       caption="Data: Open Data Philly \nPlot: @privlko")


p1/p2
ggsave("C:/Users/Ivan.Privalko/Desktop/R projects/tidytuesday/2019/week 49/tickets.jpg")

