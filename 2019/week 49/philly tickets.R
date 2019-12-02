library(tidyverse)
library(lubridate)
library(ggthemes)
library(viridis)
library(extrafont)


tickets <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-03/tickets.csv")


tickets


View(tickets)

lubridate::hour(tickets$issue_datetime)
  


t1 <- tickets %>% 
  #extract day of the week - apparently there were exactly two times of each day (14 days total)
  mutate(day_of_week = weekdays(issue_datetime),
         hour = lubridate::hour(issue_datetime)) %>% 
  group_by (day_of_week, hour) %>% 
  #the count/5 is to get the number of voters, each person voted 5 times ( should be a total of 43460/5)).
  summarise(mean_fine = mean(fine, na.rm=T),
            count_fine = n()) %>% 
  ungroup() %>% 
  #reordering factor levels to fit the weekday scehdule
  mutate(day_of_week = ordered(day_of_week, levels = c("Sunday","Saturday","Friday", "Thursday", "Wednesday","Tuesday",  "Monday"))) 

t1
#Let's see how many voted on Monday?
t1 %>% 
  arrange(desc(mean_fine))









#Plot!
ggplot(t1, aes(x = hour,y= day_of_week, fill =mean_fine))+
  geom_tile(color = "white", size = 0.1)+
  #This creates an equal ration for the squares of 1:1
  coord_equal()+
  scale_fill_viridis(name = "Average fine")+
  labs(y="",
       title="Average parking ticket fines tend to be higher at night \nthan during the day")
  



  #Good color scale emphasizing the highest number
  labs(x = NULL, y = NULL, title = "NZ Bird of the Year Voting Distribution across day and time",
       subtitle = "Sunday night's tiredness generates Monday noon's peak diversion interest?",  caption = "Data: Dragonfly Data Science | @Amit_Levinson")+
  #Wanted to display all hours of the day
  scale_x_continuous(breaks = seq(0,23,1))+
  theme_tufte()
g+
  theme(text = element_text(family = "Microsoft Tai Le"),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 14),
        plot.caption = element_text(size = 8, face = "italic"),
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 14)
  )
ggsave("nz_bird_vote.png", width =10, height = 6)