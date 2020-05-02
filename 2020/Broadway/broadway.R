library(tidyverse)
library(lubridate)
library(viridis)
library(forcats)
# Get the Data

grosses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/grosses.csv', guess_max = 40000)
synopses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/synopses.csv')
cpi <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/cpi.csv')
pre_1985_starts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-28/pre-1985-starts.csv')



# Or read in with tidytuesdayR package (https://github.com/thebioengineer/tidytuesdayR)
# PLEASE NOTE TO USE 2020 DATA YOU NEED TO USE the tidytuesdayR version after Jan 2020.

# Either ISO-8601 date or year/week works!

grosses


grosses %>% 
  ggplot(aes(x=weekly_gross)) +
  geom_density()+
  scale_x_log10(labels= scales::comma)


grosses
b_way

b_way <- 
  grosses %>% 
  mutate(year = year(week_ending),
         month = factor(month(week_ending))) %>% 
  group_by(month, year) %>% 
  summarise(avg_price = mean(avg_ticket_price, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(month = fct_recode(month, 
                            "January"= "1",
                            "February"="2",
                            "March"="3", 
                            "April"="4", 
                            "May"="5",
                            "June"="6",
                            "July"="7", 
                            "August"="8", 
                            "September"="9",
                            "October"="10", 
                            "November"="11", 
                            "December"="12")) 

  
  
  
  b_way

  
  
  ggplot(b_way, aes(x = year,y= month, fill =avg_price))+
    geom_tile(color = "white", size = 0.1)+
    coord_equal()+
    scale_fill_viridis(name = "Average ticket")+
    labs(y=NULL,
         x=NULL,
         title="Broadway tickets have slowly risen in price from 2010",
         subtitle="2018 wan an especially expensive year for tickets",
         caption = "Data: @alexcookson \nPlot: @privlko")
  
  
  ggsave("C:/Users/Ivan/Desktop/R projects/#tidytuesday/tidytuesday/2020/Broadway/broadway.jpg")
  
  