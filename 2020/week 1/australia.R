library(tidyverse)
library(lubridate)
library(sf)
library(viridis)


# Get the Data

rainfall <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/rainfall.csv')
temperature <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/temperature.csv')

# IF YOU USE THIS DATA PLEASE BE CAUTIOUS WITH INTERPRETATION
nasa_fire <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/MODIS_C6_Australia_and_New_Zealand_7d.csv')

# For JSON File of fires
url <- "http://www.rfs.nsw.gov.au/feeds/majorIncidents.json"

aus_fires <- sf::st_read(url)


aus_fires


aus_fires
nasa_fire
temperature
rainfall



df1 <- temperature %>% 
  mutate(day = day(date),
         month = month(date),
         year = year(date)) %>% 
  filter(year==1910,
         temp_type=='max') %>% 
  mutate(temp1910 = temperature) %>% 
  select(city_name, site_name, day, month, temp1910)

df1 

df1 %>% 
  ggplot(aes(x = month,y= day, fill =temp1910))+
  geom_tile(color = "white", size = 0.1)+
  coord_equal()+
  scale_fill_viridis(name = "Average temp")+
  coord_flip()+
  facet_grid(city_name~.)


df2 <- temperature %>%
  mutate(day = day(date),
         month = month(date),
         year = year(date)) %>% 
  filter(year==2018,
         temp_type=='max') %>% 
  mutate(temp2018 = temperature) %>% 
  select(city_name, site_name, day, month, temp2018)




df <- df2 %>% 
  left_join(df1) %>% 
  mutate(temp_dev = temp2018-temp1910)


  df %>% 
    ggplot(aes(x = month,y= day, fill =temp_dev))+
    geom_tile(color = "white", size = 0.1)+
    coord_equal()+
    scale_fill_viridis(name = "Average temp")+
  coord_flip()

  
  

  df <- temperature %>% 
    mutate(month = month(date),
           year = year(date)) %>%
    filter(temp_type=="max",
           year== 2015 | year== 1915) %>% 
    group_by(city_name, month, year) %>% 
    summarise(mean_temp = mean(temperature))
    
  
  
  
  
  df$month <- factor(df$month, levels = unique(df$month))
  
  
  
  
  df$month <- fct_recode(df$month,
                       "January" = "1",
                       "February" = "2",
                       "March"= "3",
                       "April" = "4", 
                       "May" = "5",
                       "June"= "6",  
                       "July"="7",
                       "August" = "8", 
                       "September" ="9", 
                       "October"="10",
                       "November"="11", 
                       "December"="12") 

  theme_set(theme_light())
    
  df %>% 
    filter(city_name !="BRISBANE") %>% 
  ggplot(aes(x=month, y=mean_temp))+
    geom_point(size=2,
               aes(col=factor(year)))+
    geom_line(size=2,
              aes(group=factor(year), col=factor(year)))+
    facet_wrap(~city_name)+
    theme(axis.text.x = element_text(angle = 90))+
    labs(title = "Australian temperatures have changed since 1915, with winters becoming warmer",
         subtitle = "Comparing average max temperature by month for 1915, and 2015 shows that winters \nwere warmer on average in 2015. \nThis is especially true in Canberra, Kent, and Melbourne.",
         x="",
         colour="Year",
         y="Average temperature in a given month (Celsius)",
         caption= "Data: Bureau of Meteorology \nPlot: @privlko") +
    theme(plot.caption = element_text(color = "gray55", size = 10))
  
  
  ggsave("C:/Users/Ivan.Privalko/Desktop/R projects/tidytuesday/2020/week 1/australia.jpg")
  