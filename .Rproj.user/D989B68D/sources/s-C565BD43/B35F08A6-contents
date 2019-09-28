library(tidyverse)
library(ggplot2)
theme_set(theme_light())


simpsons <- readr::read_delim("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-27/simpsons-guests.csv", delim = "|", quote = "")


simpsons


simpsons %>% 
  filter(str_detect(role, 'self|selves')) %>% 
  count(role, sort=TRUE) %>%
  filter(role %in% c('Himself', 'Herself'))
  
simpsons %>% 
  filter(str_detect(role, 'self|selves')) %>% 
  count(role, sort=TRUE) 

simpsons <- simpsons %>% 
  filter(str_detect(role, 'self|selves')) %>% 
  mutate(self=str_detect(role, 'self|selves'))

simpsons %>% 
  filter(self) %>% 
  count(guest_star, sort=T) %>% 
  filter(n>1) %>% 
  mutate(guest_star = fct_reorder(guest_star , n)) %>% 
  ggplot(aes(guest_star, n))+
  geom_col()+
  coord_flip()+
  labs(title = "Who has appeared on The Simpsons \nas themselves most often?")


simpsons %>% 
  filter(str_detect(role, ';')) %>% 
  add_count(role) 


simpsons %>% 
  separate_rows(role, sep= ";\\s+") %>% 
  add_count(role)

simpsons %>% 
  separate_rows(role, sep= ";\\s+") %>% 
  count(role, sort = T)

simpsons


simpsons %>% 
  separate_rows(role, sep= ";\\s+") %>% 
  add_count(role) %>% 
  mutate(self=str_detect(role, 'self|selves')) %>% 
  filter(n>12, self==F) %>% 
  count(season= parse_number(season), role) %>% 
  mutate(role = fct_reorder(role, -nn, sum)) %>% 
  ggplot(aes(season, nn))+
  geom_col()+
  facet_wrap(~role)+
  labs(title = 'Most common guest characters \nin The Simpsons',
       subtitle = 'Guest characters who do not \nplay themselves are often the ones listed below',
       y='Apearances',
       x='Season',
       caption='Source: Wikipedia \nPlot: @privlko' )


simpsons

ggsave('C:/Users/Ivan/Desktop/R projects/#tidytuesday/tidytuesday/2019/week 33/simpsons.jpg')
