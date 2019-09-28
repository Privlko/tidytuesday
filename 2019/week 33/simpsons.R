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
  filter(n>1)



