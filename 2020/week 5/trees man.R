library(tidyverse)
library(ggplot2)

# Get the Data

sf_trees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')


sf_trees


sf_trees %>% 
  count(legal_status)

sf_trees %>% 
  group_by(date, legal_status) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  filter(!is.na(date),
         legal_status!="Landmark tree",
         legal_status!="Private",
         legal_status!="Property Tree") %>% 
  ggplot(aes(x=date,
             y=n, group=legal_status,
             col=legal_status))+
  geom_point()+
  geom_line()+
  facet_wrap(~legal_status, scales = 'free_y')



###what about summarising some of this into a yearly data.

sf_trees %>% 
  mutate(year = lubridate::year(date)) %>% 
  group_by(year, legal_status) %>% 
  count() %>% 
  filter(!is.na(date),
         legal_status!="Landmark tree",
         legal_status!="Private",
         legal_status!="Property Tree") %>% 
  ggplot(aes(x=year,
             y=n, group=legal_status,
             col=legal_status))+
  geom_point()+
  geom_line()+
  facet_wrap(~legal_status)

