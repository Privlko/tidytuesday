library(tidyverse)
library(ggplot2)

# Get the Data

sf_trees <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')


sf_trees


sf_trees %>% 
  count(legal_status)


###what about summarising some of this into a yearly data.

sf_trees %>% 
  mutate(year = lubridate::year(date)) %>% 
  group_by(year, legal_status) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  filter(!is.na(year),
         legal_status!="Landmark tree",
         legal_status!="Private",
         legal_status!="Property Tree") %>% 
  ggplot(aes(x=year,
             y=n, group=legal_status,
             col=legal_status))+
  geom_point()+
  geom_line()+
  facet_wrap(~legal_status, scales = 'free')+
  theme(legend.position="bottom")+
  labs(x='Year',
       y="Number of trees planted",
       title="Number of trees peaked between 2010 and 2020 for most projects",
       subtitle="Permitted sites are still the largest producers of new trees",
       caption= "Source: data.sfgov.org \nPlot: @privlko",
       colour= "Legal status")


ggsave("C:/Users/Ivan.Privalko/Desktop/R projects/tidytuesday/2020/week 5/treesman.jpg")
