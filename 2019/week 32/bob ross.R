library(tidyverse)
library(ggraph)

theme_set(theme_light())

bob_ross <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-06/bob-ross.csv")


bob <-  bob_ross %>% 
  janitor::clean_names() %>% 
  gather(element, present, -episode, -title) %>% 
  filter(present==1) %>% 
  mutate(title = str_to_title(str_remove_all(title, '"')),
         element = str_to_title(str_replace(element, '_', ' '))) %>% 
  select(-present) %>%
   extract(episode, c('season', 'episode_number'), 
           "S(.*)E(.*)", #
           convert = T, remove = F) %>% 
   arrange(season, episode_number)


bob %>% 
  count(element, sort= T) %>% 
  head(25) %>% 
  mutate(element = fct_reorder(element, n)) %>% 
  ggplot(aes(element, n))+
  geom_col()+
  coord_flip()

bob


bob %>% 
  add_count(title) %>% 
  arrange(desc(n))


bob %>% 
  filter(element== 'Steve Ross')


bob %>% 
  group_by(season) %>% 
  summarise(episode= n_distinct(episode))

bob %>% 
  add_count(season) %>% 
  count(season, element, n, sort=T) %>% 
  rename(total_el= n,
         n=nn) %>% 
  mutate(perc = n/total_el) 


bob %>% 
  filter(!element %in% c('Tree', 'Trees')) %>% 
  add_count(season) %>% 
  count(season, element, n, sort=T) %>% 
  rename(total_el= n,
         n=nn) %>% 
  mutate(perc = n/total_el) %>% 
  filter(element=='Clouds') %>% 
  ggplot(aes(season, perc)) +geom_line()+
  expand_limits(y=0)

View(bob)

bob %>% 
  filter(!element %in% c('Tree', 'Trees')) %>% 
  group_by(season) %>% 
  mutate(total_episode = n_distinct(episode)) %>% 
  count(season, element, total_episode, sort=T) %>% 
  mutate( perc = n/total_episode) %>% 
  filter(element=='Mountain') %>% 
  ggplot(aes(season, perc)) +geom_line()+
  expand_limits(y=0)+
  scale_y_continuous(labels= scales::percent_format())





#thinking of within elements, rather than seasons or episodes

bob %>% 
  filter(!element %in% c('Tree', 'Trees')) %>% 
  group_by(season) %>% 
  mutate(total_episode = n_distinct(episode)) %>% 
  count(season, element, total_episode, sort=T) %>% 
  mutate( perc = n/total_episode)
  
  
##############new approach


library(widyr)

correlations <- bob %>% 
pairwise_cor(element, episode, sort=T)

correlations


correlations %>% 
  filter(item1=='River') %>% 
  mutate(item2=fct_reorder(item2, correlation)) %>% 
  ggplot(aes(item2, correlation))+
  geom_col()+
  coord_flip()+
  labs(title='What goes with rivers?')


correlations %>% 
  head(100)

library(ggraph)
library(igraph)


correlations %>% 
  head(100) %>% 
  graph_from_data_frame() %>% 
  ggraph()+
  geom_edge_link()+
  geom_node_point()
