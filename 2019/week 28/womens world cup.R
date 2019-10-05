library(tidyverse)
library(countrycode)



wwc_outcomes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/wwc_outcomes.csv")
squads <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/squads.csv")
codes <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-07-09/codes.csv")


codes

squads

outcomes <- dplyr::left_join(wwc_outcomes, codes, by = "team")


outcomes <- outcomes %>% 
  group_by(year, yearly_game_id) %>% 
  mutate(opposing_score = rev(score)) %>% 
  ungroup() %>% 
  mutate(won_by = score - opposing_score)


  
outcomes

?rev()


outcomes %>% 
  filter(year==2019,
         country=='United States')

ggplot(outcomes,
       aes(score))+
  geom_histogram()+
  facet_wrap(~win_status)


outcomes %>% 
  filter(yearly_game_id==12,
         year==2019)



outcomes %>% 
  filter(year==2019) %>% 
  count(round, sort=TRUE)



outcomes %>% 
  filter(year==2019,
         team %in% c('USA', 'NED'))



avg_group_scores <- outcomes %>% 
  filter(round =='Group') %>% 
  group_by(year, team) %>% 
  summarise(avg_group_score = mean(score),
            avg_group_won_by = mean (won_by)) %>% 
  ungroup()


outcomes %>% 
  inner_join(avg_group_scores, by = c('year', 'team')) %>% 
  filter(round=='Final') %>% 
  ggplot(aes(country, avg_group_won_by, fill=win_status))+
  geom_col() +
  facet_wrap(~year, scales = 'free_x')

ggsave('C:/Users/Ivan/Desktop/R projects/#tidytuesday/tidytuesday/2019/week 28/final results.jpg')

