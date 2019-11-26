library(tidyverse)
library(ggplot2)

pizza_jared <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-01/pizza_jared.csv")
pizza_barstool <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-01/pizza_barstool.csv")
pizza_datafiniti <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-01/pizza_datafiniti.csv")


pizza_jared
pizza_barstool
pizza_datafiniti


pizza_datafiniti %>% 
  count(price_range_min, price_range_max)


pizza_datafiniti %>% 
  count(province)

pizza_datafiniti %>% 
  left_join(pizza_barstool) %>% 
  count(province) %>% 
  head(20)


View(pizza_datafiniti)

pizza_barstool %>% 
  count(city) %>% 
  arrange(desc(n))

View(pizza_barstool)

pizza_barstool %>%
  select(name, c(contains('average_score'))) %>%
  arrange(desc(review_stats_all_average_score)) %>% 
  head(20) %>% 
  select(-review_stats_critic_average_score,
         -review_stats_all_average_score) %>% 
  mutate(name = fct_reorder(name, review_stats_community_average_score)) %>% 
  gather(key = source, value = average_score, c(contains('average_score'))) %>% 
  mutate(source = fct_recode(source,
                             'Dave' = 'review_stats_dave_average_score',
                             'Everyone' = 'review_stats_community_average_score')) %>%
  ggplot(aes(name, average_score))+
  geom_line(aes(group=name))+
  geom_point(size=2,
             aes(colour=source))+
  coord_flip()+
  labs(y='Average score',
       x= '',
       title = "Dave does not agree",
       subtitle = 'Of the top 20 pizza places, Dave tends to give \npizza places a lower score',
       colour='Source',
       caption = 'Source: Barstool sports \nPlot: @privlko')

theme_set(theme_light())


pizza_barstool %>%
  #filter(city== c('Bronx','Brooklyn','New York'))%>% 
  select(name, c(contains('average_score'))) %>%
  arrange(desc(review_stats_all_average_score),
          name) %>%
  head(20) %>% 
  select(name, c(contains('dave'), contains('commun'))) %>% 
  gather(key = source, value = average_score, c(contains('average_score'))) %>% 
  mutate(source = fct_recode(source,
                             'Dave' = 'review_stats_dave_average_score',
                             'Everyone' = 'review_stats_community_average_score'))  %>% 
arrange(average_score) %>% 
  ggplot(aes(name, average_score))+
  geom_line(aes(group=name))+
  geom_point(size=2,
             aes(colour=source))+
  coord_flip()+
  labs(y='Average score',
       x= '',
       title = "Dave does not agree",
       subtitle = 'Of the top 20 pizza places, Dave tends to give them a lower score',
       colour='Source',
       caption = 'Source: Barstool sports \nPlot: @privlko')



ggsave('C:/Users/Ivan/Desktop/R projects/#tidytuesday/tidytuesday/2019/week 40/pizzaratings.jpg',
       dpi=500,
       width=9,
       height=7)



pizza_datafiniti

