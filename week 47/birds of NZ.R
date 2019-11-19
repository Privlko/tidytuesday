library(tidyverse)

nz_bird <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-19/nz_bird.csv")
nz_bird
View(nz_bird)

nz_bird %>% 
  count(vote_rank)

nz_bird %>% count(date)
nz_bird %>% count(hour) %>% head(30)

ggplot(nz)


nz_bird %>% 
  filter(bird_breed=='New Zealand Falcon') %>% 
  ggplot(aes(x=date, y=..count..)) +
  geom_bar(aes(fill=vote_rank))


nz_bird %>% 
  mutate(night = if_else(hour < 9, 1, 0)) %>% 
  filter(bird_breed=='New Zealand Falcon') %>% 
  ggplot(aes(x=date, y=..count..)) +
  geom_bar(aes(fill=factor(night)))


nz_bird %>% 
  mutate(night = if_else(hour < 9 | hour > 19, 1, 0)) %>% 
  filter(stringr::str_detect(bird_breed, 'Owl') ) %>% 
  ggplot(aes(x=date, y=..count..)) +
  geom_bar(aes(fill=factor(night)))


nz_bird %>% 
  mutate(night = if_else(hour < 9 | hour > 19, 1, 0)) %>% 
  ggplot(aes(x=date, y=..count..)) +
  geom_bar(aes())+
  facet_wrap(~night)


theme_set(theme_light())
nz_bird %>% 
  mutate(penguin = if_else(bird_breed=='Yellow-eyed penguin', 1, 0),
         night = if_else(hour < 9 | hour > 19, 1, 0)) %>% 
  filter(!is.na(penguin)) %>% 
  ggplot(aes(x=date, y=..count..)) +
  geom_bar(aes(fill=factor(penguin)),
           position='fill')+
  facet_grid(~vote_rank)+
  scale_fill_discrete(name = "", labels = c("Other birds", "Yellow-eyed Penguin"))+
  labs(x= '',
       y="Portion of total vote",
       caption= "Source: New Zealand's Bird of the Year Dataset. \nPlot: @privlko",
       title= "The Yellow-Eyed Penguin gradually received a greater share of votes over time",
       subtitle="As the competition drew to a close, the penguin managed to increase the share of available first preference votes. The penguin wasn't able to increase \nthe share of other votes") 

  

nz_bird %>% 
  filter(!is.na(bird_breed)) %>% 
  group_by(date, vote_rank, bird_breed) %>% 
  count() %>% 
  ungroup() %>% 
  group_by(date, vote_rank) %>%
  mutate(sum = sum(n),
         prop = n/sum(n)) %>% 
  ungroup() %>% 
  filter(prop >0.03) %>% 
  ggplot(aes(x=date,
             y=prop,
             group=bird_breed))+
  geom_point(aes(col=bird_breed))+
  geom_line(aes(col=bird_breed))+
  facet_wrap(~vote_rank)+
  scale_colour_discrete(name = "")+
  labs(x= '',
       y="Portion of total vote",
       caption= "Source: New Zealand's Bird of the Year Dataset. \nPlot: @privlko",
       title= "The Yellow-Eyed Penguin gradually received a greater share of votes over time",
       subtitle="As the competition drew to a close, the penguin managed to increase the share of available first preference votes. The penguin wasn't able to increase \nthe share of other votes") 


         