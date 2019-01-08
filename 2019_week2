library(tidyverse)
library(scales)
library(Hmisc)
library(plm)
library(ggrepel)


# read --------------------------------------------------------------------


w2 <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-01-08/IMDb_Economist_tv_ratings.csv')



# simplify genres ---------------------------------------------------------

w2b<- w2 %>% 
  separate(genres, c('genre'),
           sep=',')

w2b$genre <- factor(w2b$genre, levels = unique(w2b$genre))
w2b$genre


# form a panel data frame -------------------------------------------------

w2b<- w2b %>% 
  filter(seasonNumber>1,
         genre !='Biography') %>% 
  arrange(titleId, seasonNumber)


p.w2b<- pdata.frame(w2b, index = c("titleId", "seasonNumber"))



# grab the change score in ratings ----------------------------------------

p.w2b$change <-  diff(p.w2b$av_rating)


# check out the best ------------------------------------------------------

best <- p.w2b %>%
  group_by(genre) %>%
  filter(row_number(desc(change)) == 1)



# check out the worst -----------------------------------------------------


worst <- p.w2b %>%
  group_by(genre) %>%
  filter(row_number(change) == 1)



# plot --------------------------------------------------------------------

ggplot(p.w2b,
       aes(x=genre, y= change, colour=genre))+
  geom_jitter(alpha=0.5)+
  geom_point(size = 2, colour='black', data = best)+
  ggrepel::geom_label_repel(aes(label = title), data = best)+
  geom_point(size = 2, colour='black', data = worst)+
  ggrepel::geom_label_repel(aes(label = title), data = worst)+
  labs(title = 'Adventure and Animation show the smallest differences between seasons',
       subtitle = 'The Fosters, Law & Order, and Rosanne had sharp positive changes in ratings. \nAre You Afraid of the Dark, Scrubs, and Lethal Weapon had sharp falls in ratings',
       x='',
       y='Difference in ratings between seasons',
       caption = 'Data: The Economist \nPlot: @privlko')+
  theme(legend.position = 'none',
        panel.grid = element_blank(),
        plot.background = element_rect(fill = 'white'),
        strip.background = element_blank(),
        panel.background = element_blank())+
  geom_hline(yintercept = 0,
             colour='darkred',
             alpha=0.3,
             size=2) 



# save --------------------------------------------------------------------


ggsave("tidytuesday_w2.jpg")
