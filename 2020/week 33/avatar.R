library(tidytuesdayR)
library(tidyverse)
library(here)

tt_available()

ava <- tt_load(2020, week = 33)


x <- ava$avatar


x %>% 
  ggplot(aes(x=s))



View(x)


df1 <- x %>% 
  group_by(book_num, chapter_num) %>% 
  summarise(mean_imdb= mean(imdb_rating, na.rm=T),
            sd_imdb = sd(imdb_rating,na.rm=T))



p <-   ggplot(df1, aes(x=factor(book_num), y=mean_imdb))+
  geom_point(aes(alpha=0.5,
                 colour=factor(book_num)))+
  geom_boxplot(aes(alpha=0.5,
                   colour=factor(book_num)))+
  scale_y_continuous(limits = c(6,10))


p + 
  theme(legend.position="bottom") +
  labs(x="Book number",
       y="IMDB rating",
       colour="Book number",
       title="Avatar improves over time",
       subtitle = "IMDB  ratings are higher in Book 3 than previous books",
       caption= "Data: github.com/tidytuesday \nPlot: @privlko")+
  scale_alpha(guide=FALSE)+
  theme_fivethirtyeight()


here("Projects", "tidytuesday")

ggsave(here("Projects", "tidytuesday", "avatar.png"))

library(ggthemes) # Load
install.packages("ggthemes")

ggthemes_data
