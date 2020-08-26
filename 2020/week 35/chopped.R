library(tidytuesdayR)
library(ggplot2)
library(tidyverse)
library(here)
library(Hmisc)
library(patchwork)

# load the data  ----------------------------------------------------------

df <- tt_load(2020, week = 35)


df
chopped <- df$chopped

chopped

p1 <- chopped %>% 
  filter(!is.na(episode_rating)) %>%
  ggplot(aes(x=season,
           y=episode_rating))+
  geom_jitter(alpha=0.25,
              aes(col=factor(season)))+
  stat_summary(fun = mean, geom = "point") + 
  stat_summary(fun.data = mean_se, 
               geom = "errorbar",
               fun.args = list(mult = 2))+
  scale_y_continuous(limits = c(5, 10))+
  theme(legend.position = "none") 



p1+labs(title = "Chopped season 35 saw a jump in ratings",
        subtitle = "Average ratings for the season contain standard errors(*1.96)",
        x="Season",
        y="Episode rating",
        caption = "Data: github.com/tidytuesday \nPlot: @privlko")


here()
ggsave(here("2020", "week 35", "chopped.png"))


rect <- data.frame(xmin=5.5, xmax=7.5, ymin=8, ymax=9)

p1 <- chopped %>% 
  filter(!is.na(episode_rating)) %>%
  ggplot(aes(x=season,
             y=episode_rating))+
  stat_summary(fun = mean, geom = "point") + 
  stat_summary(fun.data = mean_se, 
               geom = "errorbar",
               fun.args = list(mult = 2))+
  scale_y_continuous(limits = c(5, 10))+
  theme(legend.position = "none") +
  geom_rect(data=rect,
            aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            color="grey20",
            alpha=0.5,
            inherit.aes = FALSE)


  


sum1 <- chopped %>%
  filter(!is.na(episode_rating)) %>% 
  group_by(season) %>%
  summarise(mean_rating = mean(episode_rating),
            sd_rating = sd(episode_rating),
            n = n(),
            se_rating = sd_rating/sqrt(n)) 

p2 <- sum1 %>% 
  ggplot(aes(x=season, 
             y= mean_rating))+
  geom_point()+
  geom_errorbar(aes(ymin=mean_rating-(se_rating*1.96),
                    ymax=mean_rating+(se_rating*1.96)))+
  scale_y_continuous(limits = c(5, 10))+
  geom_rect(data=rect,
            aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
            color="grey20",
            alpha=0.5,
            inherit.aes = FALSE)


p3 <- p1/p2


p3

ggsave(here("2020", "week 35", "SE plot.png"))
