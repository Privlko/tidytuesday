library(tidyverse)
library(ggplot2)
library(hexbin)

theme_set(theme_light())
ipf_lifts <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-10-08/ipf_lifts.csv")


ipf_lifts %>% 
  count(sex) %>% 
  mutate(prop = n/sum(n))


ipf_lifts %>% 
  count(division) %>% 
  mutate(prop = n/sum(n))


ipf_lifts %>% 
  count(place) %>% 
  mutate(prop = n/sum(n))


ipf_lifts %>% 
  filter(place =='DD')

ipf_lifts %>% 
  ggplot(aes(age,best3squat_kg))+
  geom_point(aes(alpha=0.05))+
  facet_wrap(~sex)

doping <- ipf_lifts %>% 
  gather(key=form, value = best, best3squat_kg, best3bench_kg, best3deadlift_kg) %>%
  mutate(form = fct_recode(form,
                           'Squat' = 'best3squat_kg',
                           'Bench' = 'best3bench_kg',
                           'Deadlift' = 'best3deadlift_kg')) %>% 
  filter(place=='DD')


ipf_lifts %>% 
  gather(key=form, value = best, best3squat_kg, best3bench_kg, best3deadlift_kg) %>%
  mutate(form = fct_recode(form,
                           'Squat' = 'best3squat_kg',
                           'Bench' = 'best3bench_kg',
                           'Deadlift' = 'best3deadlift_kg')) %>% 
  filter(best > 0) %>% 
  ggplot()+
  geom_hex(aes(x=age,
               y=best,
               alpha=0.4)) +
  geom_point(data=doping,
           aes(x=age,
               y=best),
           col="darkred") +
  geom_smooth(aes(age,best,
                  col=sex),
              size=2,
              method='lm',
              se=F) +
  geom_smooth(data=doping,
              aes(age,best,
                  col=place),
              method='lm',
              size=2,
              se=F)+
  facet_grid(form~sex)+
  guides(alpha=F,
         count=F,
         fill=F)+
  labs(title='Respondents accused of doping score higher than \nthose who are not accussed',
       subtitle='Younger competitors appear most likely to be disqualified for doping',
       y='Best Result',
       x='Age',
       caption ='Source: OpenPowerLifting.org \nPlot: @privlko' ,
       colour= 'Linear estimate')

ggsave('C:/Users/Ivan.Privalko/Desktop/tidytuesday/week41/lift.jpg')


