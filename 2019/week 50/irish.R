library(tidyverse)
library(CSO)



t0 <- get_cso("EA040")

t1 <- t0 %>% 
  tbl_df() %>% 
  filter(Census.Year==2016,
         Sex!="Both sexes",
         County.and.City!="State",
          Statistic=="Irish speakers as a percentage of total (%)") %>% 
  group_by(County.and.City, Census.Year) %>% 
  arrange(value)

theme_set(theme_light())
t1
  
ggplot(t1, aes(fct_reorder(factor(County.and.City), value), value)) +
  geom_bar(stat = "identity") +
  coord_flip()


ggplot(t1, aes(fct_reorder(factor(County.and.City), value), value)) +
  geom_line(aes(group = County.and.City)) +
  geom_point(size=2.5,
             aes(col=Sex)) +
  coord_flip()+
  labs(x=NULL)
  


 big.diff <- t1 %>% 
  spread(Sex, value) %>% 
  group_by(County.and.City) %>% 
  mutate(Diff = Female - Male) %>% 
  arrange(desc(Diff)) %>% 
  filter(Diff >= 9)
 
 highlight <- filter(t1, County.and.City %in% big.diff$County.and.City)

highlight

ggplot(t1, aes(fct_reorder(factor(County.and.City), value), value)) +
  geom_line(alpha=0.2,
            aes(group = County.and.City)) +
  geom_point(alpha=0.2,
             aes(col=Sex)) +
  geom_line(data = highlight, aes(group = County.and.City)) +
  geom_point(data = highlight, aes(color = Sex), size = 2.5) +
  coord_flip()+
  labs(x=NULL,
       y="Share of population who speak Irish",
       title="Irish speakers differ by gender and geography",
       subtitle = "Western counties and cities have a greater share of Irish speakers, \nbut even within cities and counties women are more likely to speak Irish",
       caption="Data: Ireland's Central Statistics Office, Census Data \n Plot: @privlko")
ggsave("C:/Users/Ivan.Privalko/Desktop/R projects/tidytuesday/2019/week 50/irish.jpg")


map_data