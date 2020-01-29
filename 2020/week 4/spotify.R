library(tidyverse)
library(ggplot2)

spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')



spotify_songs


spotify_songs %>% 
  count(playlist_genre)


spotify_artists <- spotify_songs %>% 
  select(track_artist)



# how does kanye west compare to everyone else ----------------------------


df1 <- spotify_songs %>% 
  mutate(kanye = case_when(track_artist =="Kanye West" ~'Yes, Kanye',
                           track_artist != "Kanye West" ~ "Someone else")) %>%
  filter(!is.na(kanye)) 

df1
  

df2 <- df1 %>% 
  select(track_artist, kanye, track_popularity, danceability, energy, speechiness, acousticness, instrumentalness, liveness, valence) %>% 
  gather(-track_artist, -kanye, -track_popularity, key='measure', value='value')


df2
df3 <- df2 %>% 
  group_by(measure, kanye) %>% 
  summarise(pop= mean(track_popularity),
            mean_ = mean(value),
             n = n(),
             sd_= sd(value),
             sq_n= sqrt(n),
             se = sd_/sq_n,
            cilow = (mean_ - (se*1.96)),
            cihigh= (mean_ + (se*1.96)))


df3

ggplot(df3,
         aes(x= measure, 
             y=mean_,
             col=kanye,
             group=kanye))+
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = cilow,
                    ymax = cihigh),
                size=0.3,
                width=0.1)+
  coord_flip()+
  labs(y="Spotify score",
       x="Spotify measure",
       title = "How does Kanye West compare to the average Spotify artist",
       subtitle = "Points contain 95% Confidence Intervals",
       caption = "Data: Spotify \nPlot: @privlko",
       colour="Is it Kanye?")+
  theme(legend.position="bottom")
