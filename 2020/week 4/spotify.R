library(tidyverse)
library(ggplot2)

spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')



spotify_songs


spotify_songs %>% 
  count(playlist_genre)
