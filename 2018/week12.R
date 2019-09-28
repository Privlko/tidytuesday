
# load the packages -------------------------------------------------------



library(tidyverse)
library(lubridate)
library(ggridges)
library(nord)
library(here)
library(showtext)

?font_add_google

font_add_google("Roboto", "Roboto", regular.wt = 400)

trend_data <- read_csv("https://github.com/rfordatascience/tidytuesday/raw/master/data/week12_google_trends.csv", skip = 2, col_names = TRUE) %>% 
  set_names(str_extract(names(.), "(?<=Hurricane )(\\w+)|(Day)")) %>% 
  rename(Date = Day) %>% 
  mutate(source = "Google Trends") %>% 
  mutate_at(vars(-Date, -source), funs(./max(.)))

mediacloud_data <- read_csv("https://github.com/rfordatascience/tidytuesday/raw/master/data/week12_mediacloud_hurricanes.csv", col_names = TRUE) %>% 
  mutate(source = "Online News") %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate_at(vars(-Date, -source), funs(./max(.)))

tv_data <- read_csv("https://github.com/rfordatascience/tidytuesday/raw/master/data/week12_tv_hurricanes.csv") %>% 
  mutate(source = "TV") %>% 
  mutate(Date = mdy(Date)) %>% 
  mutate_at(vars(-Date, -source), funs(./max(.)))

all_data <- bind_rows(trend_data, mediacloud_data, tv_data) %>%
  gather(hurricane, value, -Date, - source) %>% 
  set_names(tolower(names(.)))

showtext_auto()

View(all_data)

plot <- ggplot(all_data, aes(x = date, y = source)) +
  geom_ridgeline(aes(height = value, fill = factor(hurricane)), size = 0.1, scale = 0.8, alpha = 0.8) +
  labs(title = "On nearly every form of media, hurricanes that hit mainland US received more sustained coverage than Maria in Puerto Rico",
       subtitle = "Ridgeline plots of normalized media shares (TV, Online News and Google Trends)",
       caption = "Data: fivethirtyeight | Graphic: @jakekaupp",
       y = NULL,
       x = NULL) +
  scale_x_date(expand = c(0,0)) +
  scale_fill_nord(name = "Hurricane", palette = "lumina") 
  

plot
ggsave(plot, filename = here("week12", "ROCK YOU LIKE A HURRICANE.png"), width = 6, height = 3)
