library(tidyverse)
library(ggplot2)
library(maps)
library(viridis)




# theme_map not found -----------------------------------------------------

# added it here vs use ggthemes since jlev14 was having issues with the pkg
theme_map <- function(base_size = 9, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) %+replace% theme(axis.line = element_blank(), axis.text = element_blank(), 
                                                                              axis.ticks = element_blank(), axis.title = element_blank(), panel.background = element_blank(), panel.border = element_blank(), 
                                                                              panel.grid = element_blank(), panel.margin = unit(0, "lines"), plot.background = element_blank(), legend.justification = c(0, 
                                                                                                                                                                                                         0), legend.position = c(0, 0))
} 

# Get the Data

dog_moves <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_moves.csv')
dog_travel <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_travel.csv')
dog_descriptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-17/dog_descriptions.csv')


dog_descriptions

View(dog_descriptions)



us_states <- map_data("state")
us_states


ggplot(data = us_states,
       aes(x=long,
           y=lat,
           group=group))+
  geom_polygon(fill='white',
               colour='black')


ggplot(data = us_states,
       aes(x=long,
           y=lat,
           fill=region,
           group=group))+
  geom_polygon(colour='grey90', size=0.1)+
  guides(fill=FALSE)


ggplot(data = us_states,
       aes(x=long,
           y=lat,
           fill=region,
           group=group))+
  geom_polygon(colour='grey90', size=0.1)+
  guides(fill=FALSE)+
  coord_map(projection='albers', lat0=39, lat1=45)


us_states

dog_moves$location <- tolower(dog_moves$location)


dog_moves <- dog_moves %>% 
  rename(region=location) %>% 
  filter(inUS == TRUE)



dog_moves %>% 
  arrange(desc(exported)) %>% 
  head(23)

 
dog_map <- left_join(dog_moves, us_states)


dog_map %>% 
  mutate(net_dog=imported-exported) %>% 
  ggplot(aes(x=long,
             y=lat,
             fill=net_dog,
             group=group))+
  geom_polygon(colour='grey90', size=0.1)+
  coord_map(projection='albers', lat0=39, lat1=45)+
  scale_fill_viridis(name = "Net imports versus net exports of dogs")+
  theme_map()+
  theme(legend.position="top")+
  labs(title = "Northern states on either coast typically import more dogs than they export \nSouthern states, especially those on the East Coast tend to export more than they import",
       caption = "Data: Petfinder.com \nPlot: @privlko")

  
dog_map %>% 
  mutate(imports_of_total= imported/total) %>% 
  ggplot(aes(x=long,
             y=lat,
             fill=imports_of_total,
             group=group))+
  geom_polygon(colour='grey90', size=0.1)+
  coord_map(projection='albers', lat0=39, lat1=45)+
  scale_fill_viridis(name = "Import as a share of Total Pets")+
  theme_map()+
  theme(legend.position="top")+
  labs(title = "Northern, Coastal States rely most on imported dogs from other states",
       subtitle = "Almost 25% of all Washington State pets are imported from other states",
       caption = "Data: Petfinder.com \nPlot: @privlko")


ggsave("C:/Users/Ivan.Privalko/Desktop/R projects/tidytuesday/2019/week 51/pets.jpg")


View(dog_moves)

ggplot(data = dog_map,
       aes(x=long,
           y=lat,
           fill=total,
           group=group))+
  geom_polygon(colour='grey90', size=0.1)+
  coord_map(projection='albers', lat0=39, lat1=45)+
  scale_fill_viridis(name = "Total dogs")+
  theme_map()+
  labs(title = )
