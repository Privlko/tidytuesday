library(ggrepel)
library(janitor)
library(ggrepel)
library(showtext)
library(ggplot2)
library(ggthemes)
library(tidyverse)


install.packages("colourvision")
font_add_google("Roboto", "rob")

#load the data
foot <- read_csv("C:/Users/Ivan/Desktop/dir/data/tidy/foot.csv") %>%
  clean_names()%>%
  select(-x1)

foot
View(foot)


# isolate key points ------------------------------------------------------

key_guys <- foot %>%
  filter(confederation == "CAF") %>%
  filter(gdp_weighted_share == 0) %>%
  filter(row_number((gdp_weighted_share))<10)
  
key_guys

# captions and communi ----------------------------------------------------

plot_title <- "Weighing a country's audience by GDP singles out CAF countries \nand makes their audience meaningless"
plot_subtitle <- "CAF countries almost completely unaffected by population."
plot_caption <- "Plot: @privlko"  

ggplot(foot, aes(x = log10(population_share), 
                 y = log10(gdp_weighted_share)))+
  geom_jitter(width = 0.05,
              height =  0.05,
              size = 3,
              aes(colour = confederation)) +
  ggrepel::geom_label_repel(data = subset(foot, population_share >3),
                  mapping = aes(label=country))+
  labs(title= plot_title,
       subtitle = plot_subtitle,
       caption= plot_caption,
       x = "Logged Country's Share of Population",
       y = "Logged Country's Share of Audience, Weighted by GDP") +
  scale_y_continuous(breaks = seq(-3, 5, by = 1),
                     labels = seq(-3, 5, by = 1)) +
  scale_color_brewer(palette="Set3")+
  coord_cartesian(xlim = c(-2,2), ylim = c(-2,2)) +
  ggrepel::geom_label_repel(
    aes(label=country),
    data = key_guys) 



ggsave("week11.jpeg")

?geom_text

windowsFonts()
