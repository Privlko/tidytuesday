library(tidyverse)
library(readr)
library(ggplot2)

(x <- read_csv('C:/Users/ivan.privalko/Documents/Russia/spreadsheets/labourmarket.csv')  %>% 
  select(-X3) %>% 
  gather(c(-measure, -gender), key= year, value = rate) %>% 
  ggplot(aes(x=year, 
             y=rate,
             group=gender)) +
  geom_line(aes(col=gender)) +
  geom_point(aes(col=gender)) +
  facet_wrap(~measure) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Russian employment figures are gendered and remain steady since 2014.",
       subtitle = "Women are less likely to participate in the labour market than men, although some growth in female participation appears.",
       caption = "Data: Rossstat \nGraphic: @privlko",
       y = "Percentage",
       x = "Year") +
  scale_colour_discrete("Gender", 
                      breaks=c("both","female", "male"), 
                      labels=c("Both", "Female", "Male")))



ggsave('C:/Users/ivan.privalko/Documents/Russia/charts/labourparticipation.jpg')
  
  