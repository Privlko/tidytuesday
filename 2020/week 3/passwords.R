library(ggplot2)
library(tidyverse)
library(ggrepel)
library(twitteR)




# twitter bit -------------------------------------------------------------

setup_twitter_oauth(consumer_key = "6WbAEcYk6EVfr9fyZ2BRApB4B",
                    access_token = "899733659032449030-ek4Ja11pllrBN7JnmEx1AI7jUFhrUen",
                    consumer_secret = "DIRKpJRPa0MUTNyNCrmlFGVScihGfefcJACeMHXJcPAjterb4A",
                    access_secret = "JqvOvIM8aKAdaQNJcpbF0Sfq7He8jHrpm7V3zGvacMYjb" )


passwords <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')

passwords


passwords %>% 
  count(strength)


passwords %>% 
  ggplot(aes(x=font_size, y=strength))+
  geom_jitter()
  
best <- passwords %>%
  filter(strength > 45)



worst <- passwords %>%
  filter(strength < 1 & offline_crack_sec < 0.000000112) 


best
worst


lol <- passwords %>% 
  filter(strength == 19)

lol

passwords %>% 
  ggplot(aes(x=offline_crack_sec, y=strength))+
  geom_point() +
  scale_x_log10() +
  geom_label_repel(col="deepskyblue4",
                   data = best, aes(label = password),
          size = 3, hjust = 1.5)+
  geom_label_repel(col="khaki4",
                   data = worst, aes(label = password),
                   size = 3, hjust = 1.5) + geom_label_repel(col="orangered3",
                   data = lol, aes(label = password),
                   size = 3, hjust = 1.5)+
  annotate("text", x=1e-05, y=20.5,
           label="Lol")+
  labs(title = "Complex passwords are often the strongest",
       subtitle = "Passwords that use just numerical combinations rate low in strength. \nComplex passwords are harder to break",
       x= "Time it takes to crack password in seconds",
       y= "General strength of password",
       caption = "Source:  Information is Beautiful. \nPlot: @privlko")


ggsave("C:/Users/Ivan.Privalko/Desktop/R projects/tidytuesday/2020/week 3/passwords.jpg")


updateStatus("#Tidytuesday plot looking at password strength! Code here: https://github.com/Privlko/tidytuesday/blob/master/2020/week%203/passwords.R", mediaPath = "C:/Users/Ivan.Privalko/Desktop/R projects/tidytuesday/2020/week 3/passwords.jpg")

t2 <- updateStatus("also managed to send these tweets using R, so all-round good day", 
                   inReplyTo = 1217128684714385408)

