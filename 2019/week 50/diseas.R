library(tidyverse)
library(CSO)



get_cso("EA040") %>% 
  tbl_df()


murders <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-10/international_murders.csv")

gun_murders <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-10/gun_murders.csv")

diseases <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-10/diseases.csv")

nyc_regents <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-10/nyc_regents.csv")



nyc_regents$total <- rowSums(nyc_regents[,-1], na.rm=TRUE)

nyc_regents %>% 
  filter(!is.na(score)) %>%
  ggplot(aes(score, total)) + 
  annotate("rect", xmin = 65, xmax = 99, ymin = 0, ymax = 35000, alpha = .5) +
  geom_bar(stat = "identity", color = "black", fill = "#C4843C") + 
  annotate("text", x = 66, y = 28000, label = "MINIMUM\nREGENTS DIPLOMA\nSCORE IS 65", hjust = 0, size = 3) +
  annotate("text", x = 0, y = 12000, label = "2010 Regents scores on\nthe five most common tests", hjust = 0, size = 3) +
  scale_x_continuous(breaks = seq(5, 95, 5), limit = c(0,99)) + 
  scale_y_continuous(position = "right") +
  ggtitle("Scraping By") + 
  xlab("") + ylab("Number of tests") + 
  theme_minimal() + 
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        axis.ticks.length = unit(-0.2, "cm"),
        plot.title = element_text(face = "bold"))
























diseases %>% 
  count(disease)


diseases %>% 
  group_by(year, disease) %>% 
  summarise(total = sum(count, na.rm=T)) %>% 
  ggplot(aes(x=year,
             y=total,
             colour=disease))+
  geom_point()+
  geom_line()+
  scale_y_continuous(labels = scales::comma)
  

the_disease <- "Hepatitis A"

dat <- diseases %>%
  group_by(year, weeks_reporting) %>% 
  mutate(rate = (count / population)*100000)


dat

jet.colors <- colorRampPalette(c("#F0FFFF", "cyan", "#007FFF", "yellow", "#FFBF00", "orange", "red", "#7F0000"), bias = 2.25)


ggplot(dat,
       aes(x = year,y= weeks_reporting, fill =rate))+
  geom_tile(color = "white", size = 0.1)+
  coord_equal()+
  scale_fill_viridis(name = "Rate of infecton")


       dat %>% mutate(state = reorder(state, desc(state))) %>%
  ggplot(aes(year, rate, group = state)) +
  geom_tile(color = "white", size = 0.35) +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_gradientn(colors = jet.colors(16), na.value = 'white') +
  geom_vline(xintercept = 1995, col = "black") +
  theme_minimal() + 
  theme(panel.grid = element_blank()) +
  coord_cartesian(clip = 'off') +
  ggtitle(the_disease) +
  ylab("") +
  xlab("") +  
  theme(legend.position = "bottom", text = element_text(size = 8)) + 
  annotate(geom = "text", x = 1995, y = 50.5, label = "Vaccine introduced", size = 3, hjust = 0)





dat %>% 
  filter(!is.na(count)) %>% 
  mutate(rate= count/population) %>% 
  filter(!is.na(rate)) %>% 
  ggplot (aes(x= rate,
            y= factor(year)))+
  geom_density_ridges(alpha=0.3, fill='lightblue')+
  scale_x_log10()

