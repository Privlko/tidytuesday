
# tidy tuesday week 16------------------------------------------------------------



# packages ----------------------------------------------------------------


library(tidyverse)
library(here)
library(janitor)
library(ggbeeswarm)
library(ggthemes)
library(lubridate)
library(ggplot2)
library(showtext)




# load the file -----------------------------------------------------------


exe<- read_csv("C:/Users/00015/Desktop/tidytuesday/week16/week16.csv")

exe


# parse some of the measures to stuff that makes sense --------------------


exe$state <- as.factor(exe$state)
exe$state <- parse_factor(exe$state, levels = rev(unique(exe$state)))

exe
exe$exercise <- parse_number(exe$exercise)

exe

exe$sex <- parse_factor(exe$sex, levels = rev(unique(exe$sex)))
exe$work_status <- parse_factor(exe$work_status, levels = rev(unique(exe$work_status)))

exe$work_status

# sort the data frame, simple ---------------------------------------------
exe

a <- exe %>%
  group_by(state) %>%
  filter(sex == "male" | sex== "female") %>%
  summarise(exercise = mean(exercise, na.rm = TRUE)) %>%
  arrange(desc(exercise)) 
a



# split the frame  --------------------------------------------------------


b <- exe %>%
  group_by(state, sex, work_status) %>%
  filter(sex == "male" | sex== "female") %>%
  filter(work_status=="working") %>%
  summarise(exercise = sum(exercise, na.rm = TRUE)) %>%
  arrange(desc(exercise)) 

b
b$work_status

# plot the frame ----------------------------------------------------------


ggplot(b, aes(exercise, reorder(state, exercise))) +
  geom_line(aes(group = state))+
  geom_point(aes(color = sex), size = 2.5) +
  geom_text(aes(colour=sex, label=exercise),  
            size=4)


# think about the labels --------------------------------------------------

lab_right <- 
  b %>%
  group_by(state) %>%
  arrange(desc(exercise)) %>%
  top_n(1)

lab_right

lab_left <- b %>%
  group_by(state) %>%
  arrange(desc(exercise)) %>%
  slice(2)

lab_left



# plot the new one --------------------------------------------------------


ggplot(b, aes(exercise, reorder(state, exercise))) +
  geom_line(aes(group = state))+
  geom_point(aes(color = sex), size = 2.5) +
  geom_text(data=lab_right,
            aes(colour=sex, label=exercise),  
            size=4, hjust=-0.5) +
  geom_text(data = lab_left, 
            aes(colour=sex, label=exercise),  
            size=4, hjust = 1.5) +
  scale_x_continuous(limits = c(-15, +50)) 

  b

# pull out the bigger differences -----------------------------------------
b

  big_diff <- b %>% 
    spread(sex, exercise, fill=1) %>% 
    group_by(state) %>% 
    mutate(Max = max(`female`, `male`),
           Min = min(`female`, `male`),
           Diff = Max / Min - 1) %>% 
    arrange(desc(Diff)) %>%
    filter(Diff > .55)
  
big_diff

big_diff
lab_right <- filter(lab_right, state %in% big_diff$state)
lab_left <- filter(lab_left, state %in% big_diff$state)
highlight <- filter(b, state %in% big_diff$state)



# plot the data highlighting bigger differences ---------------------------


zz<- ggplot(b, aes(exercise, reorder(state, exercise))) +
  geom_line(aes(group = state),
            alpha=0.2)+
  geom_point(aes(color = sex), 
             size = 2.5,
             alpha=0.2) +
  geom_line(data = highlight,
            aes(group = state))+
  geom_point(data=highlight,
             aes(color = sex), 
             size = 2.5) +
  geom_text(data=lab_right,
            aes(colour=sex, label=exercise),  
            size=4, hjust=-0.5) +
  geom_text(data = lab_left, 
            aes(colour=sex, label=exercise),  
            size=4, hjust = 1.5) +
  scale_x_continuous(limits = c(0, +45)) 



# tart it up a bit --------------------------------------------------------

my_title <- "Gender differences in exercise are spatial."
my_subtitle <- "Chart lists gender differences in exercise among the working population. \nDifferences between men and women are particularly relevant (>50%) \nin states where people exercise less overall. \nGender differences are less extreme in states with more exercise."
my_caption <- "Source:Centre for Disease Control and Prevention  \nPlot: @privlko"

zz +scale_color_discrete(labels = c("Female", "Male")) +
  scale_y_discrete(expand = c(.02, 0)) +
  labs(title= my_title,
       subtitle= my_subtitle,
       caption=my_caption,
       x = "Average exercise",
       y = "") +
  theme_minimal()+
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 14, margin = margin(b = 5)),
        plot.subtitle = element_text(size = 10, color = "darkslategrey", margin = margin(b = 2)),
        plot.caption = element_text(size = 8, margin = margin(t = 4), color = "grey70", hjust = 0),
        axis.title.y = element_text(size = 8, margin = margin(t = 4), color = "darkslategrey", hjust = 0),
        axis.title.x = element_text(size = 8, margin = margin(t = 4), color = "darkslategrey", hjust = 0),
        axis.text.y = element_text(margin = margin(t = 6), color = "darkslategrey", hjust = 1,
                                   lineheight = 6))

ggsave("week16.jpg")
