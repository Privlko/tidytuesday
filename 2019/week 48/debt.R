library(tidyverse)
library(ggridges)

loans <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-26/loans.csv")

loans %>% 
  count(year)
loans


 
loans <-  loans %>% 
  mutate(dt1 = paste0(year,"/",quarter )) 

loans$dt1 <- factor(loans$dt1, levels = c("15/4", "16/1", "16/2",
                                "16/3", "16/4", "17/1",
                                "17/2", "17/3", "17/4",
                                "18/1", "18/2", "18/3",
                                "18/4"))

loans 

loans %>% 
   count(dt1)
 loans %>% 
  filter(dt1 != "15/4") %>% 
  gather(key="source",
         value = "value",
         consolidation, rehabilitation,voluntary_payments,wage_garnishments) %>% 
  ggplot(aes(x=dt1,
         y=total))+
  geom_col(aes(fill=source))+
  scale_y_continuous(labels=scales::dollar)

 theme_set(theme_minimal())
 
loans %>% 
  filter(dt1 != "15/4") %>% 
  gather(key="source",
         value = "value",
         consolidation, rehabilitation,voluntary_payments,wage_garnishments) %>% 
  #filter(source == "voluntary_payments"|
  #        source == "wage_garnishments" ) %>% 
  ggplot (aes(x= value,
              y=fct_rev(dt1)))+
  geom_density_ridges(alpha=0.3, fill='lightblue')+
  scale_x_continuous(labels=scales::dollar)+
  geom_vline(xintercept= 0,
             alpha=0.3, 
             size=2,
             col='navy')+
  facet_grid(.~source,
             scales="free",
             labeller = labeller(source=c("consolidation"= "Consolidation",
                                          "rehabilitation"= "Rehabilitation",
                                          "voluntary_payments" ="Voluntary Payments",
                                          "wage_garnishments" = "Wage garnishments")))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(x= 'Total',
       y='Date in quarters',
       title="The Composition and Repayment of Student Loans is Changing",
       subtitle = "Each measure is becoming bimodal, especially after Q2 2017. \nThis includes Voluntary Payments but also includes Wage Garnishing.",
       caption= "Source: Dignity and Debt. \nPlot: @privlko")

  ggsave("2019/week 48/debt.jpg")

