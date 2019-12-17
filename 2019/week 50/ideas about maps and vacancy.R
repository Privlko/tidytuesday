library(tidyverse)
library(CSO)
library(ggrepel)
library(lme4)
library(broom.mixed)
library(broom)
library(patchwork)




t1 <-  get_cso("EP009") %>% 
  tbl_df()  %>% 
  filter(Statistic=="Vacancy Rate (%)") #%>% 
  #separate(Electoral.Division, c("ED", "Name"),
   #        extra = "merge")
  

t1  
  





View(t1)

t2 <- t1 %>%
  mutate(County.Name_=case_when(!grepl(x=Electoral.Division, pattern='^[0-9]') ~ Electoral.Division,
                                TRUE ~ NA_character_)) %>%
  extract(Electoral.Division,
          c("County.Sequence", "Division.Name", "County.Name"), "([0-9]{3}) (.+), Co\\. (.+)") %>%
  mutate(County.Name=case_when(is.na(County.Name) ~ County.Name_, TRUE ~ County.Name)) %>% 
  select(-County.Name_) %>% 
  filter(!is.na(County.Sequence),
         !is.na(Division.Name))



t2 <- t1 %>% 
  mutate(County.Name_=case_when(!grepl(x=Electoral.Division, pattern='^[0-9]') ~ Electoral.Division,
                              TRUE ~ NA_character_)) %>%
  extract(Electoral.Division,
          c("County.Sequence", "Division.Name", "County.Name"), "([0-9]{3}) (.+), (.+)") %>%
  mutate(County.Name=case_when(is.na(County.Name) ~ County.Name_, TRUE ~ County.Name)) %>% 
  select(-County.Name_) %>% 
  filter(!is.na(County.Sequence),
         !is.na(Division.Name)) 




t2
t0 <- t2 %>% 
  group_by(County.Name) %>%
  summarise(county_mean= mean(value, na.rm=T),
            county_sd = sd(value, na.rm=T),
            county_n = n()) %>%
  mutate(county_se = county_sd/sqrt(county_n)) %>% 
  ungroup() %>% 
  filter(county_n>1)




t0 %>%
  ggplot(aes(x=county_n,
             y=county_mean,
             label=County.Name))+
  geom_point()+
  geom_errorbar(aes(ymin=county_mean-(county_se*1.96),
                    ymax=county_mean+(county_se*1.96)))+
  geom_hline(yintercept = 12.8, alpha=0.3, size=3, colour='red')+
  geom_text_repel(alpha=0.3)



t2

m2 <- lmer(value ~  (1 | County.Name), t2)

summary(m2)

tidy(m2)
augment(m2, stderr=T)

augment(m2, newdata=NULL, conf.int=T)
tidy(x = m2, conf.int = TRUE)

 

q1 <- augment(m1) %>% 
  rename(fitted_fi = .fitted) %>% 
  select(value, County.Name, fitted_fi, .se.fit) %>% 
  group_by(County.Name) %>% 
  summarise(county_mean_fi = mean(fitted_fi))




q2 <- augment(m2) %>% 
  rename(fitted_ri=.fitted) %>% 
  select(value, County.Name, fitted_ri, .resid) %>% 
  group_by(County.Name) %>% 
  summarise(county_mean_ri = mean(fitted_ri))



q1
q2

q3 <- q2 %>% 
  left_join(q1) %>% 
  gather(key="model",
         value="value",
         county_mean_ri,
         county_mean_fi) 


q3 %>% 
  ggplot(aes(x=fct_reorder(factor(County.Name), value),
             y=value))+
  geom_point(aes(colour=model))+
  coord_flip()+
  geom_hline(yintercept = 14.5)


m1  <- lm(value~ County.Name, data=t2)

augment(m1, conf.int=T) %>% 
  group_by(County.Name) %>% 
  summarise(county_est=mean(.fitted),
            country_se=mean(.se.fit)) #%>% 
  ggplot(aes(x= fct_reorder(factor(County.Name), county_est),
             y=county_est))+
  geom_point()+
  geom_errorbar(aes(ymin=county_est-country_se,
                    ymax=county_est+country_se))+
  coord_flip()+
  theme_light()




t2.m %>%
  group_by(County.Name) %>% 
  mutate( county_n = n()) %>% 
  ungroup() %>% 
  ggplot(aes(x=county_n,
             y=.fitted))+
  geom_point()+
  geom_hline(yintercept = t2.m$.fixed, alpha=0.3, size=3, colour='red')




p2 <- t2.m %>%
  group_by(County.Name) %>% 
  summarise(county_mean= mean(.fitted, na.rm=T),
            county_n=n()) %>%  
  ungroup() %>% 
  ggplot(aes(x=county_n,
             y=county_mean,
             label=County.Name))+
  geom_point()+
  geom_hline(yintercept = t2.m$.fixed, alpha=0.3, size=3, colour='red')+
  geom_text_repel()


p1 <- t0 %>%
  ggplot(aes(x=county_n,
             y=county_mean,
             label=County.Name))+
  geom_point()+
  geom_errorbar(aes(ymin=county_mean-(county_se*1.96),
                    ymax=county_mean+(county_se*1.96)))+
  geom_hline(yintercept = 12.8, alpha=0.3, size=3, colour='red')+
  geom_text_repel(alpha=0.3)



p1 + p2

t2 %>% 
  filter(County.Name==contains("Dublin"))

t2 %>% 
  count(County.Name) %>% 
  arrange(desc(n)) %>% 
  head(20)


right1 <- t1 %>% 
  separate(Electoral.Division, c("ED", "Name"),
           extra = "merge") %>% 
  filter(!is.na(Name))
  

left1 <- t1 %>% 
  separate(Electoral.Division, c("ED", "Name"),
           extra = "merge") %>% 
   filter(is.na(Name),
          ED!="State")  %>% 
    mutate(Name = ED) %>% 
    select(-ED) %>% 
    rownames_to_column(var="ED") %>% 
    rename(county_mean=value) 


left1$ED <- parse_double(left1$ED)
right1$ED <- parse_double(right1$ED)

left1
right1

View(right1)
left1 %>% 
  left_join(right1, by=ED)

left1
right1

View(statbank)

admin_counties


library(dplyr)

df <- data.frame(x = c(NA, "a.b", "a.d", "b.c"))
df

df %>% separate(x, c("A", "B"))
#>      A    B
#> 1 <NA> <NA>
#> 2    a    b
#> 3    a    d
#> 4    b    c

# If you just want the second variable:
df %>% separate(x, c(NA, "B"))
#>      B
#> 1 <NA>
#> 2    b
#> 3    d
#> 4    c

# If every row doesn't split into the same number of pieces, use
# the extra and fill arguments to control what happens
df <- data.frame(x = c("a", "a b", "a b c", NA))

df
df %>% separate(x, c("a", "b"))
#> Warning: Expected 2 pieces. Additional pieces discarded in 1 rows [3].
#> Warning: Expected 2 pieces. Missing pieces filled with `NA` in 1 rows [1].
#>      a    b
#> 1    a <NA>
#> 2    a    b
#> 3    a    b
#> 4 <NA> <NA>
# The same behaviour drops the c but no warnings
df %>% separate(x, c("a", "b"), extra = "drop", fill = "right")
#>      a    b
#> 1    a <NA>
#> 2    a    b
#> 3    a    b
#> 4 <NA> <NA>
# Another option:
df %>% separate(x, c("a", "b"), extra = "merge", fill = "left")
#>      a    b
#> 1 <NA>    a
#> 2    a    b
#> 3    a  b c
#> 4 <NA> <NA>
# Or you can keep all three
df %>% separate(x, c("a", "b", "c"))
#> Warning: Expected 3 pieces. Missing pieces filled with `NA` in 2 rows [1, 2].
#>      a    b    c
#> 1    a <NA> <NA>
#> 2    a    b <NA>
#> 3    a    b    c
#> 4 <NA> <NA> <NA>

# If only want to split specified number of times use extra = "merge"
df <- data.frame(x = c("x: 123", "y: error: 7"))
df %>% separate(x, c("key", "value"), ": ", extra = "merge")
#>   key    value
#> 1   x      123
#> 2   y error: 7

# Use regular expressions to separate on multiple characters:
df <- data.frame(x = c(NA, "a?b", "a.d", "b:c"))
df %>% separate(x, c("A","B"), sep = "([\\.\\?\\:])")
#>      A    B
#> 1 <NA> <NA>
#> 2    a    b
#> 3    a    d
#> 4    b    c

# convert = TRUE detects column classes
df <- data.frame(x = c("a:1", "a:2", "c:4", "d", NA))
df %>% separate(x, c("key","value"), ":") %>% str
#> Warning: Expected 2 pieces. Missing pieces filled with `NA` in 1 rows [4].
#> 'data.frame':	5 obs. of  2 variables:
#>  $ key  : chr  "a" "a" "c" "d" ...
#>  $ value: chr  "1" "2" "4" NA ...
df %>% separate(x, c("key","value"), ":", convert = TRUE) %>% str
#> Warning: Expected 2 pieces. Missing pieces filled with `NA` in 1 rows [4].
#> 'data.frame':	5 obs. of  2 variables:
#>  $ key  : chr  "a" "a" "c" "d" ...
#>  $ value: int  1 2 4 NA NA

# Argument col can take quasiquotation to work with strings
var <- "x"
df %>% separate(!!var, c("key","value"), ":")
#> Warning: Expected 2 pieces. Missing pieces filled with `NA` in 1 rows [4].
#>    key value
#> 1    a     1
#> 2    a     2
#> 3    c     4
#> 4    d  <NA>
#> 5 <NA>  <NA>