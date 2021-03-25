
library(tidyverse)
library(lubridate)

df_toores <- read.csv("https://opendata.digilugu.ee/opendata_covid19_test_results.csv")

df_toores %>% 
  mutate(Gender = factor(Gender),
         County = factor(County),
         ResultValue = factor(ResultValue),
         ResultTime = date(ResultTime),
         StatisticsDate = ymd(StatisticsDate)) %>% 
  mutate(agegroup=factor(AgeGroup, levels=c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29",
                                            "30-34", "35-39", "40-44", "45-49", "50-54", "55-59",
                                            "60-64", "65-69", "70-74", "75-79", "80-84", "üle 85"))) %>% 
  mutate(agegroup10=fct_recode(agegroup,
                               "0-9" = "0-4", 
                               "0-9" = "5-9",  
                               "10-19" = "10-14", 
                               "10-19" = "15-19", 
                               "20-29" = "20-24", 
                               "20-29" = "25-29",
                               "30-39" = "30-34", 
                               "30-39" = "35-39", 
                               "40-49" = "40-44", 
                               "40-49" = "45-49", 
                               "50-59" = "50-54", 
                               "50-59" = "55-59",
                               "60-69" = "60-64", 
                               "60-69" = "65-69", 
                               "70-79" = "70-74", 
                               "70-79" = "75-79", 
                               "80+" = "80-84", 
                               "80+" = "üle 85")) %>% 
  mutate(week=week(ResultTime)) %>% 
  as_tibble() -> df



df %>% 
  filter(ResultValue=="P") %>% 
  filter(ResultTime > "2021-01-01") %>% 
  filter(agegroup10=="10-19") %>%
  group_by(ResultTime) %>%
  tally() %>%
  mutate(muutus=ifelse(ResultTime > "2021-03-11", "Enne", "Pärast")) %>% 
  ggplot(aes(x=ResultTime, y=n, group = muutus))+
  geom_line()+
  geom_vline(xintercept=unclass(as.Date("2021-03-11")),  col = "red")+
  geom_smooth(method = "lm")+
  labs(x="kuupäev", y="positiivsete testide arv", title = "10-19 vanusegrupi positivsete testide arv")+
  annotate("text", x=as.Date("2021-03-04"), y=2, label="Distantsõppe algus:", color="red")

