library(tidyverse)
library(stringr)

smith_data <- read_csv("food_systems/smith_data.csv") %>% janitor::clean_names()

smith_total_spend <- sum(smith_data$cost)

smith_wlocal <- smith_data %>% 
  filter(!is.na(fair_description)|!is.na(humane_description)|!is.na(ecological_description)|!is.na(local_description))

lfs <- sum(smith_wlocal$cost)/smith_total_spend # = 0.33

smith_rf <- smith_data %>% 
  filter(!is.na(fair_description)|!is.na(humane_description)|!is.na(ecological_description))

rfs <- sum(smith_rf$cost)/smith_total_spend # = 0.124

smith_vendors <- smith_wlocal %>% 
  group_by(label_brand) %>% 
  select('category', 'label_brand', 'ecological_description', 'fair_description', 
         'humane_description', 'local_description', )

srs <- smith_wlocal %>% 
  group_by(label_brand) %>% 
  summarise(total_spend = sum(cost)) %>% 
  arrange(desc(total_spend))

v <- smith_vendors[!duplicated(smith_vendors[,c('label_brand')]),]

vl <- v %>% 
  filter(!is.na(local_description)) %>% 
  left_join(srs) %>% 
  arrange(desc(total_spend))

vr <- v %>% 
  filter(!is.na(fair_description)|!is.na(humane_description)|!is.na(ecological_description)) %>% 
  left_join(srs) %>% 
  arrange(desc(total_spend))
