library(tidyverse)

theme_set(theme_classic())

df <- read_csv("Food_Systems_Project/smith_analysis/smith_data.csv") |> 
  janitor::clean_names()

#select columns for temporary analysis
df_use <- df |> 
  select(year, category, vendor, product = description, local, local_description, spend = cost) |> 
  mutate(category = str_to_title(category))



#look at spend by category
categ_spend <- df_use %>% 
  group_by(category) %>% 
  summarize(sum_spend = sum(spend))

#total spend
total_spend <- sum(df_use$spend)

##create expenditure by category plot
#chart of expenditure by category
df_ov1 <- df_use |> 
  group_by(category) %>% 
  summarize(spend_total = sum(spend), 
            spend_prop = sum(spend)/total_spend) %>% 
  mutate(perc_label = ifelse(spend_prop > 0.02, 
                             paste0(as.character(round(spend_prop, 2)*100), "%"), 
                             "")) %>% 
  mutate(full_label = paste0(category, " (", perc_label, ")")) %>% 
  mutate(x_val = "Spend") %>% 
  mutate(category = fct_reorder(category, spend_prop)) 
df_ov1 %>% 
  ggplot(aes(x = x_val, y = spend_prop, fill = category)) +
  geom_bar(position = "fill", stat = "identity") +
  geom_text(aes(label = perc_label), position = position_fill(vjust = 0.5)) +
  #scale_fill_manual(values = c("#0f9ce2", "grey65")) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(y = "Proportion of spend", x = NULL, title = "Expenditure by Category", 
       fill = "Category") 

#local expenditure
df_loc <- df_use %>% 
  mutate(local_description = ifelse(grepl("Independently", local_description), 
                                    "Independently or Cooperatively Owned within 250 mi", 
                                    local_description)) |> 
  group_by(local_description) %>% 
  summarize(spend_total = sum(spend), 
            spend_prop = sum(spend)/total_spend) %>% 
  mutate(perc_label = ifelse(spend_prop > 0.02, 
                             paste0(as.character(round(spend_prop, 2)*100), "%"), 
                             "")) %>% 
  mutate(full_label = paste0(local_description, " (", perc_label, ")")) %>% 
  mutate(x_val = "Spend") %>% 
  mutate(local_description = ifelse(is.na(local_description), "NA",
                                    local_description)) |>
  mutate(local_description = str_wrap(local_description, width = 30)) %>% 
  mutate(local_description = fct_reorder(local_description, spend_prop)) 
df_loc %>% 
  ggplot(aes(x = x_val, y = spend_prop, fill = local_description)) +
  geom_bar(position = "fill", stat = "identity") +
  geom_text(aes(label = perc_label), position = position_fill(vjust = 0.5)) +
  scale_fill_manual(values = c("#fcbb30", "#eb3f67", "#8545d8", "grey65")) + 
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank() ) +
  labs(y = "Proportion of spend", x = NULL, title = "Local Expenditure", 
       fill = "Category")

#local by category 
df_loc_c <- df_use %>% 
  mutate(local_description = ifelse(grepl("Independently", local_description), 
                                    "Independently or Cooperatively Owned within 250 mi", 
                                    local_description)) |> 
  group_by(local_description, category) %>% 
  summarize(spend_across = sum(spend)) %>% 
  group_by(category) %>% 
  transmute(local_description, spend_prop = spend_across/sum(spend_across), 
            spend_total = spend_across) %>% 
  mutate(perc_label = ifelse(spend_prop > 0.02, 
                             paste0(as.character(round(spend_prop, 2)*100), "%"), 
                             "")) %>% 
  mutate(full_label = paste0(local_description, " (", perc_label, ")")) %>% 
  left_join(categ_spend, by = "category") %>% 
  arrange(sum_spend) |> 
  mutate(local_description = ifelse(is.na(local_description), "NA",
                                    local_description)) |>
  mutate(local_description = str_wrap(local_description, width = 30)) 
order_loc <- unique(as.vector(df_loc_c$category))
df_loc_c$category <- factor(df_loc_c$category, order_loc)

#counts
df_loc_c %>% 
  ggplot(aes(x = category, y = spend_total/1000, fill = local_description)) +
  geom_bar(stat = "identity") +
  #geom_text(aes(label = spend_total), position = position_fill(vjust = 0.5)) +
  scale_fill_manual(values = c("#fcbb30", "#eb3f67", "#8545d8", "grey65")) +
  labs(y = "Spend (1000s of Dollars)", x = "Category", 
       title = "Local Expenditure by Category", 
       fill = "Category") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
