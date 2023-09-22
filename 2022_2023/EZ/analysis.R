library(tidyverse) 
library(readxl)

theme_set(theme_classic())

df <- read_xlsx("Food_Systems_Project/all_orders_manual_clean.xlsx")

df[is.na(df)] <- "NA"

total_spend <- sum(df$spend)

#create different categories of expenditure
unique(df$category)

df <- rename(df, pb = plant_based)

df <- df %>% 
  mutate(category_mod = 
           dplyr::case_when(
             category == "Fruit" ~ "Produce",
             category == "Apples" ~ "Produce", 
             category == "Broccoli" ~ "Produce",
             category == "Berries" ~ "Produce",
             category == "Cabbage" ~ "Produce",
             category == "Cauliflower" ~ "Produce",
             category == "Leafy Greens" ~ "Produce",
             category == "Melons" ~ "Produce",
             category == "Onions/Garlic" ~ "Produce",
             category == "Vegetables" ~ "Produce",
             category == "Root Vegetables" ~ "Produce",
             category == "Squash" ~ "Produce",
             category == "Poultry" ~ "Meat",
             category == "Frozen" ~ "Grocery/Staple",
             category == "Other" ~ "Produce",
             category == "Canned and Dry" ~ "Grocery/Staple",
             category == "Other Beverages (Non Dairy)" ~ "Beverages",
             TRUE ~ category
           ))

unique(df$category_mod)


#look at breakdowns of expenditure by new categories
categ_spend <- df %>% 
  group_by(category_mod) %>% 
  summarize(sum_spend = sum(spend))

# t <- df %>% 
#   filter(category_mod == "Other")

#plant-based by category 
df_pb_c <- df %>% 
  group_by(pb, category_mod) %>% 
  summarize(spend_across = sum(spend)) %>% 
  group_by(category_mod) %>% 
  transmute(pb, spend_prop = spend_across/sum(spend_across), 
            spend_total = spend_across) %>% 
  mutate(perc_label = ifelse(spend_prop > 0.02, 
                             paste0(as.character(round(spend_prop, 2)*100), "%"), 
                             "")) %>% 
  mutate(full_label = paste0(pb, " (", perc_label, ")")) %>% 
  mutate(pb = factor(pb)) %>% 
  mutate(pb = fct_relevel(pb, c("Veg Alternatives to Meat/Dairy", 
                                "Processed Culinary Ingredients", "Simple Processed Foods",
                                "Minimal (or No) Processing", 
                                "NA"))) %>% 
  left_join(categ_spend, by = "category_mod") %>% 
  arrange(sum_spend)
order <- unique(as.vector(df_pb_c$category_mod))
df_pb_c$category_mod <- factor(df_pb_c$category_mod, order)
#proportions
df_pb_c %>% 
  ggplot(aes(x = category_mod, y = spend_prop, fill = pb)) +
  geom_bar(position = "fill", stat = "identity") +
  geom_text(aes(label = perc_label), position = position_fill(vjust = 0.5)) +
  scale_fill_manual(values = c("#fcbb30", "#eb3f67", "#8545d8", "#0f9ce2", "grey65")) +
  labs(y = "Proportion of spend", x = "Category", 
       title = "Plant-Based Expenditure by Category", 
       fill = "Category") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
#counts
df_pb_c %>% 
  ggplot(aes(x = category_mod, y = spend_total/1000, fill = pb)) +
  geom_bar(stat = "identity") +
  #geom_text(aes(label = spend_total), position = position_fill(vjust = 0.5)) +
  scale_fill_manual(values = c("#fcbb30", "#eb3f67", "#8545d8", "#0f9ce2", "grey65")) +
  labs(y = "Spend (1000s of Dollars)", x = "Category", 
       title = "Plant-Based Expenditure by Category", 
       fill = "Category") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

#plant-based overall
df_pb <- df %>% 
  group_by(pb) %>% 
  summarize(spend_total = sum(spend), 
            spend_prop = sum(spend)/total_spend) %>% 
  mutate(perc_label = ifelse(spend_prop > 0.02, 
                             paste0(as.character(round(spend_prop, 2)*100), "%"), 
                             "")) %>% 
  mutate(full_label = paste0(pb, " (", perc_label, ")")) %>% 
  mutate(x_val = "Spend") %>% 
  mutate(pb = fct_reorder(pb, spend_prop))
df_pb %>% 
  ggplot(aes(x = x_val, y = spend_prop, fill = pb)) +
  geom_bar(position = "fill", stat = "identity") +
  geom_text(aes(label = perc_label), position = position_fill(vjust = 0.5)) +
  scale_fill_manual(values = c("#fcbb30", "#eb3f67", "#8545d8", "#0f9ce2", "grey65")) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank() ) +
  labs(y = "Proportion of spend", x = NULL, title = "Plant-Based Expenditure", 
       fill = "Category")

#plant-based pichart
df_pb |> 
  ggplot(aes(x="", y=spend_prop, fill=pb)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() # remove background, grid, numeric labels


#list of local

#local by category 
df_loc_c <- df %>% 
  filter(local != "Local") %>% 
  mutate(local = ifelse(local == "Local Vendors/Non-local Products", 
                        "Local Vendors/Non-Local Products", local)) %>% 
  group_by(local, category_mod) %>% 
  summarize(spend_across = sum(spend)) %>% 
  group_by(category_mod) %>% 
  transmute(local, spend_prop = spend_across/sum(spend_across), 
            spend_total = spend_across) %>% 
  mutate(perc_label = ifelse(spend_prop > 0.02, 
                             paste0(as.character(round(spend_prop, 2)*100), "%"), 
                             "")) %>% 
  mutate(full_label = paste0(local, " (", perc_label, ")")) %>% 
  # mutate(pb = fct_relevel(pb, c("Veg Alternatives to Meat/Dairy", 
  #                               "Processed Culinary Ingredients", "Simple Processed Foods",
  #                               "Minimal (or No) Processing", 
  #                               "NA"))) %>% 
  left_join(categ_spend, by = "category_mod") %>% 
  arrange(sum_spend) 
order_loc <- unique(as.vector(df_loc_c$category_mod))
df_loc_c$category_mod <- factor(df_loc_c$category_mod, order_loc)
#proportions
df_loc_c %>% 
  ggplot(aes(x = category_mod, y = spend_prop, fill = local)) +
  geom_bar(position = "fill", stat = "identity") +
  geom_text(aes(label = perc_label), position = position_fill(vjust = 0.5)) +
  scale_fill_manual(values = c("#fcbb30", "#eb3f67", "#8545d8", "#0f9ce2", "grey65")) +
  labs(y = "Proportion of spend", x = "Category", 
       title = "Local Expenditure by Category", 
       fill = "Category") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
#counts
df_loc_c %>% 
  filter(category_mod != "Local") %>% 
  ggplot(aes(x = category_mod, y = spend_total/1000, fill = local)) +
  geom_bar(stat = "identity") +
  #geom_text(aes(label = spend_total), position = position_fill(vjust = 0.5)) +
  scale_fill_manual(values = c("#fcbb30", "#8545d8", "grey65")) +
  labs(y = "Spend (1000s of Dollars)", x = "Category", 
       title = "Local Expenditure by Category", 
       fill = "Category") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# df2 <- read_csv("Food_Systems_Project/old_data/all_orders.csv")

#local
df_loc <- df %>% 
  filter(local != "Local") %>%
  mutate(local = ifelse(local == "Local Products", 
                        "Local Vendors/Local Products", 
                        ifelse(local == "Non-Local Products", 
                               "Local Vendors/Non-Local Products",
                               local))) %>% 
  group_by(local) %>% 
  summarize(spend_total = sum(spend), 
            spend_prop = sum(spend)/total_spend) %>% 
  mutate(perc_label = ifelse(spend_prop > 0.02, 
                             paste0(as.character(round(spend_prop, 2)*100), "%"), 
                             "")) %>% 
  mutate(full_label = paste0(local, " (", perc_label, ")")) %>% 
  mutate(x_val = "Spend") %>% 
  mutate(local = fct_reorder(local, spend_prop)) |> 
  arrange(desc(local), desc(spend_prop)) |> 
  mutate(text_y = cumsum(spend_prop) - spend_prop/2)
df_loc %>% 
  ggplot(aes(x = x_val, y = spend_prop, fill = local)) +
  geom_bar(position = "fill", stat = "identity") +
  geom_text(aes(label = perc_label), position = position_fill(vjust = 0.5)) +
  scale_fill_manual(values = c("#fcbb30", "#8545d8", "grey65")) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank() ) +
  labs(y = "Proportion of spend", x = NULL, title = "Local Expenditure", 
       fill = "Category")

#local pi chart
ggplot(df_loc, aes(x="", y=spend_prop, fill=local)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values = c("#b48cb3", "#7daa7e", "grey75")) +
  labs(title = "Local Expenditure", 
       fill = "Category") +
  geom_label_repel(aes(label = perc_label, y = text_y), fill = "white",
                   nudge_x = 0.6) + #, position = position_fill(vjust = 1)
  coord_polar("y", start=0) +
  theme_void() # remove background, grid, numeric labels


#mwbe 
df %>% 
  group_by(mwbe) %>% 
  summarize(spend_total = sum(spend), 
            spend_prop = sum(spend)/total_spend) %>% 
  mutate(perc_label = ifelse(spend_prop > 0.02, 
                             paste0(as.character(round(spend_prop, 2)*100), "%"), 
                             "")) %>% 
  mutate(full_label = paste0(mwbe, " (", perc_label, ")")) %>% 
  mutate(x_val = "Spend") %>% 
  mutate(mwbe = str_wrap(mwbe, width = 30)) %>% 
  mutate(mwbe = fct_reorder(mwbe, spend_prop)) %>% 
  ggplot(aes(x = x_val, y = spend_prop, fill = mwbe)) +
  geom_bar(position = "fill", stat = "identity") +
  geom_text(aes(label = perc_label), position = position_fill(vjust = 0.5)) +
  scale_fill_manual(values = c("#fcbb30", "#8545d8", "#eb3f67", "#0f9ce2", "grey65")) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank() ) +
  labs(y = "Proportion of spend", x = NULL, title = "Certified Businesses Expenditure", 
       fill = "Category")

#certification 
df_c <- df %>% 
  mutate(certification = ifelse(certification == "NA", certification, "Certified")) %>% 
  group_by(certification) %>% 
  summarize(spend_total = sum(spend), 
            spend_prop = sum(spend)/total_spend) %>% 
  mutate(perc_label = ifelse(spend_prop > 0.02, 
                             paste0(as.character(round(spend_prop, 2)*100), "%"), 
                             "")) %>% 
  mutate(full_label = paste0(certification, " (", perc_label, ")")) %>% 
  mutate(x_val = "Spend") %>% 
  mutate(certification = str_wrap(certification, width = 30)) %>% 
  mutate(certification = fct_reorder(certification, spend_prop))
df_c %>% 
  ggplot(aes(x = x_val, y = spend_prop, fill = certification)) +
  geom_bar(position = "fill", stat = "identity") +
  geom_text(aes(label = perc_label), position = position_fill(vjust = 0.5)) +
  scale_fill_manual(values = c("#0f9ce2", "grey65")) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank() ) +
  labs(y = "Proportion of spend", x = NULL, title = "Certified Vendor Expenditure", 
       fill = "Category")

#certification types
df_c2 <- df %>% 
  filter(certification != "NA") 
cert_total <- sum(df_c2$spend)
df_c3 <- df_c2 %>% 
  mutate(certification = 
           ifelse(certification == 
                    "USDA Certified Organic, Rainforest Alliance Certified, B-Corp. Certified",
                  "Rainforest Alliance Certified, B-Corp. Certified, USDA Certified Organic", 
                  certification)) %>% 
  group_by(certification) %>% 
  summarize(spend_total = sum(spend), 
            spend_prop = sum(spend)/cert_total) %>% 
  mutate(perc_label = ifelse(spend_prop > 0.02, 
                             paste0(as.character(round(spend_prop, 2)*100), "%"), 
                             "")) %>% 
  mutate(full_label = paste0(certification, " (", perc_label, ")")) %>% 
  mutate(x_val = "Spend") %>% 
  mutate(certification = str_wrap(certification, width = 40)) %>% 
  mutate(certification = fct_reorder(certification, spend_prop)) 
df_c3 %>% 
  ggplot(aes(x = x_val, y = spend_prop, fill = certification)) +
  geom_bar(position = "fill", stat = "identity") +
  geom_text(aes(label = perc_label), position = position_fill(vjust = 0.5)) +
  #scale_fill_manual(values = c("#0f9ce2", "grey65")) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        legend.spacing.y = unit(0.3, 'cm')) +
  guides(fill = guide_legend(byrow = TRUE)) +
  labs(y = "Proportion of spend", x = NULL, title = "Certified Vendor Expenditure", 
       fill = "Category")

#top spend's in each NA category

# #pb
# df %>% 
#   filter(pb == "NA") %>% 
#   arrange(desc(spend)) %>% 
#   slice(1:10)

# #local
# local_top <- df %>% 
#   filter(local == "NA") %>% 
#   arrange(desc(spend)) %>% 
#   slice(1:10) 
# write_csv(local_top, "local_top.csv")

#overall
top <- df %>% 
  arrange(desc(spend)) %>% 
  slice(1:30) 
write_csv(top, "top_spend.csv")
