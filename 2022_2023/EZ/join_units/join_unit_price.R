library(tidyverse)
library(readxl)

df <- read_csv("Food_Systems_Project/old_data/all_orders.csv")
sku <- read_excel("Food_Systems_Project/join_units/skus-export.xlsx")

sku2 <- sku |> 
  janitor::clean_names() |> 
  select(product = product_name, unit_price = base_price)

comb <- df |> 
  inner_join(sku2, by = " product") |> 
  mutate(num_units = spend/unit_price) |> 
  mutate(num_units_round = round(num_units))

write_csv(comb, "Food_Systems_Project/join_units/all_orders2.csv")

master <- read_csv("Food_Systems_Project/join_units/master.csv")
sku <- read_excel("Food_Systems_Project/join_units/skus-export.xlsx")

sku2 <- sku |> 
  janitor::clean_names() |> 
  select(product = product_name, unit_price = base_price)

sku3 <- sku2[!duplicated(sku2$product),]

comb2 <- master |> 
  left_join(sku3, by = "product") |> 
  mutate(num_units = spend/unit_price)

write_csv(comb2, "Food_Systems_Project/join_units/master_with_units.csv")
