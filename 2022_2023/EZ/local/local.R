library(tidyverse)
library(readxl)
library(sf)
library(maps)

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

#list of local
local_vendors <- df |> 
  filter(local != "NA") |> 
  group_by(vendor) |> 
  summarize(sum_spend = sum(spend)) |> 
  arrange(desc(sum_spend))
write_csv(local_vendors, "Food_Systems_Project/local/local_vendors.csv")

vendors_vec <- unique(local_vendors$vendor)
write_csv(as_tibble(vendors_vec), "Food_Systems_Project/vendor_list.csv")

#map
map_info <- read_csv("Food_Systems_Project/local/map_info.csv")

ma_map <- maps::map("state", regions = c("pennsylvania", "new york", "new hampshire", 
                                         "vermont", "connecticut", "massachusetts", "maine", 
                                         "rhode island"),
                    plot = FALSE, fill = TRUE) %>% 
  st_as_sf()

# usa <- maps::map("state", boundary=FALSE, col="gray", plot=FALSE, fill= TRUE) |> 
#   st_as_sf()

ggplot(data = ma_map) +
  geom_sf(fill = "white", color = "black") +
  geom_point(data = map_info, mapping = aes(x = Y, y = X)) +
  # geom_label(data = coords, 
  #            mapping = aes(x = Latitude, y = Longitude, label = Name), 
  #            nudge_y = -0.09) +
  theme_void()

map_info_cut <- map_info |> 
  filter(X > 41) |> 
  filter(X < 44)

ma_map2 <- maps::map("state", regions = c("connecticut", "massachusetts", 
                                         "rhode island"),
                    plot = FALSE, fill = TRUE) %>% 
  st_as_sf()

ggplot(data = ma_map2) +
  geom_sf(fill = "white", color = "black") +
  geom_point(data = map_info_cut, mapping = aes(x = Y, y = X)) +
  # geom_label(data = coords, 
  #            mapping = aes(x = Latitude, y = Longitude, label = Name), 
  #            nudge_y = -0.09) +
  theme_void()

#leaflet map
library(leaflet)
greenLeafIcon <- makeIcon(
  iconUrl = "https://leafletjs.com/examples/custom-icons/leaf-green.png",
  iconWidth = 20, iconHeight = 35,
  iconAnchorX = 0, iconAnchorY = 0,
  shadowUrl = "https://leafletjs.com/examples/custom-icons/leaf-shadow.png",
  shadowWidth = 30, shadowHeight = 30,
  shadowAnchorX = 1, shadowAnchorY = 15
)
leaflet(map_info) |> 
  addProviderTiles(providers$CartoDB.Positron) |> 
  addMarkers(lng = ~Y, lat = ~X, icon = greenLeafIcon)

