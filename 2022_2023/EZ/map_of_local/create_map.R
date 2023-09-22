library(tidyverse)
library(sf)
library(maps)
# library(usmap)
library(readxl)

# plot_usmap(include = "MA") +
#   labs(title = "New England Region") 

coords <- read_xlsx("Food_Systems_Project/map_of_local/local_coords.xlsx")

ma_map <- maps::map("state", regions = "massachusetts",
                    plot = FALSE, fill = TRUE) %>% 
  st_as_sf()

ggplot(data = ma_map) +
  geom_sf(fill = "white", color = "black") +
  geom_point(data = coords, mapping = aes(x = Latitude, y = Longitude)) +
  geom_label(data = coords, 
             mapping = aes(x = Latitude, y = Longitude, label = Name), 
             nudge_y = -0.09) +
  theme_void()
