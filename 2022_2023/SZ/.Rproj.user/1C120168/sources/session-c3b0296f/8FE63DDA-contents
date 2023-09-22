library(tidyverse)
library(bslib)
library(stringr)
library(openxlsx)

df1 <- read_csv('/Users/sara/Desktop/R/sustainability/food_systems/food_dashboard/food_data.csv')
df2 <- read.xlsx('/Users/sara/Desktop/R/sustainability/food_systems/data/all_distributors.xlsx') %>% 
  janitor::clean_names() %>% 
  select(product = "product_name", 
         brand = "vendor_name", 
         "code")       

df <- read.xlsx("food_2023.xlsx") 
colnames(df) <- df[1, ]
# dates: 4/22-3/23
df <- df[-1, ]
df <- df %>% janitor::clean_names()
df <- df[, c(8:25)]
df <- df %>% select(product = "product_description",
                  attributes = "ingredient_attribute_local_organic_etc_if_available",
                  sales = "sales_4_22_3_23",
                  code = "gtin_if_available",
                  "distributor_product_code", 
                  "manufacturer_name",
                  "brand_name",
                  "miles_from_college",
                  brand_address = "brand_or_farm_street_address_to_calculate_mileage_to_client_location",
                  "brand_city", 
                  "brand_state",
                  "brand_zip",
                  cases = "cases_4_22_3_23",
                  pack_size = "pack_size_e_g_4",
                  item = "item_e_g_5",
                  item_unit = "item_uom_e_g_lb") %>% 
  mutate(sales = round(as.numeric(sales), 2),
         miles_from_college = as.numeric(miles_from_college)) 
  
pb_att <- paste('plant based', 'plant based unprocessed or minimally processed', 'Processed Culinary Ingredients', 
                'Simple Processed Foods', 'plant based vegetarian', sep = '|')

eco_att <- paste("Organic", "USDA Organic", "Rainforest Alliance Certified", "MSC Certified", 
                 "Monterey Bay Aquarium Seafood Watch - Good Alternative", "Monterey Bay - Good Alternative", "Fair Trade USA Certified", sep = '|')
business_cert_att <- paste('Minority Owned', 'Woman Owned', "Certified B Corp", "ESOP 100% Employee Owned", sep = '|')

df <- df %>% 
  mutate(plant_based = ifelse(grepl(pb_att, attributes, ignore.case = TRUE), TRUE, FALSE),
         eco_certified = ifelse(grepl(eco_att, attributes, ignore.case = TRUE), TRUE, FALSE),
         business_certified = ifelse(grepl(business_cert_att, attributes, ignore.case = TRUE), TRUE, FALSE))

food_spend <- df %>% 
  filter(!manufacturer_name %in% c('WORLD CENTRIC', 'SLVR SRC', 'ECOLAB', 'GRNWARE', 'FRST MRK'))

spend_pct <- function(df, total_spend){
  category_spend <- round(sum(df$sales, na.rm = TRUE), 1)
  spend_pct <- round(category_spend/total_spend, 3)
  return(c(category_spend, spend_pct))
}


#####

total_dining_spend <- sum(df$sales, na.rm = TRUE) # $3,181,018
total_food_spend <- sum(food_spend$sales, na.rm = TRUE) # $3,181,018

## STARS categories
pb_items <- food_spend %>% filter(plant_based == TRUE)
pb <- spend_pct(df = pb_items, total_spend = total_food_spend)
pb_total <- pb[1] # $1,276,122
pb_pct <- pb[2] # 40.1%

eco_items <- food_spend %>% filter(eco_certified == TRUE)
eco <- spend_pct(df = eco_items, total_spend = total_food_spend)
eco_total <- eco[1] # $88,286
eco_pct <- eco[2] # 2.8%

soc_impact_items <- df %>% filter(business_certified == TRUE)
soc_impact <- spend_pct(df = soc_impact_items, total_spend = total_dining_spend)
soc_impact_total <- soc_impact[1] # $508,512
soc_impact_pct <- soc_impact[2] # 14.6%

#####

# miles info isn't accurate so ignore local category for now
local <- food_spend %>% filter(miles_from_college <= 250)
local_spend <- sum(local$sales, na.rm = TRUE) # $1,158,728

#####

## categories

# animal protein
meats <- df1 %>% 
  filter(category == 'Animal Protein') %>% 
  mutate(item = gsub("([A-Za-z]+).*", "\\1", product) %>% tolower())

meat_items <- gsub("([A-Za-z]+).*", "\\1", meats$product) %>% tolower() %>% unique()
meat_items <- meat_items[-which(meat_items == 'bbq')]
meat_list <- paste(meat_items, collapse = '|')
  
df_meat <- food_spend %>% 
  filter(grepl(meat_list, product, ignore.case = TRUE)) %>% 
  filter(plant_based == FALSE)

meat <- spend_pct(df = df_meat, total_spend = total_dining_spend)
meat_total <- meat[1] # $814,155
meat_pct <- meat[2] # 23.3%

###
# join old dataset 
#
# df4 <- select(df, c('product', 'brand' = 'brand_name', 'code', 'sales')) %>% 
#   mutate(product = as.character(str_trim(product)),
#          code = as.character(str_trim(code)),
#          brand = as.character(str_trim(brand))) %>% 
#   group_by(product, brand, code) %>% 
#   summarize(sales = sum(sales))
# distinct(product, .keep_all = TRUE)
# 
# non_food <- df3 %>% filter(is.na(manufacturer))
# 
# df3 <- df2 %>% left_join(df1, by = c('product', 'brand'))
# df5 <- df4 %>% inner_join(select(df3, -product), by = 'code')
# df6 <- df3 %>% mutate(code = paste0('00', code)) %>% inner_join(select(df4, -product), by = 'code')
# df61 <- df3 %>% mutate(code = paste0('000', code)) %>% inner_join(select(df4, -product), by = 'code')
# df7 <- df4 %>% inner_join(df3, by = c('product', 'brand'))
# df8 <- df5 %>% rbind(df6) %>% rbind(df61) %>% anti_join(df7, by = 'product')
# df9 <- df7 %>% plyr::rbind.fill(df8) %>% distinct()
###
