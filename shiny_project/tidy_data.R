library(RSocrata)
library(tidyverse)
library(choroplethrZip)

with_detail = read.socrata("https://data.cityofnewyork.us/resource/ibte-hq4u.csv")

fire_data = with_detail %>% 
  select(incident_date_time, incident_type_desc, 
         property_use_desc, zip_code, borough_desc, -property_use_desc) %>% 
  separate(borough_desc, sep = "- ", into = c("n", "borough")) %>% 
  select(-n) %>% 
  mutate(incident_type = str_sub(incident_type_desc, 1, 3), 
         incident_type = as.integer(incident_type),
         incident_hour = as.integer(substr(incident_date_time, 12, 13)),
         month = as.integer(substr(incident_date_time, 6, 7))) %>%
  filter(incident_type < 165) %>%
  select(-incident_type_desc, -incident_date_time, -incident_type)

fire_data = fire_data %>%
  mutate(month = str_replace(month, "3", "Spring"),
         month = str_replace(month, "4", "Spring"),
         month = str_replace(month, "5", "Spring"),
         month = str_replace(month, "6", "Summer"),
         month = str_replace(month, "7", "Summer"),
         month = str_replace(month, "8", "Summer"),
         month = str_replace(month, "9", "Autumn"),
         month = str_replace(month, "10", "Autumn"),
         month = str_replace(month, "11", "Autumn"),
         month = str_replace(month, "12", "Winter"),
         month = str_replace(month, "1", "Winter"),
         month = str_replace(month, "2", "Winter"),
         month = fct_relevel(month, "Spring"))


zip_map_data = fire_data %>% 
  mutate(zip_code = str_sub(zip_code, 1, 5)) %>% 
  filter(zip_code != "" & zip_code != "99999" & 
         zip_code != "10691" & zip_code != "11251") %>% 
  rename(region = zip_code) %>%
  select(-incident_hour)

write.csv(zip_map_data, file = "data/zip_map_data_tidy.csv", row.names=FALSE)

fire_data = fire_data %>%
  select(-zip_code)

write.csv(fire_data, file = "data/fire_data_tidy.csv", row.names=FALSE)