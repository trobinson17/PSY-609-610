# load packs----
# call on pacakges
library(tidyverse)
library(haven)
library(sjmisc)
library(janitor)
library(psych)

# clean ----
# import and clean data in one fell swoop
playset <- read_csv("RAW_Shared_Data_Prep.csv") %>% 
  mutate_if(is.character,as.numeric) %>%
  clean_names() %>%                            
  slice(-(1:2)) %>%                                
  filter(age < 50) %>%                   
  select(own_firearm, consume_alcohol, consume_caffeine, meals_week, 
         accidents_count) %>%      
  rename(firearm = own_firearm, alcohol = consume_alcohol, caffeine =
           consume_caffeine, meals = meals_week, accidents = accidents_count) %>%                  
  mutate(alcohol_z = std(alcohol), caffeine_z =
           std(caffeine)) %>%
  row_means(1:4, n=2, var = "problems") %>%
  select(firearm, alcohol_z, caffeine_z, meals, problems, accidents) %>%
  rowid_to_column(var = "id_row")       
View(playset)

# export ----
# write a csv and store it in subfolder titled "HW Data"
write_csv(playset, "HW Data/scrubdub_TR.csv", col_names = TRUE)