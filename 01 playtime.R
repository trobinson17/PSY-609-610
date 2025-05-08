# load packages----
library(tidyverse)
library(haven)
library(sjmisc)
library(janitor)
library(psych)

# name and import the dataset----
pt <- read_csv("Shared_Data.csv", na="NA", col_names=TRUE)

# view the dataset
View(pt)

# mean and sd and plot----
# mean, sd, and histogram of mindfulness
mean(pt$mindful_avg)
sd(pt$mindful_avg)
hist(pt$machiavell_avg)

# descriptive statistics----
describe(pt)

# slice to remove rows----
# take note of parentheses
ptslice <- pt %>% slice(-(1:180))
View(ptslice)

# select----
# select to remove columns
ptselect <- pt %>% select(impulse_avg, e_bfi:c_bfi)
View(ptselect)

# rearrange----
ptrelocate <- pt %>% relocate(101:105)
View(ptrelocate)

# rename----
# syntax note: new name first, old name last
ptrename <- pt %>% rename(impulse_mean = impulse_avg)
View(ptrename)

# filtering----
# filter out; keep everyone under the age of 45
ptfilter <- pt %>% filter(age < 45)
hist(pt$age)
hist(ptfilter$age)

# mutate---- 
pt_mut <-pt %>% mutate(age_z = std(age))
describe(pt_mut$age_z)

# row means----
pt_avg <- pt %>% row_means(4:8, n = 3, var = "personality_avg")
hist(pt_avg$personality_avg)

# recode marital status----
pt_recode <- pt %>% mutate(marital_status = rec(marital_status, 
                                                rec = "1=0; 2,3,4,5=1"))
table(pt_recode$marital_status)

# drop NAs----
pt_nona <- pt %>% drop_na(gender_text)

# stack your functions----
ptfinal <- read_csv("Shared_Data.csv") %>%
  clean_names() %>%
  slice(-(1:10)) %>%
  filter(age > 25) %>%
  select(4:8) %>%
  row_means(1:4, n=2, var = "personality_avg") %>%
  select(personality_avg) %>%
  rowid_to_column(var = "id_row")
View(ptfinal)

# Export----
write_csv(ptfinal, "playtime_stacked.csv")