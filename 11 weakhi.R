# load packages
library(tidyverse)
library(easystats)
library(psych)
library(stats)
library(janitor)

library(effects)
library(car)
library(jtools)

library(gridExtra)
library(paletteer)
library(extrafont)
library(cowplot)

# load data
st <- read_csv("cstressuncg_clean.csv") %>%
  select(`PSS Self`, `PSS Avg. UNCG`, `Resilience`, 
         `Direct Avg. UNCG`) %>% datawizard::center(1:3) %>%
  clean_names()

# regression 
m1 <- lm(direct_avg_uncg ~ pss_avg_uncg, data=st)
parameters(m1, standardize = "refit")
m2 <- lm(direct_avg_uncg ~ ., data=st)
parameters(m2, standardize = "refit")

# model comparison 
cm <- compare_models(m1,m2)
cp <- compare_performance(m1,m2, metrics=c("R2", "R2_adj"))
wald <- test_wald(m1,m2)