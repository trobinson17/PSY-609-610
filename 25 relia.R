# load packs
library(tidyverse)
library(easystats)
library(skimr)
library(psych)

# load data 
cs <- read_csv("reliability_data.csv")
conf_i <- cs %>% select(starts_with("conf"))

# estimate alpha and omega
conf_alpha <- alpha(conf_i)$total$std.alpha 
conf_alpha
conf_omega <- omega(conf_i, fm = "ml")
conf_omega$omega_h
conf_omega$omega.tot

# add systemic racism items 
conf_s_i <- cs %>% select(sys1_1_r:conf5_r)
conf_s_alpha <- alpha(conf_s_i, check.keys = TRUE)$total$std.alpha
conf_s_alpha
conf_s_omega <- omega(conf_s_i, fm = "ml")
conf_s_omega$omega_h
conf_s_omega$omega.tot