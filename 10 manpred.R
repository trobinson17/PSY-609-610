# load packages
library(tidyverse)
library(easystats)
library(psych)
library(sjmisc)

library(effects)
library(car)
library(jtools)

library(gridExtra)
library(paletteer)
library(extrafont)
library(cowplot)

# grab data
st <- read_csv("cstressuncg_clean.csv") %>%
  select(`Life Satisfaction`, `Self Esteem`, `Resilience`, 
         `Health_Behavior`) %>% datawizard::center(1:3)

# explore
std<-describe(st)
std

psych::lowerCor(st)
cor_test(x="Life Satisfaction", y = "Self Esteem", data=st)
cor_test(x="Life Satisfaction", y = "Resilience", data=st)
cor_test(x="Life Satisfaction", y = "Health_Behavior", data=st)
cor_test(x="Self Esteem", y = "Resilience", data=st)
cor_test(x="Self Esteem", y = "Health_Behavior", data=st)
cor_test(x="Resilience", y = "Health_Behavior", data=st)

# regression
m1 <- lm(`Health_Behavior`~.,data=st)
m1s <- parameters(m1,standardize="refit")
m1s
r2(m1)
model_performance(m1)

# symptom check
# 1. correlations 
  # Highest correlation is .55 followed by .52. Not quite .60, but close
# 2. is model sensitive to change? 
m2 <- lm(`Health_Behavior` ~ `Life Satisfaction` + `Resilience`, data=st)
compare_models(m1, m2, standardize = "refit", select = "se_p2")

# We will select Self Esteem

st <- read_csv("cstressuncg_clean.csv") %>%
  select(`Life Satisfaction`, `Self Esteem`, `Resilience`, 
         `Health_Behavior`) %>%
  datawizard::center(1,3)

# 1. VIF----
se_vif<-lm(`Self Esteem` ~ `Life Satisfaction` + `Resilience`, data=st)
r2(se_vif) # 41% of the variance in Self Esteem is explained by 
           # Life Satisfaction and Resilience 
# By hand
1 / (1-.412) # comes out to 1.70

# 2. tolerance----
# By hand
1-.412 # comes out to .59, meaning that 59% of the variance in Self Esteem
       # is available to explain health behaviors

# 3. inflated standard error---- 
m3 <- lm(`Health_Behavior` ~ `Self Esteem`, data=st)
compare_models(m1, m3, standardize = "refit", select = "se_p2")
# By hand
.10/.08 # comes out to 1.25, suggesting the se in m1 is 25% greater than 
        # se in m3

# diagnostics
multicollinearity(m1) # they match (within errors of rounding)