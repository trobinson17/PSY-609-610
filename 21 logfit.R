# load packs
library(tidyverse)
library(easystats)
library(psych)
library(sjmisc)
library(freqtables)

library(jtools)
library(effects)
library(car)

library(gridExtra)
library(paletteer)
library(extrafont)
library(cowplot)

# load data 
cs <- read_csv("cstressuncg_clean.csv") %>%
  janitor::clean_names() %>%
  select(life_satisfaction, self_esteem,
         resilience,general_health, direct_avg_uncg) %>%
  datawizard::standardise(1:4) %>%
  sjmisc::dicho(5)

# model 1
m1 <- glm(direct_avg_uncg_d ~ life_satisfaction + self_esteem,
          family = binomial("logit"),
          data = cs)
model_parameters(m1, exponentiate = FALSE)
model_parameters(m1, exponentiate = TRUE)

# model 2 
m2 <- glm(direct_avg_uncg_d ~ life_satisfaction + self_esteem +
            resilience + general_health,
          family = binomial("logit"),
          data = cs)
model_parameters(m2, exponentiate = FALSE)
model_parameters(m2, exponentiate = TRUE)

# model 3
m3 <- glm(direct_avg_uncg_d ~ 1,
          family = binomial("logit"),
          data = cs)
model_parameters(m3, exponentiate = FALSE)
model_parameters(m3, exponentiate = TRUE)

# model comparison 
compare_performance(m1,m2,m3,metrics = c("AIC", "BIC"))
deviance(m1)
deviance(m2)
deviance(m3)

# calculate R2 by hand
(deviance(m3) - deviance(m2))/deviance(m3) %>% round(3)
r2_mcfadden(m2)

# likelihood ratio test
test_likelihoodratio(m1,m2)