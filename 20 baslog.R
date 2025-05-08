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
ms <- read_csv("MDD_Small.csv") %>%
  select(2:3)

# frequency table
ms %>% freq_table(mdd, dsm_anhedonia)

# calculate probabilities
# MDD (1) and Anhedonia (1)
p1 <- 17/19
# No MDD (0) and Anhedonia (1)
p2 <- 5/59

# calculate odds
# MDD (1) and Anhedonia (1)
o1 <- p1/(1-p1)
# No MDD (0) and Anhedonia (1)
o2 <- p2/(1-p2)

# odds ratio
or <- o1/o2
or

# model 1 and 2 (same coefficient)
m1 <- glm(mdd ~ dsm_anhedonia, 
          family = binomial("logit"),
          data = ms)
m2 <- glm(dsm_anhedonia ~ mdd,
          family = binomial("logit"),
          data = ms)

# check odds ratio
model_parameters(m1, exponentiate = TRUE)
model_parameters(m2, exponentiate = TRUE)
model_parameters(m2)

# they match: The odds having experiencing MDD is 91.8 times greater
# among those with with anhedonia compared to those without anhedonia.
# Alternatively, the odds of presenting with anhedonia is 91.8 times greater
# among those diagnosed with MDD compared to those who are not diagnosed
# with MDD