# load packs
library(tidyverse)
library(easystats)

library(MASS)

library(mosaic)
library(psych)
library(DescTools)
library(car)
library(effects)

library(extrafont)
library(cowplot)

# load data
cs <- read.csv("Conf_UNCG_TR_ex.csv") %>%
  select(sth_mean, pol2_1, conf_mean) %>%
  datawizard::standardise(1:2)

m1 <- lm(conf_mean ~ pol2_1 + sth_mean, data = cs)
parameters(m1)
parameters(m1, standardize = "refit")

# check assumptions
predictorEffects(m1, partial.residuals = TRUE) %>% plot()

hist(m1$residuals) %>% plot()
check_normality(m1)
check_normality(m1) %>% plot()

check_heteroscedasticity(m1)
check_heteroscedasticity(m1) %>% plot()

# robust standard errors
m2 <- model_parameters(m1, vocv = "HC3")
m2

compare_parameters(m1, m2, select = "short")

# bootstrap
set.seed(0343)
m3 <- parameters(m1, vcov = "BS", vcov_args = c(R = 5000))
m3

compare_parameters(m1, m2, m3, select = "short")

# robust regression
m4 <- MASS::rlm(conf_mean ~ pol2_1 + sth_mean, psi = psi.bisquare,
                data = cs)
m4 %>% parameters()
parameters(m4, standardize = "refit")

compare_parameters(m1, m4, select = "short")