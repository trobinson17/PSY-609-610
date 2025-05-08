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

# read csv
cs <- read_csv("Conf_UNCG_TR_ex.csv") %>%
  select(d_i, pol2_1, sth_mean, conf_mean) %>%
  sjmisc::dicho(conf_mean) %>%
  datawizard::standardise(1:3)

# model 1
m1 <- glm(conf_mean_d ~ d_i + pol2_1 + sth_mean,
          family = binomial("logit"),
          data = cs)
model_parameters(m1, exponentiate = FALSE)
model_parameters(m1, exponentiate = TRUE)

# Cook's D
cooks.distance(m1) %>% hist()
influenceIndexPlot(m1,
                   vars=c("Cook", "Studentized"))

# calibaration plot
cp <- data.frame("Pred" = m1$fitted.values,
                 "Obs" = m1$y)
cc <- predtools::calibration_plot(cp,
                                  obs = "Obs",
                                  pred = "Pred",
                                  x_lim = c(-.05,1),
                                  y_lim = c(-.05,1),
                                  nTiles = 15)
cc

# Hosmer-Lemeshow
performance::performance_hosmer(m1, 
                                n_bins = 15)

density(rchisq(100000, 
               df=13)) %>% plot()
pchisq(8.626, df=13, lower.tail = F)

# AUC and discrimination 
cp %>% group_by(Obs) %>% slice_sample(n = 1)
roc <- performance_roc(m1)
roc %>% plot()

# freq table
cs %>% freq_table(conf_mean_d)
270/3