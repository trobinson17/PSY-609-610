# load packages
library(tidyverse)
library(easystats)
library(psych)
library(jtools)
library(interactions)
library(freqtables)

library(fastDummies) 

library(car)
library(effects)
library(parameters)
library(reghelper)
library(emmeans)

library(gridExtra)
library(paletteer)
library(extrafont)
library(cowplot)

# load data 
cs_e <- read_csv("Conf_UNCG_TR_ex.csv") %>% 
  select(race_f, conf_mean)

# convert to factor and reorder levels
cs_e$race_f1 <- fct_relevel(cs_e$race_f, "Black")
class(cs_e$race_f1)
levels(cs_e$race_f1)

# describe
psych::describeBy(cs_e$conf_mean, 
                  group=cs_e$race_f1)

# regression 
m1 <- lm(conf_mean ~ race_f1, data = cs_e)
model_parameters(m1)
estimate_means(m1)
e1 <- effect_plot(m1,
                  pred = race_f1,
                  data = cs_e,
                  x.label = "Race-Ethnicity",
                  y.label = "Support for Confederate Symbols")+
  scale_y_continuous(limits = c(1,7),n.breaks = 7)+
  theme_minimal_hgrid(font_family = "Lucida Sans")
e1

png("HW Plots/saycheese27.png",
    units="in", 
    width=4, height= 4, res = 200)
e1
dev.off()

# post hoc tests
# raw 
raw <- estimate_contrasts(m1, 
                   contrast="race_f1",
                   p_adjust = "none")
raw

# Holm correction
adj <- estimate_contrasts(m1, 
                   contrast="race_f1",
                   p_adjust = "holm")
adj