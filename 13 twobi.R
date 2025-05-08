# load packs
library(tidyverse)
library(easystats)
library(psych)
library(freqtables)
library(janitor)

library(car)
library(effects)
library(moderndive)
library(reghelper) 
library(interactions)

library(gridExtra)
library(paletteer)
library(extrafont)
library(cowplot)

# load data 
st <- read_csv("cstressuncg_clean.csv") %>%
  clean_names() %>% select(direct_avg_uncg, direct_acq, life_satisfaction)

# split two direct measures into binary categories
st$direct_avg_uncg_cat <- datawizard::categorize(st$direct_avg_uncg, 
                                                 split="quantile", n_groups=2,
                                                 lowest = 0)

st$direct_acq_cat <- datawizard::categorize(st$direct_acq, 
                                                 split="quantile", n_groups=2,
                                                 lowest = 0)

# regression raw weights
m1 <- lm(life_satisfaction ~ direct_avg_uncg_cat + direct_acq_cat 
         + direct_avg_uncg_cat*direct_acq_cat, data = st)
model_parameters(m1)
estimate_means(m1, 
               by = c("direct_avg_uncg_cat",
                      "direct_acq_cat"))

# cat_plot
st$direct_avg_uncg_cat <- factor(st$direct_avg_uncg_cat)
st$direct_acq_cat <- factor(st$direct_acq_cat)

factor_m1 <- lm(life_satisfaction ~ direct_avg_uncg_cat*direct_acq_cat,
                data = st)

c1 <- cat_plot(factor_m1, pred = direct_avg_uncg_cat, 
         modx = direct_acq_cat, geom = "line", 
         colors = c("#F2AD00","#B40f20"),
         interval = F, point.shape = T,
         pred.labels = c("Low", "High"),
         modx.labels = c("Low", "High"),
         legend.main = "Acq. Stress") +
  labs(y = "Life Satisfaction", x = "Peer Stress") + 
  ylim(1,5) + 
  theme_minimal_hgrid(font_family = "Lucida Sans")

# export 
png("HW Plots/saycheese21.png",
    units="in", 
    width=5, height= 5, res = 300)
c1
dev.off()