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
  clean_names() %>% select(direct_avg_uncg, direct_acq, life_satisfaction) %>%
  datawizard::standardize(direct_avg_uncg)

st$direct_acq_cat <- datawizard::categorize(st$direct_acq, 
                                            split="quantile", n_groups=2,
                                            lowest = 0)

st$direct_acq_cat <- factor(st$direct_acq_cat)

# regression 
m1 <- lm(life_satisfaction ~ direct_avg_uncg + direct_acq_cat 
         + direct_avg_uncg*direct_acq_cat, data = st)
mp <- model_parameters(m1)

# estimated marginal means
em <- estimate_means(m1, 
               by = c("direct_avg_uncg = c(-1,1)",
                      "direct_acq_cat"))

# interaction plot
i1 <- interact_plot(m1, pred = direct_avg_uncg,
              modx = direct_acq_cat,data = st,
              colors = c("#F2AD00","#B40f20"),interval = T,
              modx.labels = c("Low", "High"),
              legend.main = "Acq. Stress") +
  labs(y = "Life Satisfaction", x = "Peer Stress") + 
  ylim(1,5) + 
  theme_minimal_hgrid(font_family = "Lucida Sans")
i1

# export 
png("HW Plots/saycheese22.png",
    units="in", 
    width=5, height= 5, res = 300)
i1
dev.off()