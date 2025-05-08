# load packs
library(tidyverse)
library(easystats)
library(psych)
library(freqtables)

library(car)
library(moderndive)
library(effects)

library(interactions)

library(gridExtra)
library(paletteer)
library(extrafont)
library(cowplot)
library(janitor)

# load data
st <- read_csv("cstressuncg_clean.csv") %>%
  clean_names() %>%
  select(direct_avg_uncg, direct_acq, life_satisfaction) %>%
  datawizard::standardize(1:2)

# interaction 
m1 <- lm(life_satisfaction ~ direct_avg_uncg*direct_acq, data = st)
mp <- model_parameters(m1)
r2(m1)


# plot of interaction 
i1 <- interact_plot(m1, 
       pred = direct_avg_uncg,
       modx= direct_acq,
       colors = c("#F2AD00","#B40f20"),
       legend.main = "Acq. Stress",
       modx.labels = c("Low", "Medium", "High"),
       data=st,
       plot.points = F)+
  labs(y = "Life Satisfaction", x = "Peer Stress") + 
  ylim(1,5) +
  theme_minimal_hgrid(font_family = "Lucida Sans")
i1

# export
png("HW Plots/saycheese23.png",
    units="in", 
    width=5, height= 5, res = 300)
i1
dev.off()

# simple slopes
ss1 <- sim_slopes(m1,
                  pred = direct_avg_uncg,
                  modx = direct_acq,
                  data = st)
ss1
ss1$slopes %>% round(3)

# J-N Analysis
jn1 <- johnson_neyman(m1,
                      pred = direct_avg_uncg,
                      modx = direct_acq,
                      sig.color = "#B40f20",
                      insig.color = "grey2",
                      line.thickness = 2)

jn1$plot

jn <- jn1$plot + labs(x = "Acq. Stress (Standardized)",
       y = "Slope of Avg. Stress and Life Satisfaction",
       title = "J-N Regions of Significance") +
  scale_x_continuous(n.breaks = 6) +
  theme_minimal_hgrid(font_family = "Lucida Sans")+
  theme(legend.position = "none")
jn

# export
png("HW Plots/saycheese24.png",
    units="in", 
    width=5, height= 5, res = 300)
jn
dev.off()