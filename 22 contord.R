# load packs
library(tidyverse)
library(easystats)
library(psych)
library(freqtables)

library(ordinal)

library(effects)
library(car)

library(gridExtra)
library(paletteer)
library(extrafont)
library(cowplot)

# load data
cs <- read_csv("cstressuncg_clean.csv") %>%
  janitor::clean_names() %>%
  select(direct_avg_uncg, pss_self, pss_avg_uncg, 
         life_satisfaction) %>%
  datawizard::standardise(1:3) %>%
  sjmisc::split_var(life_satisfaction, n = 3)

cs$ls_ord <- plyr::revalue(cs$life_satisfaction_g, 
                           c("1" = "0",
                             "2" = "1", 
                             "3" = "2"))

m1 <- clm(ls_ord ~ direct_avg_uncg + pss_self + pss_avg_uncg,
              link = "logit",
              data = cs)
model_parameters(m1)

s_prob <- estimate_means(m1, 
                         by = "pss_self = seq(-2.5, 2.5, .1)")

# plot of all curves
three_c <- paletteer_d("RColorBrewer::OrRd") [c(8, 6, 4)]

p1 <- ggplot(s_prob, aes(x = pss_self, y = Probability, 
                          color = Response)) + 
  geom_line(alpha = .6, linewidth = 1) +
  scale_color_manual(values = three_c,
                     labels = c("Low", "Medium", "High")) +
  scale_x_continuous(n.breaks = 7) +
  labs(x = "Perceived Stress (z)",
       color = "Life Satisfaction") +
  theme_minimal_hgrid(font_family = "Lucida Sans")
p1

png("HW Plots/saycheese43.png",
    units="in", 
    width=5, height= 5, res = 800)
p1
dev.off()

# prop odds
ordinal::nominal_test(m1)
