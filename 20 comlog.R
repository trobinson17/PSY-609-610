# load packages
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
test <- read_csv("Conf_UNCG_TR_ex.csv") %>%
  select(pol2_1)
describe(test$pol2_1)

cs <- read_csv("Conf_UNCG_TR_ex.csv") %>%
  select(sth_mean, pol2_1, conf_mean) %>%
  datawizard::standardise(1:2) %>%
  dicho(conf_mean)
describe(cs)

# model 1
m1 <- glm(conf_mean_d ~ pol2_1 + sth_mean,
          family = binomial("logit"),
          data=cs)
model_parameters(m1, exponentiate = FALSE)
model_parameters(m1, exponentiate = TRUE)

# effect plot
p1 <- effect_plot(m1,
            pred = pol2_1, 
            interval = TRUE,
            line.colors = "#B40f20")+
  ylim(c(0,1))+
  labs(x = "Political Ideology (Standardized)", 
       y = "Probability of Supporting Confederate Symbols")+
  theme_minimal_hgrid(font_family = "Lucida Sans")
p1

png("HW Plots/saycheese38.png",
    units="in", 
    width=5, height= 5, res = 800)
p1
dev.off()

# estimate means
estimate_means(m1, 
               by="pol2_1=c(-1, 0, 1, 2)")
