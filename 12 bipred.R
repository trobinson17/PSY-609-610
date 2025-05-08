# load packs
library(tidyverse)
library(easystats)
library(psych)
library(freqtables)
library(janitor)

library(car)
library(effects)
library(moderndive)
library(reghelper) # helps standardization
library(interactions)

library(gridExtra)
library(paletteer)
library(extrafont)
library(cowplot)

# load data
st <- read_csv("cstressuncg_clean.csv") %>% 
  select(`PSS Self`, `PSS Avg. UNCG`, `Direct Avg. UNCG`, 
         `Life Satisfaction`:`Health_Behavior`) %>%
  clean_names()

# split into two categories
stc <- categorize(st,
                  select = c("direct_avg_uncg",
                             "self_esteem"),
                  n_groups = 2,
                  lowest = 0,
                  append = T)

ltd <- describe(stc$life_satisfaction)

# model 1: one bindary predictor 
lm1 <- lm(life_satisfaction ~ self_esteem_r, data = stc)
para1 <- model_parameters(lm1)

lm1_z <- beta(lm1, x = FALSE, y = TRUE) %>% parameters()

ctd <- describeBy(stc$life_satisfaction, group = stc$self_esteem_r)

# model 2: two binary predictors
lm2 <- lm(life_satisfaction ~ self_esteem_r + direct_avg_uncg_r, data=stc)
para2 <- model_parameters(lm2)

lm2_z <- beta(lm2, x = FALSE, y = TRUE) %>% parameters()


# model 2 R2
r2(lm2)

# extra: visualize lm1
s1 <-ggplot(stc, aes(x=self_esteem_r, y=life_satisfaction))+
  geom_jitter(height=0, width = .3, color="red")+  # note H and W options
  labs(x = "Self-Esteem",
       y = "Life Satisfaction")+
  ylim(c(1,5))+
  scale_x_discrete(labels = c("0"="Low SE", "1"="High SE"))+
  theme_minimal_grid(font_family = "Lucida Sans")
s1