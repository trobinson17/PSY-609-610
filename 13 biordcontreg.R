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
  select(`inc`, `Direct Avg. UNCG`, `Life Satisfaction`, `Self Esteem`) %>%
  datawizard::center(`Direct Avg. UNCG`) %>%
  clean_names()

# categorize inc_ord
st <- categorize(st,
           split="quantile",
           select = inc,
           n_groups = 3,
           lowest = 0,
           append = T)

# Model 1
m1 <- lm(life_satisfaction ~ inc + direct_avg_uncg, data = st)
m1p <- model_parameters(m1)
r2(m1)

# Standardized Model `
m1ps <- reghelper::beta(m1, x=T,y=T,skip="inc") %>% parameters()

# Model 2
stc <- categorize(st,
                  select = self_esteem,
                  n_groups = 2,
                  lowest = 0,
                  append = T)

# Model 2
m2 <- lm(life_satisfaction ~ self_esteem_r + direct_avg_uncg, data=stc)
m2p <- model_parameters(m2)
r2(m2)

# Plot
i1 <- interact_plot(m2,        
              pred = direct_avg_uncg,  
              modx = self_esteem_r,
              colors = c("red","black"),
              modx.labels = c("Low Self-Esteem", "High Self-Esteem"))+
  labs(x = "Comparative Stress", y = "Life Satisfaction",
       color = "Self Esteem")+
  ylim(1,5)+
  theme_minimal_grid(font_family = "Lucida Sans")+
  theme(legend.position = "top", legend.title = element_blank())
i1

# export
png("HW Plots/saycheese20.png",
    units="in", 
    width=4, height= 4, res = 200)
i1
dev.off()