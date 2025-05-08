# load packs
library(tidyverse)
library(easystats)
library(psych)
library(freqtables)
library(janitor)

library(ordinal)

library(effects)
library(car)

library(gridExtra)
library(paletteer)
library(extrafont)
library(cowplot)


# load data 
cs <- read_csv("cstressuncg_clean.csv") %>% 
  clean_names() %>%
  select(life_satisfaction, direct_avg_uncg) %>%
  sjmisc::dicho(direct_avg_uncg) %>%
  sjmisc::split_var(life_satisfaction, n = 3)

cs$ls_ord <- plyr::revalue(cs$life_satisfaction_g, 
                           c("1" = "0",
                             "2" = "1", 
                             "3" = "2"))

# model
m1 <- clm(ls_ord ~ direct_avg_uncg_d, 
          link = "logit",
          data = cs)

summary(m1)
model_parameters(m1)
exp(-0.86)

# baseline odds and probs
odds0 <- exp(m1$Theta) %>% round(2)
prob0 <- odds0/(1+odds0) %>% round(2)
prob0

# probability of being in lowest group is 23%
# probability in being in middle group is 30%
# probability of being in highest group is 47%
# these are the baseline probabilities (comparative stress = 0) of 
# being in each life satisfaction category
# BUT... as comparative stress goes from 0 to 1, the log-odds of being in 
# higher LS categories decrease by 0.86. 
# Another way of saying this is that as comparative stress goes up by 1, 
# the latent model shifts by 0.86

# log-odds of being above 2, high CS 1|2 boundary
-0.11 + (-.86*1)

# odds of being above 2, high CS 1|2 boundary
exp(-0.97)

# probability of being above 2, high CS 1|2 boundary
0.38/(1+0.38)

# 28% probability of being above 2

# log-odds of being above 1, high CS 0|1 boundary
1.21 + (-.86*1)

# log odds
exp(0.35)

# probability of being above 1, high CS 0|1 boundary
1.42/(1 + 1.42)

# 59% probability of being above 1

# 59% - 28% = 31% probability of being in 1 category

# 100 - 59% = 41% probability of being in 0 category

# check and save
estimate_means(m1,
               by = "direct_avg_uncg_d=c(0,1)")
m1p <- estimate_means(m1,
                      by = "direct_avg_uncg_d=c(0,1)")

# plot 
p1 <- ggplot(data = m1p,
             aes(x = Response,
                 y = Probability,
                 group = direct_avg_uncg_d,
                 color = direct_avg_uncg_d)) + 
  geom_line(linewidth = 1, alpha = 1) +
  geom_point(size = 2, alpha = 1) +
  geom_text(aes(label = round(Probability, 2)),
            nudge_y = .0375,
            show.legend = FALSE) + 
  scale_color_manual(values = c("#F2AD00","#B40f20"),
                     labels = c("Low", "High")) +
  ylim(c(0.15,.6)) +
  labs(x = "Life Satisfaction Category", 
       color = "Comparative Stress") +
  theme_minimal_hgrid(font_family = "Lucida Sans")
p1

png("HW Plots/saycheese42.png",
    units="in", 
    width= 6, height= 4, res = 300)
p1
dev.off()