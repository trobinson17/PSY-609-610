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
  select(life_satisfaction, self_esteem) %>%
  sjmisc::dicho(self_esteem) %>%
  sjmisc::split_var(life_satisfaction, n = 3)

cs$ls_ord <- plyr::revalue(cs$life_satisfaction_g, 
                           c("1" = "0",
                             "2" = "1", 
                             "3" = "2"))

# Proportional odds assumption:
  # is the effect of self-esteem constant across life satisfaction
  # categories?

# model 1
m1 <- clm(ls_ord ~ self_esteem_d, 
          link = "logit",
          data = cs)
model_parameters(m1)

summary(m1)
model_parameters(m1)
exp(1.92)

# odds and probs
odds <- exp(m1$Theta) %>% round(2)
prob <- odds/(1+odds) %>% round(2)
prob

# baseline prob in low LS is 49%
# baseline prob in mid LS is 33%
# baseline prob in high LS is 18%

m1p <- estimate_means(m1,
                      by = "self_esteem_d=c(0,1)")

# plot
p1 <- ggplot(data = m1p,
             aes(x = Response,
                 y = Probability,
                 group = self_esteem_d,
                 color = self_esteem_d)) + 
  geom_line(linewidth = 1, alpha = 1) +
  geom_point(size = 2, alpha = 1) +
  geom_text(aes(label = round(Probability, 2)),
            nudge_y = .038,
            nudge_x = -.03,
            show.legend = FALSE,
            check_overlap = TRUE) + 
  scale_color_manual(values = c("#F2AD00","#B40f20"),
                     labels = c("Low", "High")) +
  ylim(c(0,.80)) +
  labs(x = "Life Satisfaction Category", 
       color = "Self Esteem", 
       title = "Model 1") +
  theme_minimal_hgrid(font_family = "Lucida Sans") +
  theme(legend.position = "inside",
        legend.position.inside = c(0.05,.85))
p1

# model 2
m2 <- clm(ls_ord ~ 1,
          nominal = ~ self_esteem_d,
          data = cs,
          control = list(sign.nominal = "negative"))
model_parameters(m2)

# exploring m1 and m2 estimates
exp(m1$beta) %>% round(2) # proportional 
exp(m2$coefficients[3:4]) %>% mean() %>% round(2) # non-proportional
# cool, they are the same; 6.8

nom_prob <- estimate_means(m2,
                           by = "self_esteem_d=c(0,1)")
nom_prob

# plot
p2 <- ggplot(data = nom_prob,
             aes(x = Response,
                 y = Probability,
                 group = self_esteem_d,
                 color = self_esteem_d)) + 
  geom_line(linewidth = 1, alpha = 1) +
  geom_point(size = 2, alpha = 1) +
  geom_text(aes(label = round(Probability, 2)),
            nudge_y = .03,
            nudge_x = -.05,
            show.legend = FALSE,
            check_overlap = TRUE) + 
  scale_color_manual(values = c("#F2AD00","#B40f20"),
                     labels = c("Low", "High")) +
  ylim(c(0,.80)) +
  labs(x = "Life Satisfaction Category", 
       color = "Self Esteem", 
       title = "Model 2") +
  theme_minimal_hgrid(font_family = "Lucida Sans") +
  theme(legend.position = "inside",
        legend.position.inside = c(0.05,.85))
p2

grid.arrange(p1, p2, ncol=2)

png("HW Plots/saycheese41.png",
    units="in", 
    width=12, height= 4, res = 300)
grid.arrange(p1, p2,nrow=1, ncol=2)  
dev.off()

# comparing performance
compare_performance(m1, m2,
                    metrics = c("AIC","BIC"))
nominal_test(m1)
