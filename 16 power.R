# load packs
library(tidyverse)
library(easystats)
library(psych)

library(paletteer)
library(extrafont)
library(cowplot)
library(gridExtra)
library(ggforce)

library(pwr2ppl)
library(effectsize)

# read data 
cs <- read_csv("Conf_UNCG_TR_ex.csv") %>% 
  janitor::clean_names() %>%
  select(d_i, conf_mean, sth_mean, pol2_1)

cs_e <- read_csv("Conf_Mturk_TR_ex.csv") %>% 
  janitor::clean_names() %>%
  select(d_i, conf_mean, sth_mean, pol2_1)

# UNCG corr
lowerCor(cs)
correlation::cor_test(x="d_i", y="conf_mean", 
                      data=cs)
correlation::cor_test(x="d_i", y="sth_mean", 
                      data=cs)
correlation::cor_test(x="d_i", y="pol2_1", 
                      data=cs)
correlation::cor_test(x="pol2_1", y="conf_mean", 
                      data=cs)
correlation::cor_test(x="pol2_1", y="sth_mean", 
                      data=cs)
correlation::cor_test(x="sth_mean", y="conf_mean", 
                      data=cs)

# Mturk corr
correlation::cor_test(x="d_i", y="conf_mean", 
                      data=cs_e)

# focal predictor will be d-prime; outcome is conf_mean
# effect sizes should be -.15, -.25, -.35
# y ~ intercept + b1(poli) + b2(sth_mean) + b3(d-prime)

# effect size = -.15, n = 100
set.seed(4415) %>%                 
  MRC_all(ry1=.45, ry2=.21, ry3=-.15, 
          r12=.32,                   
          r13=-.20,                   
          r23=-.08,                   
          n=100,    
          alpha=.05,  
          rep=1000)

# effect size = -.15, n = 300
set.seed(4415) %>%                 
  MRC_all(ry1=.45, ry2=.21, ry3=-.15, 
          r12=.32,                   
          r13=-.20,                   
          r23=-.08,                   
          n=300,    
          alpha=.05,  
          rep=1000)

# effect size = -.15, n = 500
set.seed(4415) %>%                 
  MRC_all(ry1=.45, ry2=.21, ry3=-.15, 
          r12=.32,                   
          r13=-.20,                   
          r23=-.08,                   
          n=500,    
          alpha=.05,  
          rep=1000)

# effect size = -.25, n = 100
set.seed(4415) %>%                 
  MRC_all(ry1=.45, ry2=.21, ry3=-.25, 
          r12=.32,                   
          r13=-.20,                   
          r23=-.08,                   
          n=100,    
          alpha=.05,  
          rep=1000)

# effect size = -.25, n = 300
set.seed(4415) %>%                 
  MRC_all(ry1=.45, ry2=.21, ry3=-.25, 
          r12=.32,                   
          r13=-.20,                   
          r23=-.08,                   
          n=300,    
          alpha=.05,  
          rep=1000)

# effect size = -.25, n = 500
set.seed(4415) %>%                 
  MRC_all(ry1=.45, ry2=.21, ry3=-.25, 
          r12=.32,                   
          r13=-.20,                   
          r23=-.08,                   
          n=500,    
          alpha=.05,  
          rep=1000)

# effect size = -.35, n = 100
set.seed(4415) %>%                 
  MRC_all(ry1=.45, ry2=.21, ry3=-.35, 
          r12=.32,                   
          r13=-.20,                   
          r23=-.08,                   
          n=100,    
          alpha=.05,  
          rep=1000)

# effect size = -.35, n = 300
set.seed(4415) %>%                 
  MRC_all(ry1=.45, ry2=.21, ry3=-.35, 
          r12=.32,                   
          r13=-.20,                   
          r23=-.08,                   
          n=300,    
          alpha=.05,  
          rep=1000)

# effect size = -.35, n = 500
set.seed(4415) %>%                 
  MRC_all(ry1=.45, ry2=.21, ry3=-.35, 
          r12=.32,                   
          r13=-.20,                   
          r23=-.08,                   
          n=500,    
          alpha=.05,  
          rep=1000)


pf <- data.frame(n_s = c(100, 100, 100, 300, 300, 300, 500,
                        500, 500),
                 e_s = c(-.15, -.25, -.35, -.15, -.25, -.35,
                         -.15, -.25, -.35),
                 p_s = c(.101, .444, .841, .222, .882, .999,
                         .324, .981, 1)) 
pf$e_s <- factor(pf$e_s)
class(pf$e_s)

p1 <- ggplot(pf, aes(x = n_s, y = p_s, color = e_s, fill = e_s))+
  geom_point(shape = 15, size=2.5, alpha = .7)+
  geom_line(linewidth=1.5, alpha=2)+
  geom_hline(yintercept = .80, color = "black",
             linetype = "dotted", linewidth = 1.5)+
  geom_vline(xintercept = 300, color = "gray6", 
             linetype = "dotted", linewidth = 1.5)+
  labs(x = "Sample Size", y = "Power", fill = "Effect Size", 
       color = "Effect Size")+
  scale_x_continuous(limits = c(100, 500), n.breaks = 10)+
  scale_color_paletteer_d(guide = "legend", "wesanderson::AsteroidCity1")+
  theme_minimal_hgrid(font_family = "Lucida Sans")
p1

png("HW Plots/saycheese28.png",
    units="in", 
    width=6, height= 5, res = 800)
p1
dev.off()