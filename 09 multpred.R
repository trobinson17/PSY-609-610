# load packs 
library(tidyverse)
library(easystats)  # contains parameters, performance, datawizard packages
library(psych)

library(moderndive)
library(jtools)
library(effects)
library(car)
library(rockchalk)

library(gridExtra)
library(paletteer)
library(extrafont)
library(cowplot)
library(janitor)

# grab data 
st <- read_csv("cstressuncg_clean.csv") %>%
  select(`PSS Self`, `PSS Avg. UNCG`,`Direct Avg. UNCG`)
std<-describe(st)
std

# explore 
multcor <- psych::lowerCor(st)

# centered model
m1_c<-lm(`Direct Avg. UNCG` ~ scale(`PSS Self`, scale=FALSE) +
           scale(`PSS Avg. UNCG`, scale=FALSE), data=st)
model_parameters(m1_c)

# standardize m1_c
m1s <- parameters(m1_c,standardize="refit")
m1s

# biggest cor-B difference is PSS Avg. UNCG
  # PSS Avg. UNCG: cor - B = .06 (.02 versus -.04)
  # PSS Self UNCG: cor - B = -.01 (.29 versus .30)

# simple scatterplot PSS Avg. UNCG and Direct Avg. UNCG
s1 <- ggplot(st, aes(x=`PSS Avg. UNCG`,        
                            y=`Direct Avg. UNCG`))+
  geom_point(color="#F98400",alpha=.7,size=4)+
  geom_smooth(method="lm",color="#B40f20",fill="grey50",se=F)+
  labs(x="Peer Stress", y = "Comparative Stress")+
  theme_minimal_grid(font_family = "Lucida Sans")+
  xlim(1,5)+
  ylim(1,5)+
  geom_text(label="r = .02", x=1.5,y=1.5,family="Lucida Sans")
s1

# export
png("HW Plots/saycheese18.png",
    units="in", 
    width=2, height= 2, res = 300)
s1
dev.off()

# partial scatterplot PSS Avg. UNCG and Direct Avg. UNCG
s2 <- effect_plot(m1_c, 
            pred=`PSS Avg. UNCG`,
            interval = F,
            data=st)+
  labs(x = "Peer Stress",
       y = "Comparative Stress")+
  ylim(c(1,5))+
  theme_minimal_grid(font_family = "Lucida Sans")+
  geom_text(label="r = - .04",x=2.25,y=1.5,family="Lucida Sans")
s2

# export
png("HW Plots/saycheese19.png",
    units="in", 
    width=2, height=2, res = 300)
s2
dev.off()