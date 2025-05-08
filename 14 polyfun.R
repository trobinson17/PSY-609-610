# load packages
library(tidyverse)
library(easystats)
library(psych)

library(car)
library(effects)
library(jtools)
library(sjmisc)
library(freqtables)

library(gridExtra)
library(paletteer)
library(extrafont)
library(cowplot)

# load data
st <- read_csv("cstressuncg_clean.csv") %>%
  janitor::clean_names() %>%
  select(direct_avg_uncg, life_satisfaction, resilience) %>%
  datawizard::center(1) %>%
  datawizard::center(3)

# poke around; scatterplot
s1 <- ggplot(st, aes(x = direct_avg_uncg, y = life_satisfaction))+
  geom_point(color="red", size=2.5, alpha=.35) +
  geom_smooth(method="lm", se=F, 
              color="grey70", linewidth = 1.5) +
  labs(x = "Peer Stress",
       y = "Life Satisfaction")+
  theme_minimal_grid(font_family = "Lucida Sans")
s1

# scatterplot + loess
s1 + geom_smooth(method="loess", se=F, color="red", linewidth = 1.5)


# scatterplot + curve
s2 <- s1  +
  geom_smooth(method="lm", 
              formula = y~poly(x,2),  
              se=F, 
              color="grey30", linetype="dashed", linewidth=1.5) +
  xlim(-3,3)
s2

# export s2
png("HW Plots/saycheese26.png",
    units="in", 
    width=5, height= 5, res = 300)
s2
dev.off()


# quadratic line fits the data

# regression
m1 <- lm(life_satisfaction ~ resilience + poly(direct_avg_uncg,
                                               degree = 2,
                                               raw = TRUE), data = st)
mp <- model_parameters(m1)
predictorEffects(m1) %>% plot 
multicollinearity(m1)
r2(m1)
estimate_means(m1, 
               by = "direct_avg_uncg=c(-1,0,1)")


# effect plot
e1 <-effect_plot(m1, pred = direct_avg_uncg, 
                    interval = TRUE,
                    centered = "all", 
                    data=st,
                    line.thickness=1.25,
                    colors="#B40f20",
                 plot.points = TRUE) +
  labs(y="Life Satisfaction", x="Peer Stress")+
  theme_minimal_grid(font_family = "Lucida Sans")+
  xlim(-3,3) +
  ylim(1,5) 
e1 

# export 
png("HW Plots/saycheese25.png",
    units="in", 
    width=5, height= 5, res = 300)
e1
dev.off()