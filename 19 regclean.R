# load packs
library(tidyverse)
library(easystats)
library(psych)

library(car)
library(effects)

library(bestNormalize)

library(gridExtra)
library(paletteer)
library(extrafont)
library(cowplot)
library(ggridges)

# load data 
cs <- read_csv("Conf_UNCG_TR_ex.csv") %>%
  janitor::clean_names() %>%
  select(pol2_1, conf_mean) %>% 
  mutate(pol2_1_c = datawizard::center(pol2_1))

# visualize confederate mean
density(cs$conf_mean) %>% plot()

# ordered quantile
cs$conf_norm <- orderNorm(cs$conf_mean)$x.t
density(cs$conf_norm) %>% plot()

# winz
cs$conf_w <- datawizard::winsorize(cs$conf_mean, 
                                   robust = TRUE,
                                   method = "zscore",
                                   threshold = 2) %>% round(2)
density(cs$conf_w) %>% plot()
view(cs)

# density raw
c1 <- ggplot(cs, aes(x=conf_mean,
                          y=after_stat(count)))+
  geom_density(fill ="#F2AD00",
               color="white",
               alpha=.60,
               linewidth=1)+
  scale_x_continuous(limits = c(1,7), n.breaks = 7)+
  scale_y_continuous(limits = c(0, 275),
                     expand = c(0,0),
                     n.breaks = 6)+
  labs(x="Support for Confederate Symbols (Raw)", y="Density (Count)")+
  theme_cowplot(font_family="Lucida Sans")+
  theme(axis.ticks=element_blank())
c1

png("HW Plots/saycheese33.png",
    units="in", 
    width=5, height= 5, res = 800)
c1
dev.off()

# density o_q
c2 <- ggplot(cs, aes(x=conf_norm,
                     y=after_stat(count)))+
  geom_density(fill ="#F2AD00",
               color="white",
               alpha=.60,
               linewidth=1)+
  scale_x_continuous(limits = c(-1, 3), n.breaks = 7)+
  scale_y_continuous(limits = c(0,300), expand = c(0,0), n.breaks = 6)+
  labs(x="Support for Confederate Symbols (Quantiles)", y="Density (Count)")+
  theme_cowplot(font_family="Lucida Sans")+
  theme(axis.ticks=element_blank())
c2

png("HW Plots/saycheese34.png",
    units="in", 
    width=5, height= 5, res = 800)
c2
dev.off()

# density winz
c3 <- ggplot(cs, aes(x=conf_w,
                     y=after_stat(count)))+
  geom_density(fill ="#F2AD00",
               color="white",
               alpha=.60,
               linewidth=1)+
  scale_x_continuous(limits = c(1, 7), n.breaks = 7)+
  scale_y_continuous(expand = c(0,0), n.breaks = 6)+
  labs(x="Support for Confederate Symbols (Winsorized)", y="Density (Count)")+
  theme_cowplot(font_family="Lucida Sans")+
  theme(axis.ticks=element_blank())
c3

png("HW Plots/saycheese35.png",
    units="in", 
    width=5, height= 5, res = 800)
c3
dev.off()

# model 1 raw
m1 <- lm(conf_mean ~ pol2_1_c, data = cs)
model_parameters(m1)
model_parameters(m1, standardize = "refit")
describe(cs)

# m1 LINE
effects::predictorEffects(m1,
                          partial.residuals=T) %>% plot() #L
hist(m1$residuals) #N
check_normality(m1)
car::residualPlots(m1) #E
check_heteroscedasticity(m1)

# model 2 o_q
m2 <- lm(conf_norm ~ pol2_1_c, data = cs)
model_parameters(m2)
model_parameters(m2, standardize = "refit")

# m2 LINE
effects::predictorEffects(m2,
                          partial.residuals=T) %>% plot() #L
hist(m2$residuals) #N
check_normality(m2)
car::residualPlot(m2) #E
check_heteroscedasticity(m2) %>% plot()


# model 2 wins
m3 <- lm(conf_w ~ pol2_1_c, data = cs)
model_parameters(m3)
model_parameters(m3, standardize = "refit")

# m3 LINE
effects::predictorEffects(m3,
                          partial.residuals=T) %>% plot() #L
hist(m3$residuals) #N
check_normality(m3)
car::residualPlot(m3) #E
check_heteroscedasticity(m3) %>% plot()

# plot of residuals raw model
c4 <- ggplot(m1,
             aes(x=m1$residuals,
                 y=after_stat(count)))+
  geom_density(fill="#F2AD00",
               color="white",
               alpha=.5)+
  scale_x_continuous(limit=c(-4, 6))+
  scale_y_continuous(expand=c(0,0), n.breaks = 5)+
  labs(x="Residuals",
       y="Density (Count)")+
  theme_cowplot(font_family = "Lucida Sans")+
  theme(axis.ticks=element_blank())
c4

png("HW Plots/saycheese36.png",
    units="in", 
    width=5, height= 5, res = 800)
c4
dev.off()

# plot of residuals o_q model
c5 <- ggplot(m2,
             aes(x=m2$residuals,
                 y=after_stat(count)))+
  geom_density(fill="#F98400",
               color="white",
               alpha=.5)+
  scale_x_continuous(limit=c(-3, 3))+
  scale_y_continuous(limit = c(0,300),
                     expand=c(0,0), n.breaks = 6)+
  labs(x="Residuals",
       y="Density (Count)")+
  theme_cowplot(font_family = "Lucida Sans")+
  theme(axis.ticks=element_blank())
c5

png("HW Plots/saycheese37.png",
    units="in", 
    width=5, height= 5, res = 800)
c5
dev.off()

# Cooks D for models 
CookDm1 <- cooks.distance(m1) %>% round(3)
CookDm2 <- cooks.distance(m2) %>% round(3)
CookDm3 <- cooks.distance(m3) %>% round(3)

Cooks <- data.frame("Cook1" = CookDm1,
                    "Cook2" = CookDm2,
                    "Cook3" = CookDm3)
view(Cooks)