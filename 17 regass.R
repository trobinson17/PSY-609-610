# load packs
library(tidyverse)
library(easystats)  
library(psych)
library(qqplotr)
library(car)

library(moderndive)
library(effects)

library(ggtext)
library(gridExtra)
library(paletteer)
library(extrafont)
library(cowplot)

# read data
cs <- read_csv("Conf_UNCG_TR_ex.csv") %>% 
  janitor::clean_names() %>%
  select(pol2_1, conf_mean) %>% 
  mutate(pol2_1_c = datawizard::center(pol2_1))


# model 1
m1 <- lm(conf_mean ~ pol2_1_c, data = cs)
model_parameters(m1)
model_parameters(m1, standardize = "refit")
describe(cs)


# Linearity
predictorEffects(m1, partial.residuals=TRUE) %>% plot()

pc <- ggplot(cs, aes(x=pol2_1, y=conf_mean))+
  geom_smooth(method="lm", color="#B40f20",
              fill = "grey",se=TRUE, linetype="dashed", lwd=1.25)+
  geom_jitter(color = "#F98400", alpha = .5)+
  scale_x_continuous(limits = c(1,7), n.breaks = 7)+
  scale_y_continuous(limits = c(1, 7), n.breaks = 7)+
  labs(x = "Political Ideology",
       y = "Support for Confederate Symbols")+
  theme_minimal_grid(font_family = "Lucida Sans")
pc

pc2 <- pc + geom_smooth(method = "loess", color = "red", se = FALSE)         
pc2

png("HW Plots/saycheese29.png",
    units="in", 
    width=5, height= 5, res = 800)
pc2
dev.off()

# Normality 
in_points <- get_regression_points(m1)
density(m1$residuals) %>% plot()
hist(m1$residuals) %>% plot()
check_normality(m1)


# density
c2 <- ggplot(m1,
             aes(x=m1$residuals,
                 y=after_stat(count)))+
  geom_density(fill="#F2AD00",
               color="white",
               alpha=.5)+
  scale_x_continuous(limit=c(-4, 6))+
  scale_y_continuous(expand=c(0,0))+
  labs(x="Residuals",
       y="Density (Count)")+
  theme_cowplot(font_family = "Lucida Sans")+
  theme(axis.ticks=element_blank())
c2

png("HW Plots/saycheese32.png",
    units="in", 
    width=5, height= 5, res = 800)
c2
dev.off()


# histogram
pc3 <- ggplot(m1, 
             aes(x=m1$residuals))+
  geom_histogram(fill= "#F2AD00",
                 color="white",
                 alpha = .45,
                 bins=12)+
  scale_x_continuous(limits = c(-4, 6))+
  scale_y_continuous(expand=c(0,0),
                     limits = c(0,200))+
  labs(x="Residuals",
       y="Frequency")+
  theme_cowplot(font_family="Lucida Sans")+
  theme(axis.ticks=element_blank())
pc3

png("HW Plots/saycheese30.png",
    units="in", 
    width=5, height= 5, res = 800)
pc3
dev.off()

# Equality 
car::residualPlots(m1)
check_heteroscedasticity(m1)
plot(predictorEffects(m1, partial.residuals=TRUE))
check_heteroscedasticity(m1) %>% plot()

pc4 <- ggplot(m1, aes(x=pol2_1_c, 
                    y=abs(m1$residuals)))+
  geom_jitter(color = "#F98400", alpha = .5)+
  geom_smooth(method="lm", color="#B40f20",
              fill = "gray",se=TRUE, linetype="dashed", lwd=1.25)+
  geom_smooth(method = "loess", color = "red", se = FALSE)+
  scale_x_continuous(limits = c(-3, 5))+
  scale_y_continuous(limits = c(0, 6))+
  labs(x = "Political Ideology (Centered)", 
       y = "Absolute Residuals")+
  theme_minimal_hgrid(font_family = "Lucida Sans")
pc4

png("HW Plots/saycheese31.png",
    units="in", 
    width=5, height= 5, res = 800)
pc4
dev.off()