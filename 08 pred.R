# READING: Keith, Chapter 1: "Simple Bivariate Regression"

# load packages----
# basics
library(tidyverse)
library(psych)
library(easystats)

# easystats gives us these:
library(parameters)
library(performance)
library(modelbased)

# regression analysis
library(car)
library(moderndive)
library(reghelper)
library(parameters)

# plotting
library(paletteer)
library(gridExtra)
library(extrafont)
library(cowplot)
library(packcircles)
library(ggtext)
library(plotly)

# load data 
st <- read_csv("cstressuncg_clean.csv") %>% 
  select(`PSS Self`, `Direct Avg. UNCG`)

# describe
std <- describe(st)
std

# scatterplot
s1 <- ggplot(st, aes(x=`PSS Self`,        
                        y=`Direct Avg. UNCG`))+
  geom_jitter(color="#F98400",
              width=1,
              height=1,
              alpha=.7,
              size=4)+
  geom_smooth(method="lm",
              color="#B40f20",
              fill="grey50",
              se=F)+
  labs(x="Self Stress", y = "Comparative Stress")+
  theme_minimal_grid(font_family = "Lucida Sans")+
  xlim(1,5)+
  ylim(1,5)+
  geom_text(label="b=.52", x=1.5,y=1.5,family="Lucida Sans")
s1

# export scatterplot
png("HW Plots/saycheese16.png",
    units="in", 
    width=4, height= 4, res = 200)
s1
dev.off()

# Not centered model
lm1 <- lm(`Direct Avg. UNCG`~`PSS Self`, data=st)
lm1m <- model_parameters(lm1)
lm1m

# y=1.67+.52(x)
# 1 SD above Self Stress is 3.67
# 1 SD below Self Stress is 2.65
# predicted value for 1 SD above the mean 
1.67+(.52*3.67) # y-hat = 3.58
# predicted value for 1 SD below the mean
1.67+(.52*2.65) # y-hat = 3.05

# Centered model
lm2 <- lm(`Direct Avg. UNCG`~scale(`PSS Self`,scale=F), data=st)
lm2m <- model_parameters(lm2)
lm2m

# compare the models
cm <- compare_models(lm1,lm2)
cm

# y=3.31+.52(x)
# 1 SD above Self Stress is .51
# 1 SD below Self Stress is -.51
# predicted value for 1 SD above the mean 
3.31+(.52*.51) # y-hat = 3.58
# predicted value for 1 SD below the mean
3.31+(.52*-.51) # y-hat = 3.04

# standardized lm1
lm3<-parameters(lm1, standardize = "refit")
cm2<-compare_models(lm2,lm3)

# observed scores (x-axis) regressed onto predicted scores (y-axis)
st$`PSS Self_cent`<-scale(st$`PSS Self`,center=TRUE,scale=FALSE)
lm4<-lm(`Direct Avg. UNCG`~`PSS Self_cent`,data=st)
in_points<-get_regression_points(lm4)
View(in_points)

# scatterplot of residuals 
s2 <- ggplot(in_points, aes(x=`Direct Avg. UNCG`,        
                     y=`Direct Avg. UNCG_hat`))+
  geom_point(color="#F98400",alpha=.7,size=4)+
  geom_smooth(method="lm",color="#B40f20",fill="grey50",se=F)+
  labs(x="Comparative Stress (Observed)", y = "Comparative Stress (Predicted)")+
  theme_minimal_grid(font_family = "Lucida Sans")+
  xlim(1,5)+
  ylim(1,5)+
  geom_text(label="r = .30", x=1.5,y=1.5,family="Lucida Sans")
s2

# export s2
png("HW Plots/saycheese17.png",
    units="in", 
    width=4, height= 4, res = 200)
s2
dev.off()


# r2 and RMSE
mp <- model_performance(lm1, standardize = "refit")
mp
mp2 <- model_performance(lm2)
mp2
