library(tidyverse)
library(sjmisc)
library(psych)
library(DescTools)
library(gridExtra)
library(paletteer)
library(extrafont)
library(cowplot)
library(easystats)

# load in dataset
meanpre <- read_csv("MDD_Small.csv")

# pull variable "weight" and determine se
se <- describe(meanpre$weight)
View(se)

samdis<-data.frame("weight"=rnorm(100000,
                         mean=162.01,
                         sd=4.84))

# 95% CI
my_cis<-qnorm(c(.025, .975), mean = 162.0129, sd = 4.84) %>% round(2)

sd1<-ggplot(samdis, aes(x=weight))+
  geom_density(linewidth=.5, color="black", fill="#F98400", alpha=.5)+
  labs(x = "Sampling Distribution of Weight", y=NULL)+
  scale_y_continuous(expand=c(0,0),
                     limits=c(0,0.09))+
  geom_vline(xintercept = my_cis,
             color="black",
             lwd=.8,
             linetype="dotted")+
  theme_minimal_hgrid(font_family="Lucida Sans")+
  theme(axis.ticks.x=element_blank())
sd1

# export as png
png("HW Plots/saycheese09.png",
    units="in", 
    width=5, height= 3.5, res = 300)
sd1
dev.off()