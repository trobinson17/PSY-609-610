# load packs----
library(tidyverse)
library(psych)
library(DescTools)
library(skimr)
library(freqtables)
library(cowplot)
library(extrafont)
library(gridExtra)
library(wesanderson)
library(ggforce)

# import data----
playtime <- read_csv("MDD_Small.csv")

# histrogram----
p1 <- ggplot(playtime, 
       aes(x=insomnia_night))+
  geom_histogram(fill="royalblue2",
                 color="violet",
                 alpha = .45,
                 bins=10)+
  labs(title="Histogram Madness",
       x="Insomnia Night",
       y="Count")+
  theme_minimal_hgrid(font_family="Gill Sans MT")+
  theme(axis.ticks=element_blank())
p1

# p1 with the linedraw theme
p1 + theme_linedraw()
p1 + theme_half_open() # Tyler likes this one

# density plot----
# can also add theme at the very end of the code
p2 <- ggplot(playtime,
             aes(x=insomnia_night))+
  geom_density(fill="turquoise2",
               color="springgreen2",
               alpha=.5)+
  labs(title="Density Plot Madness",
       x="Insomnia Night",
       y="Density")+
  theme_minimal_hgrid(font_family = "Gill Sans MT")+
  theme(axis.ticks=element_blank())
p2

# export both as png----
png("HW Plots/Double_Madness.png",
    units="in", 
    width=3.5, height= 7, res = 200)
grid.arrange(p1, p2,nrow=2, ncol=1)  

dev.off()

# ggplot themes: https://ggplot2.tidyverse.org/reference/ggtheme.html
# theme_minimal() is good 
# cowplot:: theme_classic() is also good

# pivot and ugly boxplot----
small <- playtime %>% select(id, dass_depress:dass_stress)
small_long <- small %>% 
  pivot_longer(cols = dass_depress:dass_stress,
               names_to = "Trait",
               values_to = "Score")
ggplot(small_long, aes(x=Trait, y=Score))+
  geom_violin()+
  geom_sina()
