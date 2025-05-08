# load packs----
library(tidyverse)
library(psych)
library(DescTools)
library(skimr)
library(freqtables)
library(cowplot)
library(extrafont)
library(wesanderson)
library(gridExtra)
library(paletteer)
library(haven)
library(sjmisc)
library(janitor)
library(ggsci)
library(ggbeeswarm)
library(ggforce)
library(ggridges)
library(ghibli)
library(systemfonts)
library(viridis)
library(RColorBrewer)
library(treemapify)

# distribution of a single variable----

# import data
cheese <- read_csv("MDD_Small.csv")

# 1. histrogram----
c1 <- ggplot(cheese, 
             aes(x=weight))+
  geom_histogram(fill= "#F2AD00",
                 color="#F98400",
                 alpha = .45,
                 bins=12)+
  scale_y_continuous(expand=c(0,0),
                     limits = c(0,15))+
  labs(title="A)",
       x="Weight",
       y="Frequency")+
  theme_cowplot(font_family="Microsoft Tai Le")+
  theme(axis.ticks=element_blank())
c1

# 2. density plot----
c2 <- ggplot(cheese,
             aes(x=weight,
                 y=after_stat(count)))+
  geom_density(fill="#F2AD00",
               color="#F98400",
               alpha=.5)+
  scale_y_continuous(limit=c(0,1),
                     expand=c(0,0))+
  scale_x_continuous(limit=c(105,290))+
  labs(title="B)",
       x="Weight",
       y="Density (Count)")+
  theme_cowplot(font_family = "Microsoft Tai Le")+
  theme(axis.ticks=element_blank())
c2

# export both as png
png("HW Plots/saycheese.png",
    units="in", 
    width=3.5, height= 7, res = 200)
grid.arrange(c1, c2,nrow=2, ncol=1)  
dev.off()

# group distributions----
# recode the data
cheeseg <- cheese %>% mutate_if(is.character,as.numeric) %>%
  select(id,gender, social_anhedonia)

# 1. density plot----
c4 <- ggplot(cheeseg, aes(x=social_anhedonia,
                          y=after_stat(count),
                          group=factor(gender),
                          fill=factor(gender)))+
  geom_density(color="white",
               alpha=.60,
               linewidth=1)+
  scale_y_continuous(limit=c(0,10),
                     expand=c(0,0))+
  scale_fill_manual(values=c("#F2AD00","#B40f20"),
                    name=NULL,
                    labels=c("Male","Female"))+
  labs(x="Social Anhedonia", y="Density (Count)")+
  theme_cowplot(font_family="Microsoft Tai Le")+
  theme(axis.ticks=element_blank(),
        legend.position = "top")
c4

# export as png
png("HW Plots/saycheese02.png",
    units="in", 
    width=5, height= 3.5, res = 300)
c4
dev.off()

# plots of many distributions----
# recode the data
# think about using shared data set and recoding
cheesem <- cheese %>% 
  select(id,bas_drive:bas_reward) %>%
  pivot_longer(cols = bas_drive:bas_reward ,
               names_to = "trait",
               values_to = "score")

# 1. boxplot----
c5 <- ggplot(cheesem, aes(x=trait, y=score, fill=trait))+
  geom_boxplot(color="black",
               alpha=.8,
               show.legend=FALSE)+
  geom_jitter(color = "black", 
              width = .1, 
              alpha=.2,
              size=2,
              show.legend=FALSE)+
  ylim(c(1.75,4))+
  scale_x_discrete(labels=c("bas_drive"="Drive",
                            "bas_funseeking"="Fun Seeking",
                            "bas_reward"="Reward"))+
  labs(x="BAS Dimensions", y="Average Score")+
  scale_fill_paletteer_d("wesanderson::FantasticFox1",
                    direction=-1)+
  theme_minimal_hgrid(font_family = "Inter 18pt")+
  theme(axis.ticks=element_blank(),
        axis.title.x = element_blank())
c5

# explort as png
png("HW Plots/saycheese03.png",
    units="in", 
    width=5, height= 3.5, res = 300)
c5
dev.off()

# 2. violin (plus sina) plot----
c6 <- ggplot(cheesem, aes(x=trait,y=score,fill=trait))+
  geom_violin(color="black",
              alpha=.70,
              show.legend=FALSE)+
  ylim(c(1.75,4))+
  scale_x_discrete(labels=c("bas_drive"="Drive",
                            "bas_funseeking"="Fun Seeking",
                            "bas_reward"="Reward"))+
  labs(x="BAS Dimensions", y="Average Score")+
  scale_fill_paletteer_d("ghibli::LaputaMedium",
                         direction=-1)+
  theme_minimal_hgrid(font_family = "Inter 18pt")+
  theme(axis.ticks=element_blank(),
        axis.title.x = element_blank())+
  geom_sina(color="black",
            size=3,
            alpha=.5,
            show.legend = FALSE)
c6

# export
png("HW Plots/saycheese04.png",
    units="in", 
    width=5, height= 3.5, res = 300)
c6
dev.off()

# beeswarm
c7 <- ggplot(cheesem, aes(x=trait, y=score, color=trait))+
  geom_beeswarm(method="center",
                side=0,
                size=2.5,
                cex=1.75,
                alpha= 1,
                show.legend=F)+
  ylim(c(1.75,4))+
  scale_x_discrete(labels=c("bas_drive"="Drive",
                            "bas_funseeking"="Fun Seeking",
                            "bas_reward"="Reward"))+
  labs(x="BAS Dimensions",y="Average SCore")+
  scale_color_paletteer_d("ghibli::LaputaMedium",
                         direction=-1)+
  theme_minimal_hgrid(font_family = "Inter 18pt")+
  theme(axis.ticks=element_blank())
c7

# 3. ridgeline plot----
c8 <- ggplot(cheesem, aes(x=score,y=trait))+
  geom_density_ridges(aes(fill=trait),
                      show.legend=F,
                      scale=2,
                      alpha=.6,
                      color="white",
                      linetype=1,
                      lwd=1)+
  scale_y_discrete(labels=c("bas_drive"="Drive",
                            "bas_funseeking"="Fun Seeking",
                            "bas_reward"="Reward"))+
  scale_fill_paletteer_d("wesanderson::FantasticFox1",
                          direction=-1)+
  xlim(c(1,5))+
  labs(x="Average Score")+
  theme_ridges(font_family="Inter 18pt")+
  theme(axis.ticks=element_blank(),
        axis.title.y=element_blank())+
  coord_cartesian(clip="off")
c8

# export
png("HW Plots/saycheese05.png",
    units="in", 
    width=5, height= 3.5, res = 300)
c8
dev.off()

# export multiple as png
png("HW Plots/saycheese03-05.png",
    units="in", 
    width=3.5, height= 7, res = 200)
grid.arrange(c5, c6,c8,nrow=3, ncol=1)  
dev.off()

# plotting categorical variables----
# 1. order the variables----
cheese$height_ord <- split_var(cheese$height,
                               n=3) %>%
  ordered()

cheeseh <- cheese %>%
  select(height, height_ord)

c9 <- ggplot(cheese, aes(x=height_ord, color=height_ord, fill=height_ord))+
  geom_bar(alpha=.6,show.legend=FALSE,
           fill=my_colors,
           color=my_colors)+
  scale_x_discrete(labels=c("3"="Above Average",
                            "2"="Average",
                            "1"="Below Average"))+
  labs(title="Height", x=NULL, y="Count")+
  theme_minimal_vgrid(font_family="Inter 18pt")+
  coord_flip()
c9

# export as png
png("HW Plots/saycheese06.png",
    units="in", 
    width=5, height= 3.5, res = 200)
c9 
dev.off()

# 2. nominal----
# 2a. bar plot----
cheesen <- read.csv("RAW_Shared_Data_Prep.csv") %>%
  mutate_if(is.character,as.numeric) %>%
  slice(-(1:2)) %>%
  select(cat_dog) %>%
  mutate(person_type=cut(
    cat_dog,
    breaks=4,
    labels=c("Cat","Dog","Both","Neither")
  ))
my_colors <- paletteer_d("wesanderson::FantasticFox1")[c(1,4)]

c10 <- ggplot(drop_na(cheesen), aes(x=fct_rev(fct_infreq(person_type))))+
  geom_bar(fill=my_colors[1],
           color=my_colors[2],
           alpha=.65)+
  scale_y_continuous(limits=c(0,70),
                   breaks=seq(0,70,10))+
  labs(title="Pet Preference",x=NULL, y=NULL)+
  theme_minimal_vgrid(font_family = "Lucida Sans")+
  coord_flip()
c10

# export as png
png("HW Plots/saycheese07.png",
    units="in", 
    width=5, height= 3.5, res = 200)
c10
dev.off()

# 2b. tree plot----
# make a freq table
antable <- freq_table(cheesen,person_type) %>%
  select(cat,n) %>%
  rename("Animal_Type"="cat",
         "Frequency"="n") %>%
  slice(-(5))

# make a tree plot 
c11 <- ggplot(antable, aes(area=Frequency,
                           fill=Animal_Type,
                           label=paste(Animal_Type,Frequency, sep="\n")))+
  geom_treemap(color="white",
               size=4,
               show.legend=F)+
  geom_treemap_text(color="white",
                    place="centre",
                    grow=F,
                    reflow=F,
                    show.legend=F,
                    aes(family="Lucida Sans"))+
  scale_fill_paletteer_d("wesanderson::GrandBudapest2")
c11

# export as png
png("HW Plots/saycheese08.png",
    units="in", 
    width=5, height= 3.5, res = 200)
c11
dev.off()

# export bar plot and treemap together for homework
png("HW Plots/saycheese07-08.png",
    units="in", 
    width=3.5, height= 7, res = 200)
grid.arrange(c11, c10,nrow=2, ncol=1)  
dev.off()
