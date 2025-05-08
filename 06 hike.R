# packages 
library(tidyverse)
library(easystats)  
library(sjmisc)
library(psych)

library(car)
library(cannonball)

library(ggcorrplot)
library(gridExtra)
library(paletteer)
library(extrafont)
library(cowplot)
library(plotly)
library(colorblindr)
library(RColorBrewer)

# load 
st <- read_csv("cstressuncg_clean.csv")
stcor <- st %>% dplyr::select(`Direct Avg. UNCG`:res_se_av)
stcor1 <- st %>% dplyr::select(`Life Satisfaction`:Health_Behavior,res_se_av)

# compute and report correlations----
dqls <- correlation::cor_test(x="Direct Acq.", y="Life Satisfaction", 
                              data=stcor)
dqls
dar <- correlation::cor_test(x="Direct Avg. UNCG", 
                             y="Resilience", 
                             data=stcor)
dar
rls <- correlation::cor_test(x="Resilience", 
                             y="Life Satisfaction", 
                             data=stcor)
rls

# matrix plot---- 
stmat <- stcor %>% dplyr::select(`Life Satisfaction`:Health_Behavior)

m1 <- cor(stmat)
ggcorrplot(m1, method="circle")



matplot <- ggcorrplot(m1, method = "square", 
           type = "lower",
           lab=TRUE,
           lab_col = "black",
           lab_size = 4,
           outline.color = "black",
           show.legend = T,
           show.diag = T,
           colors=c("#9e0142","white","#66c2a5"),
           hc.order = T,
           insig="blank")+
  labs(x=NULL, y=NULL)+
  scale_x_discrete(labels = c("Self Esteem"="Self-esteem",
                               "General_Health"="General Health",
                              "Health_Behavior"="Health Behavior"))+
  scale_y_discrete(labels = c("Self Esteem"="Self-esteem",
                              "General_Health"="General Health",
                              "Health_Behavior"="Health Behavior"))+
  theme_minimal_grid(font_family = "Lucida Sans")+
  theme(axis.text.x=element_text(angle=40,hjust=1))+
  theme(legend.position = "none")
matplot

# export
png("HW Plots/saycheese10.png", 
    width=4, height=4,     
    units="in", res=200)
matplot
dev.off()


# scatterplot---- 
s1 <- ggplot(stcor, aes(x=`Self Esteem`,        
               y=Resilience))+
  geom_point(color="#F98400",
             alpha=.7,
             size=4)+
  geom_smooth(method="lm",
              color="#B40f20",
              fill="grey50",
              se=F)+
  labs(x="Self-esteem", y = "Resilience")+
  theme_minimal_grid(font_family = "Lucida Sans")+
  xlim(1,5)+
  ylim(1,5)
s1

# export
png("HW Plots/saycheese11.png", 
    width=4, height=4,     
    units="in", res=200)
s1
dev.off()


# scatterplot split by variable----
splitcor <- stcor %>%
  mutate(ls_split=dicho(`Life Satisfaction`))

my_colors <- paletteer_d("wesanderson::Zissou1")[c(1,4)]

s2 <- scgroup<-ggplot(splitcor, aes(x=`Self Esteem`, y=Resilience, 
                              color=factor(ls_split)))+  
  geom_point(alpha=.8, size = 4)+
  geom_smooth(method = "lm", se=F,
              linewidth = 1,
              show.legend = F)+
  scale_color_manual(values=my_colors,
                     name=NULL,
                     labels=c("Low LS","High LS"))+
  labs(x="Self-esteem", y = "Resilience")+
  theme_minimal_grid(font_family = "Lucida Sans")+
  theme(legend.position = "top")+
  xlim(1,5) + ylim(1,5)
s2

# export
png("HW Plots/saycheese12.png", 
    width=4, height=4,     
    units="in", res=200)
s2
dev.off()

# export all three
png("HW Plots/saycheese10-12.png",
    units="in", 
    width=12, height= 4, res = 200)
grid.arrange(matplot,s1,s2,nrow=1, ncol=3)  
dev.off()

# overplotted scatterplots----
# contour chart 
op1 <- ggplot(stcor1, aes(x=res_se_av,y=`Life Satisfaction`))+
  stat_density_2d(aes(fill=after_stat(level)),
                  geom="polygon",
                  color="white")+
  scale_fill_paletteer_c("viridis::rocket")+
  labs(title="Contour Chart",
       x = "Comparative Stress",
       y = "Life Satisfaction")+
  guides(fill = guide_colorbar(title="Level"))+  
  theme_minimal_grid(font_family = "Lucida Sans")
op1

# export 
png("HW Plots/saycheese13.png", 
    width=8, height=8,     
    units="in", res=200)
op1
dev.off()

# 3d Kernel Density
library(MASS)
kd<-MASS::kde2d(x=stcor1$res_se_av,
                y=stcor1$`Life Satisfaction`,
                n=40)

op2<-plot_ly(x=kd$x,
             y=kd$y,
             z=kd$z,
             colors="magma") %>% add_surface()

op2

# export
htmlwidgets::saveWidget(op2, "HW Plots/saycheese14.html")


# forest plot dfs
f1cors <- correlation(stcor1, p_adjust = "none")
forest <- f1cors %>%
  slice(1:5) %>%
  select(3,5,6) %>%
  mutate(Variable = c("SE","R","GH","HB","Str. Ex.")) %>%
  as.data.frame()

# forest plot
fp <- ggplot(forest, aes(x=r, y=reorder(Variable, r),
                  xmin=CI_low, xmax=CI_high))+ 
  geom_linerange(linewidth=2.5, color = "#B40f20", 
                 alpha=.3)+
  geom_vline(xintercept=0,                    
             color="black", lwd=1,
             linetype="dotted")+
  geom_point(shape=18,
             size = 6,
             color="#B40f20",
             alpha=1)+
  scale_x_continuous(limits=c(-.7,.7),
                     n.breaks = 10)+
  labs(title="Life Satisfaction, Well-Being, & Stress",
       y=NULL, x="Correlation [95% CI]")+
  theme_minimal_vgrid(font_family = "Lucida Sans")
fp

# export
png("HW Plots/saycheese15.png", 
    width=6, height=6,     
    units="in", res=200)
fp
dev.off()

# export fp and op1
png("HW Plots/saycheese13-15.png",
    units="in", 
    width=12, height= 5, res = 200)
grid.arrange(fp,op1,nrow=1, ncol=2)  
dev.off()