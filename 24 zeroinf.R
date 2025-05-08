# load packs
library(MASS)
library(pscl)

library(tidyverse)
library(easystats)
library(psych)

library(jtools)
library(effects)
library(car)
library(freqtables)

library(gridExtra)
library(paletteer)
library(extrafont)
library(cowplot)

# load data
cs <- read_csv("Shared_Data.csv") %>%
  select(pol_overall, pol_social, own_car, foreign_lang,
         travel_count) %>%
  datawizard::standardise(1:2)

# describe outcome
describe(cs$travel_count)
# mean = 5.99, var = 167.44

# model 1 
m1 <- glm(travel_count ~  pol_overall + pol_social + own_car +
            foreign_lang,
          data = cs,
          family = poisson(link = "log"))
model_parameters(m1)
check_overdispersion(m1)
check_zeroinflation(m1)

# model 2
m2 <- glm.nb(travel_count ~ pol_overall + pol_social + own_car +
               foreign_lang,
             data = cs,
             link = "log")
summary(m2)
model_parameters(m2)
m2_alpha <- 1/m2$theta 
m2_alpha
check_overdispersion(m2)

# model 3
m3 <- zeroinfl(travel_count ~ pol_overall + pol_social + own_car +
                 foreign_lang,
               data = cs,
               dist = "poisson",
               link = "logit")

# model 4
m4 <- zeroinfl(travel_count ~ pol_overall + pol_social + own_car +
                 foreign_lang,
               data = cs,
               dist = "negbin",
               link = "logit")
model_parameters(m4)

# zeroinfl model comparison
compare_performance(m3, m4,
                    metrics = c("AIC", "BIC"))

# zeroinfl and negbin model comparison
compare_performance(m4, m2,
                    metrics = c("AIC", "BIC"))

# feeder dataframe
csf <- data.frame("pol_overall" = mean(cs$pol_overall),
                  "pol_social" = cs$pol_social,
                  "own_car" = mean(cs$own_car),
                  "foreign_lang" = mean(cs$foreign_lang))

# predicting zeros and counts
pred_count <- data.frame("p_zeros" = predict(m4,
                                             newdata = csf,
                                             type = "zero"),
                         "p_counts" = predict(m4,
                                              newdata = csf,
                                              type = "count"),
                         "p_overall" = predict(m4,
                                               newdata = csf,
                                               type = "response"))
view(pred_count)
csnew <- data.frame(pred_count, cs)
view(csnew)

# plots
# counts
p1 <- ggplot(csnew, aes(x = pol_social, y = p_counts))+
  geom_point(size=3, alpha=.7, color="#F98400")+
  geom_line(aes(y=p_counts), color="#F98400", alpha=.4, linewidth=1.5)+
  labs(x="Social Political Ideology (z)", y="Predicted Poisson Counts")+
  xlim(c(-2, 2))+
  ylim(c(0,12))+
  theme_minimal_grid(font_family = "Lucida Sans")
p1

# zeros
p2 <- ggplot(csnew, aes(x = pol_social, y = p_zeros))+
  geom_point(size=3, alpha=.7, color="#F2AD00")+ 
  geom_line(aes(y=p_zeros), color="#F2AD00", alpha=.4, linewidth=1.5)+
  labs(x="Social Political Ideology (z)", y="Prob of Zero Inflation")+
  xlim(c(-2,2))+
  ylim(c(0,1))+
  theme_minimal_grid(font_family = "Lucida Sans")
p2

# export plots
# export plots
png("HW Plots/saycheese46.png",
    units="in", 
    width=12, height= 4, res = 300)
grid.arrange(p1, p2,nrow=1, ncol=2)  
dev.off()