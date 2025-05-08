# load packs
library(MASS)

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
  select(mindful_avg:impulse_avg, machiavell_avg,
         accidents_count, age) %>%
  datawizard::standardise(1:3)

# descriptives of outcome
describe(cs$accidents_count) # m = 1.6, v = 3.2
hist(cs$accidents_count)

# histogram of car accidents
h1 <- ggplot(cs, aes(x = accidents_count)) +
  geom_histogram(fill= "#F2AD00",
                 color="#F98400",
                 alpha = .45,
                 bins=12) +
  scale_y_continuous(limits = c(0,75),
                     n.breaks = 10,
                     expand = c(0,0)) +
  labs(x = "Lifetime Car Accidents (Count)", y = "Frequency") +
  theme_minimal_hgrid(font_family = "Lucida Sans")
h1

# export histogram
png("HW Plots/saycheese45.png",
    units="in", 
    width=5, height=5, res = 300)
h1
dev.off()

# model 1
m1 <- glm(accidents_count ~ mindful_avg + impulse_avg +
            machiavell_avg, data = cs,
          family = poisson(link="log"))
model_parameters(m1)
model_parameters(m1, exponentiate = TRUE)
r2_mcfadden(m1)
check_overdispersion(m1)

# m1 plot
p1 <- effect_plot(model = m1,
                  pred = machiavell_avg,
                  data = cs,
                  outcome.scale = "response", 
                  colors = "#F98400") +
  ylim(0,10) +
  labs(x = "Machiavellianism", y = "Lifetime Car Accidents (Count)",
       title = "Model 1") +
  theme_minimal_grid()
p1

# model 2
m2 <- glm.nb(accidents_count ~ mindful_avg + impulse_avg +
               machiavell_avg, data = cs, link = "log")
model_parameters(m2)
model_parameters(m2, exponentiate = TRUE)
summary(m2)
m2_alpha <- 1/m2$theta 
m2_alpha
r2_mcfadden(m2)
check_overdispersion(m2)

# m2 plot
p2 <- effect_plot(model = m2,
                  pred = machiavell_avg,
                  data = cs,
                  outcome.scale = "response", 
                  colors = "#F98400") +
  ylim(0,10) +
  labs(x = "Machiavellianism", y = "Lifetime Car Accidents (Count)",
       title = "Model 2") +
  theme_minimal_grid()
p2

# model comparison 
compare_models(m1, m2, select="short")
compare_performance(m1, m2, 
                    metrics = c("AIC", "BIC")) 

# AICs and BICs drop considerably from model 1 to 2

# age as an offset
m3 <- glm.nb(accidents_count ~ mindful_avg + impulse_avg +
               machiavell_avg + offset(log(age)),
             data = cs, link = "log")
model_parameters(m3)
model_parameters(m3, exponentiate = TRUE)
summary(m3)
m3_alpha <- 1/m3$theta                    
m3_alpha
r2_mcfadden(m3)
check_overdispersion(m3)


# export plots
png("HW Plots/saycheese44.png",
    units="in", 
    width=12, height= 4, res = 300)
grid.arrange(p1, p2,nrow=1, ncol=2)  
dev.off()