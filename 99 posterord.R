# load packs
library(tidyverse)
library(easystats)
library(psych)
library(freqtables)
library(janitor)

library(ordinal)

library(effects)
library(car)

library(gridExtra)
library(paletteer)
library(extrafont)
library(cowplot)

# load data 
cs <- read_csv("poster_data_ex.csv") %>%
  select(sth_mean, pol2_1, conf5_r) %>%
  datawizard::standardize(1:2)
describe(cs)

cs$conf5_r <- factor(cs$conf5_r)

cs$conf_ord <- plyr::revalue(cs$conf5_r,
                             c("1" = "0",
                               "2" = "1",
                               "3" = "2",
                               "4" = "3",
                               "5" = "4",
                               "6" = "5",
                               "7" = "6"))

# model 1
m1 <- clm(conf_ord ~ pol2_1 + sth_mean,
          link = "logit",
          data = cs)
summary(m1)
model_parameters(m1)
ordinal::nominal_test(m1) # satisfied prop odds assumption
performance::r2_mcfadden(m1) # model evaluation


# odds and probs
odds <- exp(m1$Theta) %>% round(2)
prob <- odds/(1+odds) %>% round(2)
prob

# estimate means for plot
m1p <- estimate_means(m1,
                      by = "pol2_1 = seq(-2.5, 2.5, .1)")
view(m1p)

# plot
p1 <- ggplot(m1p, aes(x = pol2_1, y = Probability, color = Response))+
  geom_line(alpha = .6, linewidth = 1)+
  scale_x_continuous(n.breaks = 7)+
  labs(x = "Political Ideology (z)", y = NULL,
       title = "Response Probs")+
  ylim(c(0, 1))+
  theme_minimal_hgrid(font_family = "Calibri Light")
p1

pf <- p1 + scale_color_paletteer_d("RColorBrewer::YlOrRd", direction = -1,
                                   labels = c("Strongly Oppose", " ", " ",
                                              " ", " ", " ", 
                                              "Strongly Support"))
pf

# export
png("HW Plots/saycheese47.png",
    units="in", 
    width=9, height= 6, res = 300)
pf
dev.off()