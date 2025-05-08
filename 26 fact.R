# load packs
library(tidyverse)
library(easystats)
library(psych)
library(jtools)

# load data
cs <- read_csv("reliability_data.csv") %>%
  select(sys1_1:iso2_2)

# factor analysis on systemic racism 
fa1 <- fa(cs, fm = "pa", nfactors = 2, rotate = "oblimin")
fa1
fa1$loadings

# scree and eigenvalues
p1 <- scree(cs, factors = TRUE, pc = FALSE)
p1

# parallel analysis
fa2 <- fa.parallel(cs, fm = "pa", fa = "fa", sim = FALSE, error.bars = TRUE,
                   n.iter = 500)
fa2