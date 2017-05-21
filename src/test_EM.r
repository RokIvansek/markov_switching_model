library(dplyr)
library(xlsx)
library(ggplot2)

setwd("~/Documents/Faks/Matematika_z_racunalnikom/src")
df <- read.xlsx(file = '../data/RSExample_MSCIUS.xls', sheetIndex=1,  header = TRUE)
df <- df %>%
  rename(dateX = NA.)

ggplot() + 
  geom_line(data = df, aes(x = dateX, y = return), colour='blue')

# Testing the em implementation
source("em.r")

returns <- df$return

# Initial values for parameters
# Base regime
mi0 <- 0.04
sigma0 <- 1
# Jump regime
mi1 <- -0.04
sigma1 <- 4
# Switcing variable
p00 <- 0.80 # prob to switch to regime 2 when in regime 1
p11 <- 0.80 # prob to switch to regime 1 when in regime 2
fi <- 0.5 # fi is probability that the model initializes in regime 1

tmp <- inferenceForecastRecursion(mi0, sigma0, mi1, sigma1, p00, p11, fi, returns)
fors0 <- tmp$forecasts
fors1 <- 1 - fors0
infs0 <- tmp$inferences
infs1 <- 1 - infs0
smoothInfs0 <- tmp$smoothedInferences
smoothInfs1 <- 1 - smoothInfs0

logLikelihood(mi0, sigma0, mi1, sigma1, returns, fors0)

maximizeParameters(mi0, sigma0, mi1, sigma1, p00, p11, fi, returns, infs0, fors0, smoothInfs0)

optimalParams <- EMalgorithm(mi0, sigma0, mi1, sigma1, p00, p11, fi, returns)
optimalParams





