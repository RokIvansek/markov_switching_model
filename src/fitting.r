library(dplyr)
library(lubridate)
library(ggplot2)

setwd("~/Documents/Faks/Matematika_z_racunalnikom/markov_switching_model/src")

load(file='../data/dnevne_cene_2013_2015_deseasonalized.RData')

df <- df %>%
  select(datum, cena_EUR_MWh_deseasonalized) %>%
  rename(cena = cena_EUR_MWh_deseasonalized)

# Split to train and test
split_date = mdy("01-01-2015")
train_data <- df %>% filter(datum < split_date)
test_data <- df %>% filter(datum >= split_date)

ggplot() + 
  geom_line(data = train_data, aes(x = datum, y = cena), colour='blue') +
  geom_line(data = test_data, aes(x = datum, y = cena), colour='red')

# We assume the model with normal distributions in both regimes and a bernoulli distribution for goverening the switching
# Actualy data looks like it has two regimes, but both are kind of the same (one isn't jumpier than the other). Both are very jumpy.

# Initial values for parameters
# Regime1
mi0 <- 5
sigma0 <- 10
# Regime2
mi1 <- -5
sigma1 <- 15
# Switcing variable
p00 <- 0.55 # prob to switch to regime 2 when in regime 1
p11 <- 0.6 # prob to switch to regime 1 when in regime 2
fi <- 0.4 # fi is probability that the model initializes in regime 1


# Training

# Predicting
source("predict.r")
modelData <- model(mi0, sigma0, mi1, sigma1, p00, p11, fi)
predictions <- modelData$prediction
regimes <- modelData$regimes

predictData <- data.frame(
  x = 1:length(predictions),
  values = predictions,
  regimes = regimes
)

#
ggplot(data = predictData, aes(x = x, y = values, colour=regimes)) + 
  geom_line(aes(group=1))

