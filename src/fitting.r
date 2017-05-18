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
# Base regime
mi1 <- 5
sigma1 <- 10
# Jump regime
mi2 <- -5
sigma2 <- 15
# Switcing variable
tau1 <- 0.55 # prob to switch to regime 2 when in regime 1
tau2 <- 0.6 # prob to switch to regime 1 when in regime 2
fi <- 0.4 # fi is probability that the model initializes in regime 1


# Training
trainWithEmAlgo <- function(mi1, sigma1, mi2, sigma2, tau1, tau2, fi, tsData) {
  # Get the set of probabilites for each observation, take switching probabilities in account
  T1 <- tau1 * dnorm(tsData, mean=mi1)
  T2 <- tau2 * dnorm(tsData, mean=mi2)
  
  head( T_1 / (T_1 + T_2) )
}

trainWithEmAlgo(mi1, sigma1, mi2, sigma2, prob1, prob2, train_data$cena)


