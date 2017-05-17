library(dplyr)
library(lubridate)

setwd("~/Documents/Faks/Matematika_z_racunalnikom/markov_switching_model/src")

load(file='../data/dnevne_cene_2013_2015_deseasonalized.RData')

df <- df %>%
  select(datum, cena_final_reslm2) %>%
  rename(cena = cena_final_reslm2)

# Split to train and test
split_date = mdy("07-01-2015")
train_data <- df %>% filter(datum < split_date)
test_data <- df %>% filter(datum >= split_date)

ggplot() + 
  geom_line(data = train_data, aes(x = datum, y = cena), colour='blue') +
  geom_line(data = test_data, aes(x = datum, y = cena), colour='red')


# We assume the model with normal distributions in both regimes and a bernoulli distribution for goverening the switching

# Initial values for parameters
set.seed(42)

# Base regime
mi_base <- 0
sigma_base <- 8

# Jump regime
mi_jump <- 0
sigma_jump <- 20

# Switcing variable
prob1 <- 0.01 # prob to switch to jump when in base regime
prob2 <- 0.98 # prob to switch to base when in jump regime





