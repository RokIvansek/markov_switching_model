library(dplyr)
library(lubridate)
library(ggplot2)

# setwd("~/Documents/Faks/Matematika_z_racunalnikom/markov_switching_model/src")
setwd("~/Documents/Faks/Matematika_z_racunalnikom/src")

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
  geom_line(data = test_data, aes(x = datum, y = cena), colour='red') +
  labs(title = "Deseasonalized electricity spot prices\n", x = "Date", y = "Deseasonalized spot price")

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
source("em.r")
optimalParams <- EMalgorithm(mi0, sigma0, mi1, sigma1, p00, p11, fi, train_data$cena, printSteps=TRUE)
mi0 <- optimalParams$mi0
sigma0 <- optimalParams$sigma0
mi1 <- optimalParams$mi1
sigma1 <- optimalParams$sigma1
p00 <- optimalParams$p00
p11 <- optimalParams$p11
fi <- optimalParams$fi

# Predicting
source("predict.r")
n <- length(test_data$datum)

modelData <- model(mi0, sigma0, mi1, sigma1, p00, p11, fi, steps=n)
predictions <- modelData$prediction
regimes <- modelData$regimes

# TODO: Mogoče dodajaj postopoma
plotPredictionTrue(predictions, test_data$cena, test_data$datum, regimes)
# TODO: Dodaj metriko MAPE, pri obeh povpreči za m predikcij in zračunaj še sigmo
rmseMetric <- rmse(predictions, test_data$cena)
print(rmseMetric)
mapeMetric <- mape(predictions, test_data$cena)
print(mapeMetric)

RMSEs <- 1:10000
MAPEs <- 1:10000

for (i in 1:10000) {
  modelData <- model(mi0, sigma0, mi1, sigma1, p00, p11, fi, steps=n)
  predictions <- modelData$prediction
  rmseMetric <- rmse(predictions, test_data$cena)
  # print(rmseMetric)
  mapeMetric <- mape(predictions, test_data$cena)
  # print(mapeMetric)
  RMSEs[i] <- rmseMetric
  MAPEs[i] <- mapeMetric
}

mean(RMSEs)
sd(RMSEs)
mean(MAPEs)
sd(MAPEs)
