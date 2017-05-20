library(dplyr)
# library(lubridate)
library(xlsx)
library(ggplot2)

df <- read.xlsx(file = '../data/RSExample_MSCIUS.xls', sheetIndex=1,  header = TRUE)
returns <- df$return

inferenceForecastRecursion <- function(mi0, sigma0, mi1, sigma1, p00, p11, fi, values){
  
  # Inititalize empty vectors
  forecastProbs0 <- c()
  inferenceProbs0 <- c()
  
  # Append fi as the first forecast value
  forecastProbs0 <- c(forecastProbs0, fi) 
  
  # Calculate first inference using fi an append it
  inf0 <- (dnorm(values[1], mi0, sigma0)*fi) / ((dnorm(values[1], mi0, sigma0)*fi) +
                                                  dnorm(values[1], mi1, sigma1)*(1-fi))
  inferenceProbs0 <- c(inferenceProbs0, inf0)
  
  # Loop trough all values and recursively calculate inferences and forecasts
  for (i in 2:length(values)){
    
    lastInf <- inferenceProbs0[i-1]
    
    for0 <- p00*lastInf + (1 - p11)*(1 - lastInf)
    forecastProbs0 <- c(forecastProbs0, for0)
    
    lastFor <- forecastProbs0[i]
    inf0 <- (dnorm(values[i], mi0, sigma0)*lastFor) / ((dnorm(values[i], mi0, sigma0)*lastFor) +
                                                          dnorm(values[i], mi1, sigma1)*(1-lastFor))
    inferenceProbs0 <- c(inferenceProbs0, inf0)
  }
  
  # smoothedInfProbs0 <- c(0.058)
  smoothedInfProbs0 <- c(last(inferenceProbs0)) # The last smoothed inference probabilites are the same
  
  # Smooth the inferences
  for (i in 2:length(inferenceProbs0)) {
    smooth0 <- tail(inferenceProbs0, i)[1]*
      (p00*smoothedInfProbs0[i-1]/(tail(forecastProbs0, i-1)[1]) +
      (1-p11)*(1-smoothedInfProbs0[i-1])/(1 - tail(forecastProbs0, i-1)[1]))
    smoothedInfProbs0 <- c(smoothedInfProbs0, smooth0)
  }

  return(list(forecasts=forecastProbs0, inferences=inferenceProbs0, smoothedInferences=rev(smoothedInfProbs0)))
}
  
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

logLikelihood <- function(mi0, sigma0, mi1, sigma1, values, forecastProbs){
  return(sum(log(dnorm(values, mi0, sigma0)*forecastProbs + dnorm(values, mi1, sigma1)*(1-forecastProbs))))
}

logLikelihood(mi0, sigma0, mi1, sigma1, returns, fors0)

maximizeParameters <- function(mi0, sigma0, mi1, sigma1, p00, p11, fi, values, infProbs, forProbs, smoothedInfProbs){
  mi0_ <- sum(smoothedInfProbs*values)/sum(smoothedInfProbs)
  print(mi0_)
  mi1_ <- sum((1-smoothedInfProbs)*values)/sum(1-smoothedInfProbs)
  print(mi1_)
  sigma0_ <- sqrt(sum(smoothedInfProbs*((values - mi0_)**2))/sum(smoothedInfProbs))
  print(sigma0_)
  sigma1_ <- sqrt(sum((1-smoothedInfProbs)*((values - mi1_)**2))/sum(1-smoothedInfProbs))
  print(sigma1_)
  # TODO: Figure this ps out
  p_tilda_00 <- head(infProbs, -1)*(tail(smoothedInfProbs,-1)/head(forProbs,-1))*p00
  p_tilda_01 <- head(1-infProbs, -1)*(tail(smoothedInfProbs,-1)/head(forProbs,-1))*(1-p11)
  # print(p_tilda_00)
  p00_ <- sum(p_tilda_00)/sum(tail(smoothedInfProbs,-1))
  print(p00_)
  p_tilda_11 <- head(1-infProbs, -1)*(tail(1-smoothedInfProbs, -1)/head(1-forProbs, -1))*p11
  p_tilda_10 <- head(infProbs, -1)*(tail(1-smoothedInfProbs, -1)/head(1-forProbs, -1))*(1-p00)
  p11_ <- sum(p_tilda_11)/sum(tail(1-smoothedInfProbs, -1))
  print(p11_)
  print(p_tilda_00 + p_tilda_10)
  print(p_tilda_10 + p_tilda_11)
  fi_ <- smoothedInfProbs[1]
  print(fi_)
}

maximizeParameters(mi0, sigma0, mi1, sigma1, p00, p11, fi, returns, infs0, fors0, smoothInfs0)

EMalgorithm <- function(mi0, sigma0, mi1, sigma1, p00, p11, fi, values){
  
}
