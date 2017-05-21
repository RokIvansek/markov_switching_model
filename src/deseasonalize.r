library(dplyr)
library(ggplot2)
library(lubridate)
library(TSA)
# library(deseasonalize)

setwd("~/Documents/Faks/Matematika_z_racunalnikom/markov_switching_model/src")
# setwd("~/Documents/Faks/Matematika_z_racunalnikom/src")

load(file='../data/dnevne_cene_2013_2015.RData')
# load(file='../data/dnevne_cene_2015.RData')

ggplot(data = df, aes(x = datum, y = cena_EUR_MWh)) +
  geom_line(colour="green")

# DESEASONALIZE NEW

# Detect the seasonality using fourier transform
p <- periodogram(df$cena_EUR_MWh)

# display the 4 highest "power" frequencies
dd <- data.frame(freq=p$freq, spec=p$spec)
top <- dd %>%
  arrange(desc(spec)) %>%
  mutate(period_in_day = 1/freq) %>%
  filter(spec > 2500)

top

# Convert to time series
electricityTS <- ts(df$cena_EUR_MWh,
                    start = c(year(first(df$datum)), 1),
                    end = c(year(last(df$datum)), 365),
                    frequency = 365)

# plot(electricityTS)

# Decomposition using stl
# TODO: How does this decomposition really work? How does it determine what is seasonal or what is not?
# TODO: Something to do with the periodogram? One would think it takes some threshold and cuts out the frequencies with the highest spec in periodogram.
stlFit = stl(electricityTS, s.window = 'periodic')
plot(stlFit)
remainder <- stlFit$time.series[,3]
plot(remainder)

df$cena_EUR_MWh_deseasonalized <- remainder

save(df, file='../data/dnevne_cene_2013_2015_deseasonalized.RData')
