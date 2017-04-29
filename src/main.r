library(dplyr)
library(ggplot2)
library(deseasonalize)

setwd("~/Documents/Faks/Matematika_z_racunalnikom/src")

load(file='../data/dnevne_cene_2013_2015.RData')

ggplot(data = df, aes(x = datum, y = cena_EUR_MWh)) +
  geom_line(colour="blue")

# DESEASONALIZE

# Fitting and subtracting a sine curve for seasons on anual level
y = df$cena_EUR_MWh
t = 1:1077

ssp <- spectrum(y)  
per <- 1/ssp$freq[ssp$spec==max(ssp$spec)]
reslm <- lm(y ~ sin(2*pi/per*t)+cos(2*pi/per*t))
summary(reslm)

rg <- diff(range(y))
plot(y~t,ylim=c(min(y)-0.1*rg,max(y)+0.1*rg))
lines(fitted(reslm)~t,col=4,lty=2)   # dashed blue line is sin fit

# including 2nd harmonic really improves the fit
reslm2 <- lm(y ~ sin(2*pi/per*t)+cos(2*pi/per*t)+sin(4*pi/per*t)+cos(4*pi/per*t))
summary(reslm2)
lines(fitted(reslm2)~t,col=3)    # solid green line is periodic with second harmonic

fitted(reslm)

df$reslm <- fitted(reslm)
df$reslm2 <- fitted(reslm2)

ggplot() + 
  geom_line(data = df, aes(x = datum, y = cena_EUR_MWh), colour='blue') +
  geom_line(data = df, aes(x = datum, y = reslm), colour='red')  +
  geom_line(data = df, aes(x = datum, y = reslm2), colour='green')

# After subtracting the price is now centered around zero
ggplot(data = df, aes(x = datum, y = cena_EUR_MWh - reslm)) +
  geom_line(colour="red")

ggplot(data = df, aes(x = datum, y = cena_EUR_MWh - reslm2)) +
  geom_line(colour="green")

df <- df %>%
  mutate(
    cena_EUR_MWh_reslm = cena_EUR_MWh - reslm,
    cena_EUR_MWh_reslm2 = cena_EUR_MWh - reslm2
  )

# Subtracting a week average to remove weekly seasonality
df <- df %>% mutate(teden = week(datum))
df <- df %>% mutate(leto = year(datum))

df <- df %>%
  group_by(leto, teden) %>%
  mutate(
    tedensko_povprecje_reslm = mean(cena_EUR_MWh_reslm, na.rm=TRUE),
    tedensko_povprecje_reslm2 = mean(cena_EUR_MWh_reslm2, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    cena_final_reslm = cena_EUR_MWh_reslm - tedensko_povprecje_reslm,
    cena_final_reslm2 = cena_EUR_MWh_reslm2 - tedensko_povprecje_reslm2
  )
  
  
  
  
  
