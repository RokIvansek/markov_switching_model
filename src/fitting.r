library(dplyr)

setwd("~/Documents/Faks/Matematika_z_racunalnikom/src")

load(file='../data/dnevne_cene_2013_2015_deseasonalized.RData')

df <- df %>%
  select(datum, cena_final_reslm2)