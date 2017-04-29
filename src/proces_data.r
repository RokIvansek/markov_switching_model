library(readODS)
library(dplyr)
library(lubridate)

setwd("~/Documents/Faks/Matematika_z_racunalnikom/src")

filePath <- '../data/Cp_in_Cn_2012_SL.ods'
n <- get_num_sheet_in_ods(filePath)

df <- read_ods(filePath, 1, skip=9, col_names=F)
# df_colnames <- c("datum", "cena_EUR_MWh")
df <- df %>%
  mutate(datum = dmy(substr(A, 1, 10))) %>%
  rename(cena_EUR_MWh = B) %>%
  select(datum, cena_EUR_MWh) %>%
  group_by(datum) %>%
  summarise(cena_EUR_MWh = mean(cena_EUR_MWh, na.rm=TRUE))

for (i in 2:n){
  tmp <- read_ods(filePath, i, skip=9, col_names=F)
  tmp <- tmp %>%
    mutate(datum = dmy(substr(A, 1, 10))) %>%
    rename(cena_EUR_MWh = B) %>%
    select(datum, cena_EUR_MWh) %>%
    group_by(datum) %>%
    summarise(cena_EUR_MWh = mean(cena_EUR_MWh, na.rm=TRUE))
  df <- rbind(df, tmp)
}

save(df, file='../data/dnevne_cene_2013.RData')

load('../data/dnevne_cene_2013.RData')
df2013 <- df
load('../data/dnevne_cene_2014.RData')
df2014 <- df
load('../data/dnevne_cene_2015.RData')
df2015 <- df
rm(df)

df <- rbind(df2013, df2014, df2015)
save(df, file='../data/dnevne_cene_2013_2015.RData')
