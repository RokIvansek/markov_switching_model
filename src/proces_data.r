library(readODS)
library(dplyr)
library(lubridate)

setwd("~/Documents/Faks/Matematika_z_racunalnikom/src")

filePath <- '../data/Cp_in_Cn_2015_SL.ods'
n <- get_num_sheet_in_ods(filePath)
start_sheet = 2

df <- read_ods(filePath, start_sheet, skip=9, col_names=F)
# df_colnames <- c("datum", "cena_EUR_MWh")
df <- df %>%
  mutate(datum = dmy(substr(A, 1, 10))) %>%
  mutate(ura = substr(A, 12, 13)) %>%
  rename(cena_EUR_MWh = B) %>%
  select(datum, ura, cena_EUR_MWh) # %>%
  # select(datum, cena_EUR_MWh) %>%
  # group_by(datum) %>%
  # summarise(cena_EUR_MWh = mean(cena_EUR_MWh, na.rm=TRUE))
  
hour(df$datum) <- as.integer(df$ura)

for (i in (start_sheet+1):n){
  tmp <- read_ods(filePath, i, skip=9, col_names=F)
  tmp <- tmp %>%
    mutate(datum = dmy(substr(A, 1, 10))) %>%
    mutate(ura = substr(A, 12, 13)) %>%
    rename(cena_EUR_MWh = B) %>%
    select(datum, ura, cena_EUR_MWh) # %>%
    # select(datum, cena_EUR_MWh) %>%
    # group_by(datum) %>%
    # summarise(cena_EUR_MWh = mean(cena_EUR_MWh, na.rm=TRUE))
  hour(df$datum) <- as.integer(df$ura)
  df <- rbind(df, tmp)
}

df <- df %>% select(datum, cena_EUR_MWh)

save(df, file='../data/urne_cene_2015.RData')

load('../data/dnevne_cene_2013.RData')
df2013 <- df
load('../data/dnevne_cene_2014.RData')
df2014 <- df
load('../data/dnevne_cene_2015.RData')
df2015 <- df
rm(df)

df <- rbind(df2013, df2014, df2015)
save(df, file='../data/dnevne_cene_2013_2015.RData')
