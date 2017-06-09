library(readODS)
library(dplyr)
library(lubridate)

# setwd("~/Documents/Faks/Matematika_z_racunalnikom/src")
setwd("~/Documents/Faks/Matematika_z_racunalnikom/markov_switching_model/src")

filePath <- '../data/Cp_in_Cn_2014_SL.ods'
n <- get_num_sheet_in_ods(filePath)
start_sheet = 1

df <- read_ods(filePath, start_sheet, skip=9, col_names=F)
# df_colnames <- c("datum", "cena_EUR_MWh")
df <- df %>%
  mutate(datum = dmy(substr(A, 1, 10))) %>%
  # mutate(ura = substr(A, 12, 13)) %>%
  rename(cena_EUR_MWh = B) %>%
  # select(datum, ura, cena_EUR_MWh) # %>%
  select(datum, cena_EUR_MWh) %>%
  group_by(datum) %>%
  summarise(cena_EUR_MWh = mean(cena_EUR_MWh, na.rm=TRUE))
  
# hour(df$datum) <- as.integer(df$ura)

# This is a specific parse for sheet 2 in 2014 data
tmp <- read_ods(filePath, 2, skip=9, col_names=F)
tmp$A <- gsub('Feb', '2', tmp$A)
tmp$A <- gsub( " .*$", "", tmp$A)
tmp <- tmp %>%
  mutate(datum = dmy(A)) %>%
  rename(cena_EUR_MWh = B) %>%
  select(datum, cena_EUR_MWh) %>%
  group_by(datum) %>%
  summarise(cena_EUR_MWh = mean(cena_EUR_MWh, na.rm=TRUE))


for (i in (start_sheet+2):n){
  tmp <- read_ods(filePath, i, skip=9, col_names=F)
  tmp <- tmp %>%
    mutate(datum = dmy(substr(A, 1, 10))) %>%
    # mutate(ura = substr(A, 12, 13)) %>%
    rename(cena_EUR_MWh = B) %>%
    # select(datum, ura, cena_EUR_MWh) # %>%
    select(datum, cena_EUR_MWh) %>%
    group_by(datum) %>%
    summarise(cena_EUR_MWh = mean(cena_EUR_MWh, na.rm=TRUE))
  # hour(df$datum) <- as.integer(df$ura)
  # print(i)
  # print(dim(tmp))
  df <- rbind(df, tmp)
}

df <- df %>% select(datum, cena_EUR_MWh)

save(df, file='../data/dnevne_cene_2014.RData')

load('../data/dnevne_cene_2013.RData')
df2013 <- df
load('../data/dnevne_cene_2014.RData')
df2014 <- df
load('../data/dnevne_cene_2015.RData')
df2015 <- df
rm(df)

df <- rbind(df2013, df2014, df2015)
save(df, file='../data/dnevne_cene_2013_2015.RData')
