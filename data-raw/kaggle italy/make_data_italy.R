df <- read.csv("./data-raw/kaggle italy/covid19_italy_region.csv")
dim(df)
ls(df)


# data cleaning -----------------------------------------------------------
df$Date <- as.Date(df$Date)
df$Country <- NULL
df <- as_tibble(df)

df <- df %>%
  rename(
    region = RegionName,
    pos.new = NewPositiveCases,
    pos.total = TotalPositiveCases,
    pos.cur = CurrentPositiveCases
  )

df$t <- rank(df$Date,ties.method = "first")

summary(df)



# save for package --------------------------------------------------------
italy <- df
use_data(italy,overwrite = TRUE)



# add lombardia -----------------------------------------------------------
library(pmdplyr)

lombardia <- italy %>% filter(region=="Lombardia")

lombardia$t <- rank(lombardia$Date)
lombardia <- as_pibble(lombardia, .t=t, .i=region)


lombardia <- lombardia %>%
  mutate(infected = pos.total - lag(pos.total),
         growth = log(infected)/log(lag(infected)))

use_data(lombardia, overwrite = TRUE)

