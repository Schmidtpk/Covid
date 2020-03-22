df <- read.csv("./data-raw/kaggle italy/covid19_italy_region.csv")
dim(df)
ls(df)
library(tidyverse)
library(plm)

# data cleaning -----------------------------------------------------------

# date
df$Date <- as.Date(df$Date)

# unneccesary variables
df$Country <- NULL

# rename variables
df <- as_tibble(df)

df <- df %>%
  rename(
    region = RegionName,
    pos.new = NewPositiveCases,
    pos.total = TotalPositiveCases,
    pos.cur = CurrentPositiveCases,
  )

# make panel data frame pdata.frame
df$t <- df$Date-min(df$Date)
df <- pdata.frame(df,index = c("region","t"))


# add time dummies
# t <- factor(df$Date)
# dummies = model.matrix(~t)

# save for package --------------------------------------------------------
italy <- df
use_data(italy,overwrite = TRUE)

# save regions for codebook
codes_region <- as.matrix(unique(italy$region),ncol=1)
write.csv(codes_region, "./data-raw/kaggle italy/codes_region_italy.csv")



