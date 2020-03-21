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
