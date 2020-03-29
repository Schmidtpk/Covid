library(tidyr)
library(httr)
library(dplyr)

#link: risklab
#https://raw.githubusercontent.com/jgehrcke/covid-19-germany-gae/master/cases-rl-crowdsource-by-state.csv
# kreis also exists

# link combination
#"https://raw.githubusercontent.com/jgehrcke/covid-19-germany-gae/master/data.csv"

df <- as_tibble(read.csv(text=as.character(
  GET(
    "https://raw.githubusercontent.com/jgehrcke/covid-19-germany-gae/master/cases-rl-crowdsource-by-state.csv"
  ))))
df


df <- df%>%rename(
  date = time_iso8601
)

#each timing only elicited once
length(unique(df$date))
dim(df)

df <- df %>% select(-c(sum_cases))

ls(df)
df$sum_cases<-NULL



df <- df %>% pivot_longer(-c(date),values_to = "positive")

df$country <- "Germany"
df$region <- substr(df$name,4,5)
df$name <- NULL

risklab <- df
use_data(risklab,overwrite = T)
