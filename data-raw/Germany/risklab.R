library(tidyr)
library(httr)
library(dplyr)

#link: risklab
#https://raw.githubusercontent.com/jgehrcke/covid-19-germany-gae/master/cases-rl-crowdsource-by-state.csv
# kreis also exists

# link combination
#"https://raw.githubusercontent.com/jgehrcke/covid-19-germany-gae/master/data.csv"

#risklab cases
df <- as_tibble(read.csv(text=as.character(
  GET(
    "https://raw.githubusercontent.com/jgehrcke/covid-19-germany-gae/master/cases-rl-crowdsource-by-state.csv"
  ))))
df



df <- df%>%rename(
  date = time_iso8601
)

df$date <- as.Date(df$date)

#each timing only elicited once
length(unique(df$date))
dim(df)

df <- df %>% select(-c(sum_cases))

ls(df)
df$sum_cases<-NULL



df <- df %>% pivot_longer(-c(date),values_to = "positive")

df$country <- "Germany"
df$region.code <- substr(df$name,4,5)
df$name <- NULL


# if two numbers available take larger (assume more updated)
df <- df %>% group_by(region.code,date) %>%
  mutate(
    n= n(),
    rank_temp = rank(desc(positive))
  ) %>% filter(rank_temp==1) %>% select(-c(n,rank_temp)) %>% ungroup()

#add names to codes
library(ISOcodes)
codes <- as_tibble(ISO_3166_2)%>%filter(substr(Code,1,2)=="DE")
codes$Code <- substr(codes$Code,4,5)

df <- df %>% left_join(codes,by=c("region.code"="Code"))

df <- df %>% rename(
  region=Name
)

df <- df %>% select(date,positive, region, country)

risklab <- df
#use_data(risklab,overwrite = T)
