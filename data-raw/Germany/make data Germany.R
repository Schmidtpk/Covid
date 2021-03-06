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
    "https://raw.githubusercontent.com/jgehrcke/covid-19-germany-gae/master/data.csv"
  ))))
df



df <- df%>%rename(
  date = time_iso8601
)

df$date <- as.Date(df$date)

#each timing only elicited once
length(unique(df$date))
dim(df)

df <- df %>% select(-c(sum_cases,sum_deaths,source))

ls(df)
df$sum_cases<-NULL



df <- df %>% pivot_longer(-c(date),names_sep = "_",names_to = c("name","name2"))

df <- df %>% pivot_wider(names_from="name2",values_from = "value", values_fn = list(value=mean))


df$country <- "Germany"
df$region.code <- substr(df$name,4,5)
df$name <- NULL

df <- df %>% rename(
  dead=deaths,
  positive=cases
)

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

df <- df %>% rename(region=Name)

df <- df %>% select(date,positive, dead,region, country)

#rename regions for matching
df$region[df$region=="Bayern"]<-"Bavaria"
df$region[df$region=="Rheinland-Pfalz"]<-"North Rhine-Westphalia"
df$region[df$region=="Baden-Württemberg"]<-"Baden-WÃ¼rttemberg"

Ger <- df
#use_data(Ger,overwrite = T)
message(paste0("range of dates for Germany until:", range(df$date)[2]))
