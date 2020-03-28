library(httr)
library(lubridate)
library(dplyr)

vars <- seq(as.Date("2020-01-22"),Sys.Date(),"days")
df <- NULL
for(var.cur in 1:length(vars))
{

  var.cur <- vars[var.cur]
  df.cur <-as_tibble(read.csv(
    text=as.character(
      GET(
 paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/",
              format(var.cur,"%m"),"-",format(var.cur,"%d"),"-",year(var.cur),
              ".csv")
        )),
    header=T))

  #skif if no data exists
  if(nrow(df.cur)==0)
    {warning(paste0("No data for date ",var.cur))
    next()
  }

  #rename columns as not consistent in data
  colnames(df.cur)[grepl("rovince", colnames(df.cur))] <- "region"
  colnames(df.cur)[grepl("ountry", colnames(df.cur))] <- "country"

  df.cur$date <- var.cur
  df <- plyr::rbind.fill(df.cur,df)
}
saveRDS(df,"./data-raw/JHU/download_jhu.RDS")
df <- readRDS("./data-raw/JHU/download_jhu.RDS")

jhu <- as_tibble(df)

ls(jhu)
jhu <- jhu %>%
  rename(
    current = Active,
    positive = Confirmed,
    dead = Deaths,
    recovered = Recovered
  )


jhu_all<-jhu
#use_data(jhu_all,overwrite = T)


# drop all excpet country/region observations
jhu <- jhu %>% filter(is.na(Admin2)|(Admin2==""))

# drop all but states
jhu <- jhu%>%filter((country=="US" & (region %in%state.name)) |
                      country!="US")


jhu <- jhu %>% select(country,region, date,
                      current,positive,dead,recovered)

jhu$region <- as.character(jhu$region)
jhu$country <- as.character(jhu$country)

jhu$region[jhu$region=="00"|jhu$region=="0"|jhu$region==""]<-NA

#use_data(jhu,overwrite = T)
