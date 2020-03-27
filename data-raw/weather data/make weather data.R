library(httr)

vars <- c("cloud","tMax","tMin","precip","humidity","wind")
df <- NULL
for(var.cur in vars)
{

  df.cur <-as_tibble(read.csv(
    text=as.character(
      GET(
        paste0("https://raw.githubusercontent.com/imantsm/COVID-19/master/csv/",
               var.cur,
               ".csv")),
    header=T)))

  cat(var.cur,sum(grepl("Other",df.cur$Province.State)))

  #sum(grepl("ther",df.cur$Province.State)>

  df.cur.long <- df.cur %>% tidyr::pivot_longer(starts_with("X"),names_to = "date")
  df.cur.long$date <- as.Date(df.cur.long$date, "X%m.%d.%y")

  df.cur.long$var <- var.cur
  df <- rbind(df.cur.long,df)
}



weather <- df %>%
  rename(
    region = Province.State,
    country = Country.Region
  )




weather <- weather %>%
  tidyr::pivot_wider(
    names_from = "var",
    values_from = "value"
    )


#summary(weather$region)



weather$country<-as.character(weather$country)
weather$region<-as.character(weather$region)
weather$region[weather$region==""]<-NA

#use_data(weather,overwrite = T)
