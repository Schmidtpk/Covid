library(httr)
library(dplyr)

df <-as_tibble(read.csv(
  text=as.character(
    GET(
      "https://raw.githubusercontent.com/daenuprobst/covid19-cases-switzerland/master/covid_19_cases_switzerland_standard_format.csv"
    )),
  header=T))

switzerland <- df

#use_data(switzerland,overwrite = T)
