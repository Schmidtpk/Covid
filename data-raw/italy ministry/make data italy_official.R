library(httr)
library(lubridate)
library(dplyr)



vars <- seq(as.Date("2020-02-24"),Sys.Date(),"days")
df <- NULL
for(var.cur in 1:length(vars))
{

  var.cur <- vars[var.cur]
  df.cur <-as_tibble(read.csv(
    text=as.character(
      GET(
 paste0(
   "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni-",
   year(var.cur),format(var.cur,"%m"),format(var.cur,"%d"),
   ".csv")
 )),
    header=T))

  #skif if no data exists
  if(nrow(df.cur)==0)
    {warning(paste0("No data for date ",var.cur))
    next()
  }


  #rename columns as not consistent in data
  colnames(df.cur)[grepl("denominazione", colnames(df.cur))] <- "region"
  colnames(df.cur)[grepl("stato", colnames(df.cur))] <- "country.code"


  df.cur$date <- var.cur
  df.cur$country <- "Italy"
  df <- plyr::rbind.fill(df.cur,df)
}
italy_o <- as_tibble(df)

ls(italy_o)
italy_o <- italy_o %>%
  rename(
    region.code = codice_regione,
    tests = tamponi,
    pos.total = totale_casi,
    dead = deceduti,
    quarantine = totale_positivi,
    recovered = dimessi_guariti,
    hospital = totale_ospedalizzati,
    hopsital_with_symptoms = ricoverati_con_sintomi,
    home = isolamento_domiciliare,
    new.hostpial_or_home = variazione_totale_positivi
  )


italy_o <- italy_o %>% select(-c(data,note_en,note_it))

italy_o$region <- as.character(italy_o$region)
italy_o$country <- as.character(italy_o$country)


#use_data(italy_o,overwrite = T)
