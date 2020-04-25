library(httr)
library(dplyr)

# measures <- as_tibble(read.csv(
#   text=as.character(
#     GET(
#       paste0("https://www.iddynamics.jhsph.edu/apps/connect/hit-covid/_w_9327cb8c/session/8b05f03723cb2c8b799049abc3025cb5/download/download_data?w=9327cb8c")
#     )),
#   header=T))

measures <-read.csv("./data-raw/measures/hit-2020-04-20.csv")
# data cleaning -----------------------------------------------------------

measures$start <- as.Date(measures%>%pull(date_of_update), format = "%Y-%m-%d")

measures <- measures %>% rename(
  type = intervention_specific_clean,
  country.code = country,
  country = country_name,
  region = admin1_name
)

list_treatment <- unique(measures$type)
list_treatment[grep("school", list_treatment)]

ls(measures)

measures$duration

measures<-measures%>%filter(required=="required",
                            subpopulation=="entire population",
                            status_simp=="Strongly Implemented")

#View(measures%>%filter(country=="Italy"))

hit <- measures
use_data(hit,overwrite = TRUE)


#measures %>% filter(national_entry!="Yes",is.na(region)) %>% View

# extract treatment data frame ----------------------------------------------------
measures$end <- NA

treatments <- measures %>% select(type,start,end,
                                  region,country,
                                  country.code)



#  merge with population data ---------------------------------------------
#todo
treatments$population <- NA
treatments$ratio.pop <- NA

treatments_hit <- treatments

unique(treatments_hit$type)

use_data(treatments_hit,overwrite = TRUE)
