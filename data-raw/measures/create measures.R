library(googledrive)
#rm(list=ls())
# drive_download("https://docs.google.com/spreadsheets/d/1tYfD3vvSEaW3Cq9-UZoDtlKIfaMMVJq8XnV9XJUdK3s/edit#gid=0",
#                      path = paste0("./data-raw/measures/download_temp_",substr(date(),5,10),".csv"),overwrite = TRUE)
# measures <- read.csv( paste0("./data-raw/measures/download_temp_",substr(date(),5,10),".csv"),dec = ",")
# paste0("./data-raw/measures/download_temp_",substr(date(),5,10),".csv")

#measures <- read.csv( paste0("./data-raw/measures/download_temp_Mar 24.csv"),dec = ",")

measures <- as_tibble(read.csv(header = T,"./data-raw/measures/events.csv",
                     dec = ",",sep = ","))

# data cleaning -----------------------------------------------------------

measures$Start <- as.Date(measures$Start, format = "%d.%m.%Y")
measures$End <- as.Date(measures$End, format = "%d.%m.%Y")

# format treatments
measures$Type <- as.character(measures$Type)
measures$Type <- gsub(" ","",measures$Type)
measures$Type <- gsub("\\)","I",measures$Type)
measures$Type <- gsub("\\(","I",measures$Type)
measures$Type <- gsub(",","",measures$Type)
measures$Type <- gsub("-","",measures$Type)
measures$Type <- gsub("/","I",measures$Type)

# save in package ---------------------------------------------------------

measures$Country <- as.character(measures$Country)
use_data(measures,overwrite = TRUE)



# extract treatment data frame ----------------------------------------------------

measures$region <- ifelse(measures$ADM1=="",NA, as.character(measures$Region))
measures$adm_level <- ifelse(measures$ADM1=="",1, 2)
measures$in_country <- ifelse(measures$ADM1=="",NA, as.character(measures$Country))
measures$Name <- NULL
measures$type<-measures$Type;measures$Type<-NULL
measures$end<-measures$End;measures$End<-NULL
measures$start<-measures$Start;measures$Start<-NULL
measures$meta <- measures$Meta..e.g..group.size.thresholds
names(measures)[names(measures) == 'Affected.Pop.Share'] <- 'share'
names(measures)[names(measures) == 'Country'] <- 'country'
names(measures)[names(measures) == 'ISO.Code'] <- 'country.code'
names(measures)[names(measures) == 'ADM1'] <- 'region.code'

measures$region.code[measures$adm_level==1]<-NA
measures$region[measures$adm_level==1]<-NA


measures$name <- ifelse(measures$adm_level==1,
                        as.character(measures$country.code),
                        paste0(as.character(measures$region),as.character(measures$country.code)))




treatments <- measures %>% select(type,start,end,meta,share,
                                  region,country,
                                  region.code,country.code,
                                  name,
                                  adm_level,
                                  in_country)



#  merge with population data ---------------------------------------------

# read population data
pop <- Covid::pop
pop <- pop %>% select(asciiname,adm_level,population,country.code,region.code)



pop <- pop %>% group_by(country.code) %>%
  mutate(num_countries = sum(adm_level==1))%>%
  filter(num_countries==1)%>%
  select(-c(num_countries)) %>%
  ungroup()



# compute ratio of country for each
pop <- pop %>% group_by(country.code) %>%
  mutate(country = unique(asciiname[adm_level==1]),
         population = as.numeric(population)/10^6,
         total_population = sum(population[adm_level==1]),
         total_population2 = sum(population[adm_level!=1]),
         ratio.pop = population/total_population) %>% ungroup()


ls(treatments)
ls(pop)

pop$region.code[pop$adm_level==1]<-NA
pop <- pop %>% select(-c(adm_level, asciiname, country, total_population,total_population2))



# View(pop)
# View(treatments%>%select(region.code,country.code,country,region))

treatments <- treatments %>%
  left_join(pop,
            by=c("region.code","country.code"))

ls(treatments)
#View(treatments %>% filter(country.code=="DE"))

### check
# nrow(treatments %>% filter(is.na(ratio.pop)))
# nrow(treatments %>% filter(!is.na(ratio.pop)))
#
# View(treatments %>% filter(is.na(\ratio.pop)))
#
# View(setdiff(
#   treatments %>% select(region.code,country.code),
#   pop        %>% select(region.code,country.code)))


# recode regions ----------------------------------------------------------
level_key <- c(Bavaria="Bayern",
               #Baden-WÃ¼rttemberg
               #Saarland
               Saxony="Sachsen",
               #Berlin
               #Bremen
               #Hamburg
               Hesse="Hessen",
               'Lower Saxony' ="Niedersachsen",
               'North Rhine-Westphalia' = "Nordrhein-Westfalen",
               'Rhineland-Palatinate' = "Rheinland-Pfalz",
               'Saxony-Anhalt' = "Sachsen-Anhalt",
               #Schleswig-Holstein
               Thuringia="ThÃ¼ringen"
               )

treatments$region <- recode(treatments$region, !!!level_key)


use_data(treatments,overwrite = TRUE)

message(paste("Treatments for: ", unique(treatments$country),collapse = "\n"))

