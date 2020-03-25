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

# save in package ---------------------------------------------------------

measures$Country <- as.character(measures$Country)
use_data(measures,overwrite = TRUE)



# extract treatment df ----------------------------------------------------

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
measures$name <- paste0(as.character(measures$ADM1)," in ",as.character(measures$country.code))


treatments <- measures %>% select(type,name,adm_level,in_country,start,end,meta,share,country,ADM1,country.code,region)

treatments$ADM1 <- ifelse(treatments$adm_level==1,
                          as.character(treatments$country.code),
                          as.character(treatments$ADM1))

#  merge with population data ---------------------------------------------

# read population data
pop <- Covid::pop
pop <- pop %>% select(asciiname,admin1.code,population,country.code,feature.code) %>%
  rename(
    "ADM1" = "admin1.code",
    "adm_level" = "feature.code"
  )

#adapt levels of "is country" dummy
pop$adm_level <- ifelse(pop$adm_level=="PCLI",1,2)
pop$ADM1 <- ifelse(pop$adm_level==1,
                   as.character(pop$country.code),
                   as.character(pop$ADM1)
                   )
#View(pop %>% filter(adm_level==1))

#drop NAs
pop <- pop %>% filter(!is.na(country.code))

pop <- pop %>% group_by(country.code) %>%
  mutate(num_countries = sum(adm_level==1))%>%
  filter(num_countries==1)%>%
  select(-c(num_countries)) %>%
  ungroup()



# compute ratio of country for each
pop <- pop %>% group_by(country.code) %>%
  mutate(country = unique(asciiname[adm_level==1]),
         total_population = sum(population[adm_level==1]),
         total_population2 = sum(population[adm_level!=1]),
         ratio.pop = population/total_population) %>% ungroup()


### join population
# treatments$ADM1 <- as.character(treatments$ADM1)
# pop$country <- as.character(pop$country)
# pop$country.code <- as.character(pop$country.code)
# treatments$country.code <- as.character(treatments$country.code)

ls(treatments)
ls(pop)

pop <- pop %>% select(-c(adm_level, asciiname, country, total_population,total_population2))

treatments <- treatments %>% left_join(pop, by=c("ADM1","country.code"))

ls(treatments)
#View(treatments %>% filter(country.code=="DE"))

### check
# nrow(treatments %>% filter(is.na(ratio.pop)))
# nrow(treatments %>% filter(!is.na(ratio.pop)))
#
# View(treatments %>% filter(is.na(ratio.pop)))
#
# View(setdiff(
#   treatments %>% select(ADM1,country.code),
#   pop        %>% select(ADM1,country.code)))


use_data(treatments,overwrite = TRUE)
