library(usethis)
colnames<-c("geonameid","name","asciiname","alternatenames","latitude","longitude","feature class","feature code","country code","cc2","admin1 code","admin2 code","admin3 code","admin4 code","population","elevation","dem","timezone","modification date")

##adm1 dataset for coding instructions
sss<-read.csv("data-raw/admin1CodesASCII.txt",sep="\t",encoding="UTF-8",col.names = c("code","name","name_ascii","id"))
adm1_codes <- sss
#use_data(adm1_codes,overwrite=TRUE)


population<-NULL
for(country in unique(substr(sss$code,1,2))) {
  # if(country == "DE")
  #   browser()

  # temp <- tempfile()
  # download.file(paste0("https://download.geonames.org/export/dump/",country,".zip"),temp)
  # unzip(zipfile=temp,exdir="data-raw/geonames")
  regions<-read.csv(paste0("data-raw/geonames/",country, ".txt"),col.names=colnames,sep="\t")

  if(country != "DE")
  {
    regions<-regions[regions$feature.code=="ADM1"|regions$feature.code=="PCLI",]
    regions<-regions[regions$feature.class=="A",]
  } else {
    regions<-regions[regions$feature.code %in% c("ADM1","PCLI","ADM3"),]
    regions<-regions[regions$feature.class=="A",]
  }

  regions$name<-NULL
  regions$alternatenames<-NULL
  regions$feature.class<-NULL
  regions$cc2<-NULL
  regions$admin2.code<-NULL
  regions$admin3.code<-NULL
  regions$admin4.code<-NULL
  regions$elevation<-NULL
  regions$modification.date<-NULL#as.Date(levels(regions$modification.date),format="%Y-%m-%d")[regions$modification.date]
  regions<-droplevels(regions)
  regions$latitude <- as.character(regions$latitude)
  regions$longitude <- as.character(regions$longitude)
  regions$geonameid <- as.character(regions$geonameid)
  regions$population <- as.numeric(regions$population)

  population<-rbind(regions,population)

  if(mean(is.na(population$latitude))>0)
    browser()

}
pop<-population



 pop <- pop %>% rename(
    "region.code" = "admin1.code",
    "adm_level" = "feature.code"
  )

#adapt levels of "is country" dummy
pop$adm_level <- ifelse(pop$adm_level=="PCLI",
                        1, ifelse(pop$adm_level=="ADM1", 2, 3))

#drop NAs
pop <- pop %>% filter(!is.na(country.code))

pop$region.code[pop$adm_level==1]<-NA

popall <- pop

use_data(popall, overwrite = TRUE)
