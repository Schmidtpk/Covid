library(usethis)
colnames<-c("geonameid","name","asciiname","alternatenames","latitude","longitude","feature class","feature code","country code","cc2","admin1 code","admin2 code","admin3 code","admin4 code","population","elevation","dem","timezone","modification date")

##adm1 dataset for coding instructions
sss<-read.csv("data-raw/admin1CodesASCII.txt",sep="\t",encoding="UTF-8",col.names = c("code","name","name_ascii","id"))
adm1_codes <- sss
#use_data(adm1_codes,overwrite=TRUE)


population<-NULL
for(country in unique(substr(sss$code,1,2))) {
  # temp <- tempfile()
  # download.file(paste0("https://download.geonames.org/export/dump/",country,".zip"),temp)
  # unzip(zipfile=temp,exdir="data-raw/geonames")
  regions<-read.csv(paste0("data-raw/geonames/",country, ".txt"),col.names=colnames,sep="\t")
  regions<-regions[which(regions$feature.code=="ADM1"|regions$feature.code=="PCLI"),]
  regions<-regions[which(regions$feature.class=="A"),]
  regions$name<-NULL
  regions$alternatenames<-NULL
  regions$feature.class<-NULL
  regions$cc2<-NULL
  regions$admin2.code<-NULL
  regions$admin3.code<-NULL
  regions$admin4.code<-NULL
  regions$elevation<-NULL
  regions$modification.date<-as.Date(levels(regions$modification.date),format="%Y-%m-%d")[regions$modification.date]
  regions<-droplevels(regions)
  population<-rbind(regions,population)
}
pop<-population

#use_data(pop,overwrite = TRUE)
