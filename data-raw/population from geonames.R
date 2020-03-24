colnames<-c("geonameid","name","asciiname","alternatenames","latitude","longitude","feature class","feature code","country code","cc2","admin1 code","admin2 code","admin3 code","admin4 code","population","elevation","dem","timezone","modification date")

regions<-read.csv("../../../allCountries.txt",sep="\t",col.names=colnames,fill=TRUE)

regions<-regions[which(ad$feature.class=="A"),]
regions$name<-NULL
regions$alternatenames<-NULL
regions$feature.class<-NULL
regions$cc2<-NULL
regions$admin2.code<-NULL
regions$admin3.code<-NULL
regions$admin4.code<-NULL
regions$elevation<-NULL
regions2<-regions[which(regions$feature.code=="ADM1"|regions$feature.code=="PCLI"),]
