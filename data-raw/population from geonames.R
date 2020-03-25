colnames<-c("geonameid","name","asciiname","alternatenames","latitude","longitude","feature class","feature code","country code","cc2","admin1 code","admin2 code","admin3 code","admin4 code","population","elevation","dem","timezone","modification date")

regions<-read.csv("../../allCountries.txt",sep="\t",col.names=colnames,fill=TRUE)

regions<-regions[which(regions$feature.class=="A"),]
regions$name<-NULL
regions$alternatenames<-NULL
regions$feature.class<-NULL
regions$cc2<-NULL
regions$admin2.code<-NULL
regions$admin3.code<-NULL
regions$admin4.code<-NULL
regions$elevation<-NULL


regions<-regions[which(regions$feature.code=="ADM1"|regions$feature.code=="PCLI"),]
#type_cast
regions$latitude<-as.numeric(levels(regions$latitude))[regions$latitude]
regions$longitude<-as.numeric(levels(regions$longitude))[regions$longitude]
regions$population<-as.numeric(levels(regions$population))[regions$population]
regions$dem<-as.numeric(levels(regions$dem))[regions$dem]
regions$geonameid<-as.numeric(levels(regions$geonameid))[regions$geonameid]
regions$modification.date<-as.Date(levels(regions$modification.date),format="%Y-%m-%d")[regions$modification.date]
population<-droplevels(regions)
saveRDS(population,"data/population.RDa")
