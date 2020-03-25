library(googledrive)

# drive_download("https://docs.google.com/spreadsheets/d/1tYfD3vvSEaW3Cq9-UZoDtlKIfaMMVJq8XnV9XJUdK3s/edit#gid=0",
#                      path = paste0("./data-raw/measures/download_temp_",substr(date(),5,10),".csv"),overwrite = TRUE)


measures <- read.csv( paste0("./data-raw/measures/download_temp_",substr(date(),5,10),".csv"),dec = ",")

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


measures$name <- ifelse(measures$ADM1=="",measures$Country, measures$Region)
measures$adm_level <- ifelse(measures$ADM1=="",1, 2)
measures$in_country <- ifelse(measures$ADM1=="",NA, measures$Country)
measures$Name <- NULL
measures$type<-measures$Type;measures$Type<-NULL
measures$end<-measures$End;measures$End<-NULL
measures$start<-measures$Start;measures$Start<-NULL
measures$meta <- measures$Meta..e.g..group.size.thresholds
names(measures)[names(measures) == 'Affected.Pop.Share'] <- 'share'

treatments <- measures %>% select(type,name,adm_level,in_country,start,end,meta,share)
use_data(treatments,overwrite = TRUE)
