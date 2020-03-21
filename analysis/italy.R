library(Covid)


ggplot(italy %>% filter(Latitude>quantile(Latitude,.65)), aes(x=Date,y=log(infected),color=region))+geom_point()+geom_line()
ggplot(italy %>% filter(Latitude>quantile(Latitude,.65)), aes(x=Date,y=pos.total,color=region))+geom_point()+geom_line()

measures <- measures %>% filter(Country=="Italy")

# format treatments
measures$Type <- as.character(measures$Type)
measures$Type <- gsub(" ","",measures$Type)
measures$Type <- gsub("\\)","I",measures$Type)
measures$Type <- gsub("\\(","I",measures$Type)
measures$Type <- gsub(",","",measures$Type)
measures$Type <- gsub("-","",measures$Type)


#safe treatments and dummies
treatments <- unique(measures$Type)
treatments <- treatments[treatments!=""]
italy[,treatments] <- NA

# merge treatments to italy data ------------------------------------------

# define interval of dummies for treatment
t.interval <- 0:20
dummies <- outer(treatments, t.interval, FUN = "paste0")[1:(length(t.interval)*length(treatments))]
italy[,dummies] <- FALSE

italy$region <- as.character(italy$region)

for (i in 1:nrow(measures))
{

  if(measures$Type[i]!="")
  {
    for(time_lag in t.interval)
    {
      # safe time from start of treatment
      italy[italy$Date == measures$Start[i]+time_lag &
              (italy$region==measures$ADM1[i] | measures$ADM1[i]==""),
            measures$Type[i]
            ] <- time_lag

      #safe dummie
      italy[italy$Date == measures$Start[i]+time_lag &
              (italy$region==measures$ADM1[i] | measures$ADM1[i]==""),
            paste0(measures$Type[i],time_lag)
            ] <- TRUE
    }
  }

}

italy$growth[is.nan(italy$growth)] <- 0
italy$growth[!is.finite(italy$growth)] <- 0

formula.cur <- paste0("growth ~ " ,paste0(grep("SchoolClosing",dummies,value=TRUE),collapse = "+"))
summary(plm(formula.cur
    ,italy,effect = "twoways"))

formula.cur <- paste0("growth ~ as.numeric(t)+as.factor(region)+" ,paste0(grep("CurfewImild",dummies,value=TRUE),collapse = "+"))
summary(plm(formula.cur
            ,italy,model="pooling"))

