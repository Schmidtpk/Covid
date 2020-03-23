library(Covid)

world.measures

treatments<-c("UniversityClosings", "SchoolClosings", "CurfewIMildOnlyPrivatePublicLifeI", "CurfewILockdownofAllNonEssentialPublicLifeI")

df <- subset(world, is.na(in_country))


#drop countries without any measures in data set
df <- keep_units_with(df,treatments)

show_measures_of(df = df,units = unique(df$country))
g

df$growth<-df$pos.total/lag(df$pos.total)
#df <- subset(df,!is.na(growth) & is.finite(growth))

model.plm.iFX<-plm("growth ~ dplyr::between(SchoolClosings_diff,4,6)",
                   subset(df,subset = pos.total>10),
                   model = "pooling")
summary(model.plm.iFX)



model.plm.iFX<-plm(paste("growth ~ ", paste(paste0("lag(",treatments[2],"_active,5)"),collapse = "+")),
                   subset(df,subset = pos.total>10) ,
                   model = "pooling")
summary(model.plm.iFX)

model.plm.iFX<-plm(paste("growth ~ ", paste(paste0("lag(",treatments,"_active,5)"),collapse = "+")),
                   subset(df,subset = pos.total>10) ,
                   model = "pooling")
summary(model.plm.iFX)

