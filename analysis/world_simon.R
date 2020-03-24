library(Covid)

world.measures

treatments<-c("UniversityClosings", "SchoolClosings", "CurfewIMildOnlyPrivatePublicLifeI", "CurfewILockdownofAllNonEssentialPublicLifeI")

df <- subset(world, is.na(in_country))


#drop countries without any measures in data set
df <- keep_units_with(df,treatments)

show_measures_of(df = df,units = unique(df$country))

df$growth<-df$pos.total/lag(df$pos.total)


df$CurfewIMildOnlyPrivatePublicLifeI_active<-(df$CurfewIMildOnlyPrivatePublicLifeI_active+df$CurfewILockdownofAllNonEssentialPublicLifeI_active)>0



model.plm.pooling<-plm(paste("growth ~", paste(paste0("lag(",treatments,"_active,5)"),collapse = "+")),subset(df,subset = pos.total>10) ,model = "pooling")
model.plm.iFX<-plm(paste("growth ~ ", paste(paste0("lag(",treatments,"_active,5)"),collapse = "+")),subset(df,subset = pos.total>10) ,effect = "individual")
model.plm.iFXtrends<-plm(paste("growth ~ ", paste(paste0("lag(",treatments,"_active,5)"),collapse = "+"),"+factor(country)*Date"),subset(df,subset = pos.total>10) ,effect = "individual")



stargazer(type="text",
          omit=c("Date$"),
          omit.labels=c("Country-specific time trends"),
          lmtest::coeftest(model.plm.pooling,vcov=vcovHC(model.plm.pooling,cluster="group")),
          lmtest::coeftest(model.plm.iFX,vcov=vcovHC(model.plm.iFX,cluster="group")),
          lmtest::coeftest(model.plm.iFXtrends,vcov=vcovHC(model.plm.iFXtrends,cluster="group")),
          add.lines = list(c("Country fixed effects", "No","Yes", "Yes")))
