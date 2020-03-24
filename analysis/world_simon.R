library(Covid)

world.measures

treatments<-c("UniversityClosings", "SchoolClosings", "CurfewIMildOnlyPrivatePublicLifeI", "CurfewILockdownofAllNonEssentialPublicLifeI")

df <- subset(world, is.na(in_country))

#drop countries without any measures in data set
df <- df[units_with(df,treatments),]

show_measures_of(df = df,units = unique(df$country))

df$growth<-df$pos.total/plm::lag(df$pos.total)
df$lagpos<-plm::lag(df$pos.total)

df$datenum<-as.numeric(df$Date)-18315

model.plm.pooling<-plm(paste("(growth) ~", paste(paste0("lag(",treatments,"_active,5)"),collapse = "+")),subset(df,subset = pos.total>10) ,model = "pooling")
model.plm.iFX<-plm(paste("(growth) ~ ", paste(paste0("lag(",treatments,"_active,5)"),collapse = "+")),subset(df,subset = pos.total>10) ,effect = "individual")
model.plm.2iFX<-plm(paste("(growth) ~ ", paste(paste0("lag(",treatments,"_active,5)"),collapse = "+"),""),subset(df,subset = pos.total>10) ,effect = "twoway")
model.plm.iFXtrends<-plm(paste("(growth) ~ ", paste(paste0("lag(",treatments,"_active,5)"),collapse = "+"),"+factor(country)*datenum"),subset(df,subset = pos.total>10) ,effect = "individual")
model.plm.iFXtrends2<-plm(paste("(growth) ~ ", paste(paste0("lag(",treatments,"_active,5)"),collapse = "+"),"+factor(country)*poly(datenum,2)"),subset(df,subset = pos.total>10) ,effect = "individual")



stargazer(type="text",
          omit=c("datenum$","poly\\(datenum, 2\\)"),
          omit.labels=c("Country-specific time trends","Country-specific time trends (squared)"),omit.yes.no = c("Yes","-"),
          lmtest::coeftest(model.plm.pooling,vcov=vcovHC(model.plm.pooling,cluster="group")),
          lmtest::coeftest(model.plm.iFX,vcov=vcovHC(model.plm.iFX,cluster="group")),
          lmtest::coeftest(model.plm.2iFX,vcov=vcovHC(model.plm.2iFX,cluster="group")),
          lmtest::coeftest(model.plm.iFXtrends,vcov=vcovHC(model.plm.iFXtrends,cluster="group")),
          lmtest::coeftest(model.plm.iFXtrends2,vcov=vcovHC(model.plm.iFXtrends2,cluster="group")),
          add.lines = list(c("Country fixed effects", "-","Yes", "Yes","Yes", "Yes"),
                           c("Day fixed effects", "-","-", "Yes","-", "-")
          ))

