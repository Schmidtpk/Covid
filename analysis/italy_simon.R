library(Covid)

show_ts(outcome = "pos.total/plm::lag(pos.total)",regions = NULL)+facet_wrap(vars(region))

show_ts(outcome = "pos.total",regions = NULL)+facet_wrap(vars(region))

treatments<-c("UniversityClosings", "SchoolClosings", "CurfewIMildOnlyPrivatePublicLifeI", "CurfewILockdownofAllNonEssentialPublicLifeI")

it$growth<-it$pos.total/lag(it$pos.total)

it$datenum<-as.numeric(it$Date)-18315

model.plm.pooling<-plm(paste("log(growth) ~", paste(paste0("lag(",treatments,"_active,5)"),collapse = "+")),subset(it,subset = pos.total>10) ,model = "pooling")
model.plm.iFX<-plm(paste("log(growth) ~ ", paste(paste0("lag(",treatments,"_active,5)"),collapse = "+")),subset(it,subset = pos.total>10) ,effect = "individual")
#model.plm.2iFX<-plm(paste("log(growth) ~ ", paste(paste0("lag(",treatments,"_active,5)"),collapse = "+"),""),subset(it,subset = pos.total>10) ,effect = "twoway")
model.plm.iFXtrends<-plm(paste("log(growth) ~ ", paste(paste0("lag(",treatments,"_active,5)"),collapse = "+"),"+factor(RegionCode)*datenum"),subset(it,subset = pos.total>10) ,effect = "individual")
model.plm.iFXtrends2<-plm(paste("log(growth) ~ ", paste(paste0("lag(",treatments,"_active,5)"),collapse = "+"),"+factor(RegionCode)*poly(datenum,2)"),subset(it,subset = pos.total>10) ,effect = "individual")

#rob_se <- list(sqrt(diag(vcovHC(model.plm.iFX, type = "HC1",cluster="group"))),
 #              sqrt(diag(vcovHC(model.plm.pooling, type = "HC1",cluster="group"))),
  #             sqrt(diag(vcovHC(model.plm.iFXtrends, type = "HC1",cluster="group"))))


stargazer(type="text",
          omit=c("datenum$","poly\\(datenum, 2\\)"),
          omit.labels=c("Region-specific time trends","Region-specific time trends (squared)"),omit.yes.no = c("Yes","-"),
          lmtest::coeftest(model.plm.pooling,vcov=vcovHC(model.plm.pooling,cluster="group")),
          lmtest::coeftest(model.plm.iFX,vcov=vcovHC(model.plm.iFX,cluster="group")),
#          lmtest::coeftest(model.plm.2iFX,vcov=vcovHC(model.plm.2iFX,cluster="group")),
          lmtest::coeftest(model.plm.iFXtrends,vcov=vcovHC(model.plm.iFXtrends,cluster="group")),
          lmtest::coeftest(model.plm.iFXtrends2,vcov=vcovHC(model.plm.iFXtrends2,cluster="group")),
          add.lines = list(c("Region fixed effects", "-","Yes", "Yes", "Yes")#,
#                           c("Day fixed effects", "-","-", ""-", "-")
          ))

