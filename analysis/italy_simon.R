library(Covid)

show_ts(outcome = "pos.total/lag(pos.total)",regions = NULL)+facet_wrap(vars(region))

show_ts(outcome = "pos.total",regions = NULL)+facet_wrap(vars(region))

treatments<-c("UniversityClosings", "SchoolClosings", "CurfewImildonlyprivatepubliclifeI", "CurfewILockdownofAllNonEssentialPublicLifeI")

it$growth<-it$pos.total/lag(it$pos.total)
#strict curfew implies mild curfew
it$CurfewImildonlyprivatepubliclifeI_active<-it$CurfewILockdownofAllNonEssentialPublicLifeI_active+it$CurfewImildonlyprivatepubliclifeI_active>0


model.plm.pooling<-plm(paste("growth ~", paste(paste0("lag(",treatments,"_active,5)"),collapse = "+")),subset(it,subset = pos.total>10) ,model = "pooling")
model.plm.iFX<-plm(paste("growth ~ ", paste(paste0("lag(",treatments,"_active,5)"),collapse = "+")),subset(it,subset = pos.total>10) ,effect = "individual")
model.plm.iFXtrends<-plm(paste("growth ~ ", paste(paste0("lag(",treatments,"_active,5)"),collapse = "+"),"+factor(RegionCode)*Date"),subset(it,subset = pos.total>10) ,effect = "individual")

rob_se <- list(sqrt(diag(vcovHC(model.plm.iFX, type = "HC1",cluster="group"))),
               sqrt(diag(vcovHC(model.plm.pooling, type = "HC1",cluster="group"))),
               sqrt(diag(vcovHC(model.plm.iFXtrends, type = "HC1",cluster="group"))))

stargazer(model.plm.pooling,
          model.plm.iFX,
          model.plm.iFXtrends,
          se=rob_se,type="text",omit=c("Date$", "^Constant$"),
          add.lines = list(c("Region fixed effects", "No","Yes", "Yes"),
                           c("Region-specific time trends","No", "No", "Yes")))
