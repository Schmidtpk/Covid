library(Covid)

show_ts(outcome = "pos.total/lag(pos.total)",regions = NULL)+facet_wrap(vars(region))

show_ts(outcome = "pos.total",regions = NULL)+facet_wrap(vars(region))

treatments<-c("UniversityClosings", "SchoolClosings", "CurfewImildonlyprivatepubliclifeI", "CurfewILockdownofAllNonEssentialPublicLifeI")

it$growth<-it$pos.total/lag(it$pos.total)
#strict curfew implies mild curfew
it$CurfewImildonlyprivatepubliclifeI_active<-it$CurfewILockdownofAllNonEssentialPublicLifeI_active+it$CurfewImildonlyprivatepubliclifeI_active>0


model.plm.iFX<-plm(paste("growth ~ ", paste(paste0("lag(",treatments,"_active,5)"),collapse = "+")),subset(it,subset = pos.total>10) ,effect = "individual")
summary(model.plm.iFX)

model.plm.pooling<-plm(paste("growth ~", paste(paste0("lag(",treatments,"_active,5)"),collapse = "+")),subset(it,subset = pos.total>10) ,model = "pooling")
summary(model.plm.pooling)



summary(model.plm.iFX,
        vcov=vcovHC(model.plm.iFX,cluster="group"))
summary(model.plm.pooling,
        vcov=vcovHC(model.plm.pooling,cluster="group"))



