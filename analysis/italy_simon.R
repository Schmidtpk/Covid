library(Covid)

show_ts(outcome = "pos.total/plm::lag(pos.total)",regions = NULL)+facet_wrap(vars(region))

show_ts(outcome = "pos.total",regions = NULL)+facet_wrap(vars(region))

treatments<-c("UniversityClosings", "SchoolClosings", "CurfewIMildOnlyPrivatePublicLifeI", "CurfewILockdownofAllNonEssentialPublicLifeI")

it$growth<-it$pos.total/lag(it$pos.total)

it$datenum<-as.numeric(it$Date)-18315

model.plm.pooling<-plm(paste("log(growth) ~", paste(paste0("lag(",treatments,"_active,5)"),collapse = "+")),subset(it,subset = pos.total>10) ,model = "pooling")
model.plm.iFX<-plm(paste("log(growth) ~ ", paste(paste0("lag(",treatments,"_active,5)"),collapse = "+")),subset(it,subset = pos.total>10) ,effect = "individual")
model.plm.2iFX<-plm(paste("log(growth) ~ ", paste(paste0("lag(",treatments,"_active,5)"),collapse = "+"),""),subset(it,subset = pos.total>10) ,effect = "twoway")
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
          #lmtest::coeftest(model.plm.2iFX,vcov=vcovHC(model.plm.iFX,cluster="group")),
          lmtest::coeftest(model.plm.iFXtrends,vcov=vcovHC(model.plm.iFXtrends,cluster="group")),
          lmtest::coeftest(model.plm.iFXtrends2,vcov=vcovHC(model.plm.iFXtrends2,cluster="group")),
          add.lines = list(c("Region fixed effects", "-","Yes", "Yes", "Yes")#,
#                           c("Day fixed effects", "-","-", ""-", "-")
          ))



it$R0_alt<-NA
it$pos.new<-pmax(it$pos.new,0)
for (ccc in unique(it$region)) {
  currit<-which(it$region==ccc)
  if(length(currit)<=2)
    next;
  t_start <- seq(2, length(it[currit,c("pos.new")])-1)
  t_end <- t_start + 1
  incid<-c(it[currit,c("pos.new")])
  incid[is.na(incid)]<-0
  Rres <- estimate_R(
    incid = incid,
    method = "parametric_si",
    config = make_config(list(
      t_start = t_start, t_end = t_end,
      mean_si = 5, std_si = 4
    )))
  it[currit,c("R0_alt")]<-c(NA,NA,c(Rres$R$`Mean(R)`))
}


it$tt_curfews=(it$CurfewIMildOnlyPrivatePublicLifeI_active+it$CurfewILockdownofAllNonEssentialPublicLifeI_active)>0
it$firstcurfew<-1*(it$tt_curfews!=plm::lag(it$tt_curfews))
for(i in 1:100)
  it$firstcurfew<-ifelse(it$firstcurfew<plm::lag(it$firstcurfew)&!is.na(plm::lag(it$firstcurfew)),1+plm::lag(it$firstcurfew),it$firstcurfew)
for(i in 1:100)
  it$firstcurfew<-ifelse(it$firstcurfew==plm::lead(it$firstcurfew)&!is.na(plm::lead(it$firstcurfew)),-1+plm::lead(it$firstcurfew),it$firstcurfew)
it$firstcurfewp6<-it$firstcurfew-6
ggplot(subset(it,pos.total>10&abs(firstcurfewp6)<14))+geom_smooth(aes(firstcurfewp6,R0_alt,color=region,alpha=1/sqrt(1+abs(firstcurfewp6))^0.01),size=1)+ylim(0.5,10)

it$SchoolClosings_diffp6<-it$SchoolClosings_diff-6
ggplot(subset(it,subset=pos.total>30&abs(SchoolClosings_diffp6)<14))+geom_smooth(aes(SchoolClosings_diffp6,R0_alt,color=region,alpha=1/sqrt(1+abs(SchoolClosings_diffp6))^0.01),size=1)+ylim(0.5,10)


