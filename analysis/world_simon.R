library(Covid)
library(EpiEstim)

world.measures

treatments<-c("SchoolClosings","CurfewIMildOnlyPrivatePublicLife")
# subset only countries (no regions)
df <- subset(Covid::all, adm_level==1)
df$`share_RestaurantBarClosure`<-df$`share_Restaurant/BarClosure`


df$pos.total<-df$pos.new
for(i in 1:100)
  df$pos.total<-df$pos.new+ifelse(is.na(plm::lag(df$pos.total)),0,plm::lag(df$pos.total))

df$growth<-df$pos.total/plm::lag(df$pos.total)
df$growth[which(df$pos.total<10)]<-NA

df$R0_alt<-NA
for (ccc in unique(df$country)) {
  currdf<-which(df$country==ccc)
  if(length(currdf)<=2)
    next;
  t_start <- seq(2, length(df[currdf,c("pos.new")])-1)
  t_end <- t_start + 1
  incid<-c(df[currdf,c("pos.new")])
  incid[is.na(incid)]<-0
  Rres <- estimate_R(
    incid = incid,
    method = "parametric_si",
    config = make_config(list(
      t_start = t_start, t_end = t_end,
      mean_si = 5, std_si = 3
    )))
  df[currdf,c("R0_alt")]<-c(NA,NA,c(Rres$R$`Mean(R)`))
}


model.plm.pool<-plm(paste("log(growth) ~ ", paste(paste0("plm::lag(share_",treatments,",6)"),collapse = "+")),subset(df,subset = pos.total>10) ,model = "pooling")
model.plm.iFX<-plm(paste("log(growth) ~ ", paste(paste0("plm::lag(share_",treatments,",6)"),collapse = "+")),subset(df,subset = pos.total>10) ,effect = "individual")
model.plm.iFXtrends<-plm(paste("log(growth) ~ ", paste(paste0("plm::lag(share_",treatments,",6)"),collapse = "+"),"+factor(country)*date"),subset(df,subset = pos.total>10) ,effect = "individual")
model.plm.iFXtrends2<-plm(paste("log(growth) ~ ", paste(paste0("plm::lag(share_",treatments,",6)"),collapse = "+"),"+factor(country)*poly(date,2)"),subset(df,subset = pos.total>10) ,effect = "individual")

stargazer(type="text",
          omit=c("date\\d*$","poly\\(date"),
          omit.labels=c("Country-specific time trend","Country-specific squared time trend"),omit.yes.no = c("Yes","-"),
          lmtest::coeftest(model.plm.iFX,vcov=vcovHC(model.plm.iFX,cluster="group")),
          lmtest::coeftest(model.plm.iFXtrends,vcov=vcovHC(model.plm.iFXtrends,cluster="group")),
          lmtest::coeftest(model.plm.iFXtrends2,vcov=vcovHC(model.plm.iFXtrends2,cluster="group"))
          )


model.plm.pool<-plm(paste("R0_alt ~ ", paste(paste0("plm::lag(share_",treatments,",6)"),collapse = "+")),subset(df,subset = pos.total>10) ,model = "pooling")
model.plm.iFX<-plm(paste("R0_alt ~ ", paste(paste0("plm::lag(share_",treatments,",6)"),collapse = "+")),subset(df,subset = pos.total>10) ,effect = "individual")
model.plm.iFXtrends<-plm(paste("R0_alt ~ ", paste(paste0("plm::lag(share_",treatments,",6)"),collapse = "+"),"+factor(country)*date"),subset(df,subset = pos.total>10) ,effect = "individual")
model.plm.iFXtrends2<-plm(paste("R0_alt ~ ", paste(paste0("plm::lag(share_",treatments,",6)"),collapse = "+"),"+factor(country)*poly(date,2)"),subset(df,subset = pos.total>10) ,effect = "individual")

stargazer(type="text",
          omit=c("date\\d*$","poly\\(date"),
          omit.labels=c("Country-specific time trend","Country-specific squared time trend"),omit.yes.no = c("Yes","-"),
          lmtest::coeftest(model.plm.iFX,vcov=vcovHC(model.plm.iFX,cluster="group")),
          lmtest::coeftest(model.plm.iFXtrends,vcov=vcovHC(model.plm.iFXtrends,cluster="group")),
          lmtest::coeftest(model.plm.iFXtrends2,vcov=vcovHC(model.plm.iFXtrends2,cluster="group"))
          )
df$firstschoolclosing<-1*(df$tt_SchoolClosings!=plm::lag(df$tt_SchoolClosings))
for(i in 1:100)
  df$firstschoolclosing<-ifelse(df$firstschoolclosing<plm::lag(df$firstschoolclosing)&!is.na(plm::lag(df$firstschoolclosing)),1+plm::lag(df$firstschoolclosing),df$firstschoolclosing)
for(i in 1:100)
  df$firstschoolclosing<-ifelse(df$firstschoolclosing==plm::lead(df$firstschoolclosing)&!is.na(plm::lead(df$firstschoolclosing)),-1+plm::lead(df$firstschoolclosing),df$firstschoolclosing)
df$firstschoolclosingp6<-df$firstschoolclosing-6
ggplot(subset(df,subset=name%in%c("Belgium", "Italy", "Japan", "Netherlands", "Switzerland")&pos.total>30&abs(firstschoolclosingp6)<14))+geom_line(aes(firstschoolclosingp6,growth,color=name,alpha=1/sqrt(1+abs(firstschoolclosingp6))^0.01),size=1)+ylim(1,1.5)


df$tt_curfews=(df$tt_CurfewILockdownofAllNonEssentialPublicLife+df$tt_CurfewIMildOnlyPrivatePublicLife)>0
df$firstcurfew<-1*(df$tt_curfews!=plm::lag(df$tt_curfews))
for(i in 1:100)
  df$firstcurfew<-ifelse(df$firstcurfew<plm::lag(df$firstcurfew)&!is.na(plm::lag(df$firstcurfew)),1+plm::lag(df$firstcurfew),df$firstcurfew)
for(i in 1:100)
  df$firstcurfew<-ifelse(df$firstcurfew==plm::lead(df$firstcurfew)&!is.na(plm::lead(df$firstcurfew)),-1+plm::lead(df$firstcurfew),df$firstcurfew)
df$firstcurfewp6<-df$firstcurfew-6
ggplot(subset(df,pos.total>30&abs(firstcurfewp6)<14))+geom_line(aes(firstcurfewp6,growth,color=name,alpha=1/sqrt(1+abs(firstcurfewp6))^0.01),size=1)+ylim(1,1.5)
