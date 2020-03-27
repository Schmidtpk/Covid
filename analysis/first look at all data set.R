rm(list=ls())
library(Covid)

# summary data ------------------------------------------------------------

# available treatments
#world.measures
#show_countries_of(measure = c("School","Curfew","Ban","Canc","clos"))

# subset only countries (no regions)
df <- subset(all, adm_level==0)
#df <- world

dflong <- subset(all_long, adm_level==0)

treatments.summary <-  dflong %>% group_by(treatment) %>%
  summarise(first = min(date[active],na.rm=TRUE),
            last = max(date[active],na.rm=TRUE),
            ratio_active = mean(active,na.rm=TRUE),
            num_countries = sum(length(unique(country[active]))),
            enacted_in = list(unique(country[active])))
treatments.summary
treatments.summary$enacted_in[8]

treatments.country.summary <-  dflong %>%
  group_by(treatment,country) %>%
  summarise(n=n(),
            first = min(date[active],na.rm=TRUE),
            last = max(date[active],na.rm=TRUE),
            days_active = sum(active,na.rm=TRUE),
            range_active = last-first+1) %>% filter(days_active>0)
View(treatments.country.summary)


countries <- dflong %>%
  group_by(country) %>%
  summarise(first_case = min(date[positive>0],na.rm=TRUE),
            first_cluster = min(date[positive>100],na.rm=TRUE),
            highest_new = max(positive,na.rm=TRUE),
            first_treatment = min(date[active],na.rm=TRUE),
            first_treatment_any = min(date[share>0],na.rm=TRUE),
            in_country = unique(country)[1],
            in_country_wrong = unique(country)[2])
countries %>% filter(first_treatment<"2020-04-01")
countries %>% filter(first_treatment_any<"2020-04-01")

View(countries %>% filter(highest_new>100))


ggplot(all_long %>% filter(name=="Germany"), aes(x=date,y=treatment,col=active))+geom_point()
ggplot(all_long %>% filter(name=="Germany"), aes(x=date,y=treatment,col=share>0))+geom_point()

ggplot(all_long %>% filter(country=="Germany")%>%filter(treatment %in% unique(treatment[active])), aes(x=date,y=share,col=treatment))+
  geom_line(alpha=.6)+facet_wrap(vars(name))


ggplot(all_long %>% filter(country=="Germany"),
       aes(x=date,y=treatment,col=share>0,shape=active))+
  geom_point()+facet_wrap(vars(name))+
  xlim(as.Date(c("2020-03-10","2020-03-26")))

ggplot(all_long %>% filter(country=="Italy"),
       aes(x=date,y=name,col=share>0,shape=active))+
  geom_point()+facet_wrap(vars(treatment))+
  xlim(as.Date(c("2020-02-15","2020-03-26")))

ggplot(all_long %>% filter(country=="Germany",adm_level==0),
       aes(x=date,y=share,col=treatment,linetype=is.na(active)))+
  geom_line()


ggplot(all_long %>% filter(country=="Italy",adm_level==0),
       aes(x=date,y=share,col=treatment,linetype=is.na(active)))+
  geom_line()


ggplot(all_long %>% filter(adm_level==0,is.finite(byC_first_treatment)),
       aes(x=date,y=country,col=share>0,shape=active))+
  geom_point()+facet_wrap(vars(treatment))+
  xlim(as.Date(c("2020-02-15","2020-03-26")))


df.cur <- df%>%
  select(pos.growth,
         contains("tt_SchoolC"),
         contains("tt_Cur"),
         wind,precip,tMax,tMin,cloud)%>%drop_na()



# panel regression --------------------------------------------------------



plm.pool <- plm(
  formula = "lead(pos.growth,7)~
  wind+precip+tMax+tMin+cloud+
  tt_CurfewIMildOnlyPrivatePublicLifeI+tt_SchoolClosings",
  data = df,model ="pooling")

plm.pool <- plm(
  formula = "lead(pos.growth,7)~
  wind+precip+tMax+tMin+cloud+
  tt_CurfewIMildOnlyPrivatePublicLifeI+tt_SchoolClosings",
  data = df,effect ="individual")

plm.time<- plm(
  formula = "lead(pos.growth,7)~
  wind+precip+tMax+tMin+cloud+
  tt_CurfewIMildOnlyPrivatePublicLifeI+tt_SchoolClosings",
  data = df,effect ="time")

plm.ind<- plm(
  formula = "lead(pos.growth,7)~
  wind+precip+tMax+tMin+cloud+
  tt_CurfewIMildOnlyPrivatePublicLifeI+tt_SchoolClosings",
  data = df,effect ="individual")

plm.both <- plm(
  formula = "lead(pos.growth,7)~
  wind+precip+tMax+tMin+cloud+
  tt_CurfewIMildOnlyPrivatePublicLifeI+tt_SchoolClosings",
  data = df,effect ="twoways")


stargazer(type="text",
          #omit.labels=c("Country-specific time trend","Country-specific squared time trend"),omit.yes.no = c("Yes","-"),
          lmtest::coeftest(plm.pool,vcov=vcovHC(plm.pool,cluster="group")),
          lmtest::coeftest(plm.time,vcov=vcovHC(plm.time,cluster="group")),
          lmtest::coeftest(plm.ind,vcov=vcovHC(plm.ind,cluster="group")),
          lmtest::coeftest(plm.both,vcov=vcovHC(plm.both,cluster="group")),add.lines =
            list(
              c("effects","pooling","time","individual","both")
            ))


