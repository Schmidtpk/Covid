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
treatments.summary$enacted_in[7]

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


ggplot(all_long %>% filter(country=="Italy",adm_level==0),
       aes(x=date,y=positive))+
  geom_point()



# panel regression --------------------------------------------------------

df$outcome <- df$positive
df$outcome <- pmax(0,df$outcome-plm::lag(df$outcome))
df$pos.growth <- log(df$positive/plm::lag(df$outcome))
df<-subset(df,subset=positive>10&plm::lag(df$outcome)>10)



panel_reg(
  df=df,
  "plm::lead(pos.growth,6)~
  wind+precip+tMax+tMin+cloud+
  tt_SchoolClosings+
  tt_CurfewIMildOnlyPrivatePublicLifeI")



df <- df %>% group_by(country) %>%
  mutate(
    temperature = tMax-mean(tMax,na.rm=TRUE)
  ) %>% ungroup() %>% mypanel()

attributes(df)

panel_reg(
  df=df,
  "lead(pos.growth,7)~
  wind+precip+tMax+tMin+cloud+temperature+
  tt_SchoolClosings+
  tt_CurfewIMildOnlyPrivatePublicLifeI")
