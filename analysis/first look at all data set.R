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
#View(treatments.country.summary)


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

#View(countries %>% filter(highest_new>1000))




# US ---------------------------------------------------------------

USstates <- Covid::all_long %>%filter(country=="US",region %in%state.name)%>%
  group_by(region) %>%
  summarise(first_case = min(date[positive>0],na.rm=TRUE),
            first_cluster = min(date[positive>100],na.rm=TRUE),
            highest_positive = max(positive,na.rm=TRUE),
            highest_dead = max(dead,na.rm = TRUE),
            first_treatment = min(date[active],na.rm=TRUE),
            first_treatment_any = min(date[share>0],na.rm=TRUE),
            in_country = unique(country)[1],
            in_country_wrong = unique(country)[2])
View(USstates)

ggplot(all_long %>% filter(country=="US"),
       aes(x=date,y=dead,col=region))+
         geom_line()

ggplot(all_long %>% filter(adm_level==0,is.finite(byC_first_treatment)),
       aes(x=date,y=dead))+
  geom_line()+facet_wrap(vars(country))

# Some plots --------------------------------------------------------------



ggplot(all_long %>% filter(name=="Germany"),
       aes(x=date,y=treatment,col=active,shape=share>0.5))+
  geom_point()+facet_wrap(vars(name))
#todo: Why cancelation has zero share?
#View(all_long %>% filter(name=="Germany",date=="2020-03-20", treatment =="CancelationofLargeEvents"))




ggplot(all_long %>% filter(country=="Germany",
                           treatment %in% unique(treatment[active])),
       aes(x=date,y=share,col=treatment))+
  geom_line(alpha=.6)+
  facet_wrap(vars(name))


ggplot(all_long %>% filter(country=="Germany"),
       aes(x=date,y=treatment,col=share>0,shape=active))+
  geom_point()+facet_wrap(vars(name))+
  xlim(as.Date(c("2020-03-10","2020-03-26")))

ggplot(all_long %>% filter(country=="Italy"),
       aes(x=date,y=name,col=share>0,shape=active))+
  geom_point()+facet_wrap(vars(treatment))+
  xlim(as.Date(c("2020-02-15","2020-03-26")))

ggplot(all_long %>% filter(country%in%unique(treatments$country),
                           adm_level==0),
       aes(x=date,y=name,col=share>0,shape=active))+
  geom_point()+facet_wrap(vars(treatment))+
  xlim(as.Date(c("2020-02-15","2020-03-26")))


ggplot(all_long %>% filter(country=="Italy",adm_level==0),
       aes(x=date,y=share,col=treatment,linetype=is.na(active)))+
  geom_line()

#todo: active whenever share is positive (why different?)
#all_long %>% filter(is.na(active), share>0) %>% View()


ggplot(all_long %>% filter(name=="Lombardy"),
       aes(x=date,y=pos.total_it))+
  geom_line()


ggplot(all_long %>% filter(country=="Italy",adm_level==0),
       aes(x=date,y=positive))+
  geom_point()





# panel regression --------------------------------------------------------

#df.cur <- df %>% filter(country %in% unique(treatments$country), !country %in% c("Iran","Japan","South Korea","US")) %>% mypanel()
#df.cur <- df %>% filter(country %in% unique(treatments$country)) %>% mypanel()
df.cur <- df %>% filter(country %in% c("Italy","Spain","France","Germany","Switzerland","Belgium","Netherlands")) %>% mypanel()

df.cur<- df.cur %>% filter(positive>100) %>% mypanel()


df.cur$outcome <- df.cur$dead
df.cur$outcome_new<- pmax(0,df.cur$outcome-plm::lag(df.cur$outcome))
df.cur$outcome_growth_rate <- log(df.cur$outcome_new/plm::lag(df.cur$outcome))

df.cur$outcome_time <- plm::lead(df.cur$outcome_growth_rate,7)

#View(df.cur %>% select(positive,country,date,contains("outcome")))

df.cur<- df.cur %>% filter(is.finite(outcome_time),!is.na(outcome_time)) %>% mypanel()


treatment_dummies <- paste(paste0("tt_",unique(treatments$type)),collapse = "+")


panel_reg(
  df=df.cur,
  paste0("outcome_time~",
  "wind+precip+tMax+tMin+cloud+humidity+",
  treatment_dummies))


panel_reg(
  df=df.cur,
  "outcome_time~
  wind+precip+tMax+tMin+cloud")

panel_reg(
  df=df.cur,
  "outcome_time~
  wind")

factorize <- function(x,length=5) {
  as.factor(
    findInterval(x,
                  quantile(x, seq(0+1/length,1-1/length,length.out = length-1),na.rm=T)
    )
    -(length-1)/2)}

factorize(df.cur$tMin,3)
factorize(df.cur$tMin,5)

panel_reg(
  df=df.cur,
  "outcome_time~
  factorize(tMax)+
  factorize(humidity)")



