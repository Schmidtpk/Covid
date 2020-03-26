rm(list=ls())
library(Covid)

# summary data ------------------------------------------------------------

# available treatments
#world.measures
#show_countries_of(measure = c("School","Curfew","Ban","Canc","clos"))

# subset only countries (no regions)
df <- subset(Covid::all, adm_level==1)
#df <- world

dflong <- Covid::all_long

treatments.summary <-  dflong %>% group_by(treatment) %>%
  summarise(first = min(date[active],na.rm=TRUE),
            last = max(date[active],na.rm=TRUE),
            ratio_active = mean(active,na.rm=TRUE),
            num_countries = sum(length(unique(country[active]))),
            enacted_in = list(unique(country[active])))
treatments.summary
treatments.summary$enacted_in[8]

treatments.country.summary <-  dflong %>%
  filter(adm_level==1)%>%
  group_by(treatment,country) %>%
  summarise(first = min(date[active],na.rm=TRUE),
            last = max(date[active],na.rm=TRUE),
            days_active = sum(active,na.rm=TRUE),
            range_active = last-first+1) %>% filter(days_active>0)
View(treatments.country.summary)


countries <- dflong %>%
  filter(adm_level==1)%>%
  group_by(country) %>%
  summarise(first_case = min(date[positive>0],na.rm=TRUE),
            first_cluster = min(date[positive>100],na.rm=TRUE),
            highest_new = max(positive,na.rm=TRUE),
            first_treatment = min(date[active],na.rm=TRUE),
            first_treatment_any = min(date[share>0],na.rm=TRUE),
            in_country = unique(in_country)[1],
            in_country_wrong = unique(in_country)[2])
countries %>% filter(first_treatment<"2020-04-01")
countries %>% filter(first_treatment_any<"2020-04-01")

View(countries %>% filter(highest_new>100))


ggplot(all_long %>% filter(name=="Germany"), aes(x=date,y=treatment,col=active))+geom_point()
ggplot(all_long %>% filter(name=="Germany"), aes(x=date,y=treatment,col=share>0))+geom_point()

ggplot(all_long %>% filter(country=="Germany")%>%filter(treatment %in% unique(treatment[active])), aes(x=date,y=share,col=treatment))+
  geom_line(alpha=.6)+facet_wrap(vars(name))



