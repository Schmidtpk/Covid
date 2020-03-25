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
  summarise(first = min(date[active]),
            last = max(date[active]),
            ratio_active = mean(active),
            num_countries = sum(length(unique(country[active]))),
            enacted_in = list(unique(country[active])))
treatments.summary
treatments.summary$enacted_in[8]

treatments.country.summary <-  dflong %>% group_by(treatment,country) %>%
  summarise(first = min(date[active]),
            last = max(date[active]),
            days_active = sum(active),
            range_active = last-first+1) %>% filter(days_active>0)
View(treatments.country.summary)


countries <- dflong %>% group_by(name) %>%
  summarise(first_case = min(date[pos.new>0],na.rm=TRUE),
            first_cluster = min(date[pos.new>100],na.rm=TRUE),
            highest_new = max(pos.new,na.rm=TRUE),
            first_treatment = min(date[active],na.rm=TRUE),
            in_country = unique(in_country)[1],
            in_country_wrong = unique(in_country)[2])
View(countries %>% filter(first_treatment<"2020-04-01"))
View(countries %>% filter(highest_new>100))




show_measures_of(units = "United Kingdom",df = df)
show_measures_of(units = "Germ",df=df)

