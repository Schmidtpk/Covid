rm(list=ls())
library(Covid)

# summary data ------------------------------------------------------------

# available treatments
world.measures
#show_countries_of(measure = c("School","Curfew","Ban","Canc","clos"))

# subset only countries (no regions)
df <- subset(world, is.na(in_country))
#df <- world

dflong <- df %>%
  tidyr::pivot_longer(cols = matches(world.measures) & ends_with("_active"),
                      c("measure"),values_to = "active")

measures.summary <-  dflong %>% group_by(measure) %>%
  summarise(first = min(Date[active]),
            last = max(Date[active]),
            ratio_active = mean(active),
            num_countries = sum(length(unique(country[active]))),
            enacted_in = list(unique(country[active])))
measures.summary
measures.summary$enacted_in[8]

measures.country.summary <-  dflong %>% group_by(measure,country) %>%
  summarise(first = min(Date[active]),
            last = max(Date[active]),
            days_active = sum(active),
            range_active = last-first+1,
            enacted_in = list(unique(country[active]))) %>% filter(days_active>0)
View(measures.country.summary)


countries <- dflong %>% group_by(label) %>%
  summarise(first_case = min(Date[pos.new>0]),
            first_cluster = min(Date[pos.new>100]),
            highest_new = max(pos.new),
            first_measure = min(Date[active]),
            in_country = unique(in_country)[1],
            in_country_wrong = unique(in_country)[2])
View(countries %>% filter(first_measure<as.Date("2020-04-01")))
View(countries %>% filter(highest_new>100))

show_countries_of(df = df,measure_name = world.measures)


show_measures_of(units = "United Kingdom",df = df)
show_measures_of(units = "Germ",df=df)

