library(Covid)

# summary data ------------------------------------------------------------

# available treatments
world.measures
show_countries_of(measure = c("School","Curfew","Ban","Canc","clos"))

df <- subset(world, is.na(in_country))

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

measures.country.summary <-  dflong %>% group_by(measure,country) %>%
  summarise(first = min(Date[active]),
            last = max(Date[active]),
            days_active = sum(active),
            range_active = last-first+1,
            enacted_in = list(unique(country[active]))) %>% filter(days_active>0)
View(measures.country.summary)




#drop countries without any measures in data set
#df <- keep_units_with(df,treatments)

show_measures_of(df = df)
show_countries_of(df = df,measure_name = world.measures)


