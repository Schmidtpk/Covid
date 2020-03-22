library(Covid)

show_ts(outcome = "log(Deaths)",regions = NULL)+facet_wrap(vars(region))


show_ts(df = world, outcome = "dead",regions = "Italy")

show_ts(df = world, outcome = "dead",regions = "Germany")

show_ts(df = world, outcome = "dead",regions = "Mainland China")+
  facet_wrap(vars(region),scales = "free_y")


show_measures_of(c("Germany","Italy"))



