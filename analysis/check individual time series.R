library(Covid)

show_ts(outcome = "log(Deaths)",regions = NULL)+facet_wrap(vars(region))


show_ts(df = world, outcome = "dead",regions = "Italy")
