#link from https://github.com/jgehrcke/covid-19-germany-gae/issues/43

library(httr)
library(dplyr)
library(jsonlite)

get <- GET("https://services7.arcgis.com/mOBPykOjAyBO2ZKk/arcgis/rest/services/RKI_Landkreisdaten/FeatureServer/0/query?f=json&where=1%3D1&returnGeometry=false&spatialRel=esriSpatialRelIntersects&outFields=*&orderByFields=cases%20desc&outSR=102100&resultOffset=0&resultRecordCount=1000&cacheHint=true")





df <- content(get, "text")

df <- fromJSON(df)

df <- df$features

dat <- df$attributes

ls(dat)

