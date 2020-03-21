library(googledrive)

drive_download("https://docs.google.com/spreadsheets/d/1tYfD3vvSEaW3Cq9-UZoDtlKIfaMMVJq8XnV9XJUdK3s/edit#gid=0",
                     path = "./data-raw/measures/download_temp.csv",overwrite = TRUE)

df <- read.csv( "./data-raw/measures/download_temp.csv")

# data cleaning -----------------------------------------------------------

df$Start <- as.Date(df$Start, format = "%d.%m.%Y")


# save in package ---------------------------------------------------------

measures <- df
use_data(measures,overwrite = TRUE)
