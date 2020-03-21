df <- read.csv("./data-raw/novel-corona-virus-2019-dataset/covid_19_data.csv")
dim(df)
ls(df)
head(df)

a <- table(df$Province.State[df$Country.Region=="Mainland China"])
a[a!=0]

df <- read.csv("./data-raw/novel-corona-virus-2019-dataset/COVID19_open_line_list.csv",)
dim(df)
ls(df)
head(df)

table(df$symptoms)

df$date <- as.Date(as.character(df$date_confirmation),format = "%d.%m.%y")

ggplot(df, aes(x=date))+geom_histogram()
table(df$country)
df <- read.csv("./data-raw/novel-corona-virus-2019-dataset/COVID19_line_list_data.csv",)

df <- as_tibble(df)
dim(df)
ls(df)
head(df)

df$date <- as.Date(as.character(df$reporting.date),format = "%m/%d/%y")
ggplot(df, aes(x=date))+geom_histogram()
table(df$country)
