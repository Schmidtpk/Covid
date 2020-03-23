#.rs.restartR()
df <- read.csv("./data-raw/novel-corona-virus-2019-dataset/covid_19_data.csv")
dim(df)
ls(df)
library(dplyr)

# data cleaning -----------------------------------------------------------

df$Country.Region <- as.character(df$Country.Region)
df$Province.State <- as.character(df$Province.State)

# create label of observation
df$label <- ifelse(df$Province.State=="",df$Country.Region,df$Province.State)

# safe in which country if applicable
df$in_country <- ifelse(df$Province.State=="",NA,df$Country.Region)


# rename variables
df <- as_tibble(df)
df <- df %>%
  rename(
    Date = ObservationDate,
    pos.new = Confirmed,
    dead = Deaths,
    country = Country.Region,
    region = Province.State
  )

# date
df$Date <- as.Date(df$Date,"%m/%d/%Y")
df$t <- df$Date-min(df$Date)

# compute additional variables
df <- df %>% group_by(label) %>% arrange(Date) %>%
  mutate(pos.total = cumsum(pos.new),
         dead.total = cumsum(dead))

# check for duplicates and drop
dim(df)
sum(duplicated(df %>% select(label,Date,Last.Update)))
df <- df %>% group_by(label,t) %>%
  mutate(temp_n = n(),
         temp_rank = rank(desc(Last.Update))) %>%
  filter(temp_rank==1)
dim(df)
sum(duplicated(df %>% select(label,t,Last.Update)))

# make panel data frame pdata.frame
df <- plm::pdata.frame(df,index = c("label","t"), stringsAsFactors=F)




# define new columns ------------------------------------------

# extract treatments
treatments <- unique(measures$Type)
#td: formatting?


# define dummies for treatments (true if treatment currently active)
df[,paste0(treatments,"_active")] <- FALSE

# define variable for saving time of first enactement
df[,paste0(treatments,"_t")] <- as.Date("2000-01-01")

# define variable for difference to first enactement
df[,paste0(treatments,"_diff")] <- 0

# for comparison necessary
df$region <- as.character(df$region)

# loop over all measures and safe if active and safe first enactement date
# sort before by ADM1 (region overwrites nation) when on same date
measures <- measures %>% arrange(desc(ADM1))
for (i in 1:nrow(measures))
{

  if(measures$Type[i]!="")
  {
    # right region or nation
    if(measures$ADM1[i]=="")
      correct.i <- (df$country==measures$Country[i])
    else
      correct.i <- (df$region==measures$ADM1[i])

    # after starting date
    correct.t <- df$Date >= measures$Start[i] &
      # if end date exists, before end date
      (if(is.na(measures$End[i])) TRUE else (df$Date <= measures$End[i]))

    # check if each measure has fit
    if(sum(correct.i)==0)
    {

      if(measures$ADM1[i]=="")
        correct.i <- grepl(substr(measures$ADM1[i],1,5),df$country,ignore.case = T)
      else
        correct.i <- grepl(substr(measures$ADM1[i],1,5),df$region,ignore.case = T)

      if(sum(correct.i)!=0)
      {
        warning(paste("region-country",measures$ADM1[i],measures$Country[i],"has only fit after loosening matching"))
      } else {
        warning(paste("region-country",measures$ADM1[i],measures$Country[i],"has no fit"))
      }

      if(length(unique(df$region[correct.i]))>1 & measures$ADM1[i]!="")
        warning(paste("Multiple regions were assigned to aforementioned measure with mild matching"))
      if(length(unique(df$country[correct.i]))>1 & measures$ADM1[i]=="")
        warning(paste("Multiple countries were assigned to aforementioned measure with mild matching"))

    } else if(sum(correct.i & correct.t)==0)
    {
      warning(paste("mesaure",measures$Type[i],"in",measures$ADM1[i],measures$Country[i]," at date", measures$Start[i],"has no fit"))
    }

    # for each measure find dates and regions that currently enact it
    df[
      correct.t & correct.i,
      paste0(measures$Type[i],"_active")
      ] <- TRUE

    # for each measure save enacting date
    df[
      correct.i,
      paste0(measures$Type[i],"_t")
      ] <- measures$Start[i]

    # for each measure save difference to enacting date
    df[
      correct.i,
      paste0(measures$Type[i],"_diff")
      ] <-
      df$Date[correct.i] - measures$Start[i]
  }
}

subset(df, country=="Italy",
       select = c("Date",
                  "UniversityClosings_active",
                  "SchoolClosings_active",
                  "SchoolClosings_diff",
                  "SchoolClosings_t"))

subset(measures, Country=="Italy", select = c("Type","Start", "ADM1"))

# save for package --------------------------------------------------------
world <- df
#use_data(world,overwrite = TRUE)
world.measures <- treatments
#use_data(world.measures,overwrite = T)


# save regions for codebook
world_codes_country <- as.matrix(unique(df$country),ncol=1)
world_codes_region <- as.matrix(unique(df$region),ncol=1)
write.csv(world_codes_region,
          "./data-raw/novel-corona-virus-2019-dataset/world_codes_region.csv")
write.csv(world_codes_country,
          "./data-raw/novel-corona-virus-2019-dataset/world_codes_country.csv")


