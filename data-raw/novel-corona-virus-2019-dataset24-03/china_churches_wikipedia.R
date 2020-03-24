#.rs.restartR()
#rm(list=ls())
df <- read.csv("./data-raw/novel-corona-virus-2019-dataset24-03/covid_19_data.csv")
dim(df)
ls(df)
library(dplyr)

# data cleaning -----------------------------------------------------------

df$country <- as.character(df$Country.Region)
df$region <- as.character(df$Province.State)
df$Country.Region<-NULL
df$Province.State<-NULL

# create name of observation
df$name <- ifelse(df$region=="",df$country, df$region)
df$adm_Level <- ifelse(df$region=="",1, 2)
df$in_country <- ifelse(df$region=="",NA, df$country)


# rename variables
df <- as_tibble(df)
df <- df %>%
  rename(
    date = ObservationDate,
    pos.new = Confirmed,
    dead = Deaths
  )

# date
df$date <- as.Date(df$date,"%m/%d/%Y")




# check for duplicates and drop
dim(df)
sum(duplicated(df %>% select(name,date,Last.Update)))
df <- df %>% group_by(name,t) %>%
  mutate(temp_n = n(),
         temp_rank = rank(desc(Last.Update))) %>%
  filter(temp_rank==1)%>%ungroup()
dim(df)
sum(duplicated(df %>% select(name,t,Last.Update)))
df$Last.Update<-NULL
df$SNo<-NULL



# reshape treatment ------------------------------------------

# expand
date_min <- min(df$date,treatments$start,treatments$end,na.rm=TRUE)
date_max <- max(df$date,treatments$start,treatments$end,na.rm=TRUE)
treatments_l <- merge(treatments, seq(date_min,date_max,"days"), all = T)

treatments_long <-treatments_l %>% tidyr::pivot_longer(type,"a","treatment")
treatments_long$a <- NULL
treatments_long$treatment <- treatments_long$value;treatments_long$value <- NULL
treatments_long$date<-treatments_long$y
treatments_long$y<-NULL
treatments_long$active <- treatments_long$date>=treatments_long$start
treatments_long$active <- ifelse(is.na(treatments_long$end),
                                 treatments_long$active,
                                 treatments_long$active & treatments_long$date<=treatments_long$end)

all_long <- df %>% full_join(treatments_long,by=c("date","name"))
ls(df)
ls(treatments_long)
ls(all)
dim(all)

all_long %>% select(treatment,date,name)



# Add variables -----------------------------------------------------------




# All ---------------------------------------------------------------------

#   Format measures (implications, etc.) ------------------------------------

measures_implies <- list(
  c("BanofGroupGatherings","CancelationofLargeEvents"),
  c("CurfewILockdownofAllNonEssentialPublicLifeI","CurfewILockdownofAllNonEssentialPublicLifeI")
)

for(cur.implication in measures_implies)
{
  df[,paste0(cur.implication[2],"_active")]<-
    as.vector(df[,paste0(cur.implication[1],"_active")]|
                df[,paste0(cur.implication[2],"_active")])
}

# make panel data frame pdata.frame
df$t <- df$date-min(df$date)
df <- plm::pdata.frame(df,index = c("name","t"), stringsAsFactors=F)

world <- df
#use_data(world,overwrite = TRUE)



# Summary treatments ------------------------------------------------------





world.measures <- treatments
#use_data(world.measures,overwrite = T)


# save regions for codebook
world_codes_country <- as.matrix(unique(df$country),ncol=1)
world_codes_region <- as.matrix(unique(df$region),ncol=1)
write.csv(world_codes_region,
          "./data-raw/novel-corona-virus-2019-dataset/world_codes_region.csv")
write.csv(world_codes_country,
          "./data-raw/novel-corona-virus-2019-dataset/world_codes_country.csv")


