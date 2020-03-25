
# +++ KAGGLE -------------------------------------------------------------------------

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
df$adm_level <- ifelse(df$region=="",1, 2)
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
df <- df %>% group_by(name,date) %>%
  mutate(temp_n = n(),
         temp_rank = rank(desc(Last.Update))) %>%
  filter(temp_rank==1)%>%ungroup()
dim(df)
sum(duplicated(df %>% select(name,date,Last.Update)))
df$Last.Update<-NULL
df$SNo<-NULL



# +++ TREATMENT ------------------------------------------
source("./data-raw/measures/create measures.R")

treatments$type <- paste0(treatments$type,"II",treatments$meta)



# expand time -----------------------------------------------------------------
date_min <- min(df$date,treatments$start,treatments$end,na.rm=TRUE)
date_max <- max(df$date,treatments$start,treatments$end,na.rm=TRUE)
treatments_long <- merge(treatments, seq(date_min,date_max,"days"), all = T)
treatments_long$date<-treatments_long$y
treatments_long$y<-NULL

# expand treatment --------------------------------------------------------

# df <- tibble(
#   group = c(1:2, 1),
#   item_id = c(1:2, 2),
#   item_name = c("a", "b", "b"),
#   value1 = 1:3,
#   value2 = 4:6
# )
# df %>% tidyr::complete(group, tidyr::nesting(item_id, item_name))
# df %>% tidyr::complete(group, item_id,item_name)
#
#
# dim(treatments_long)

#View(treatments_long %>% filter(country=="Germany", date=="2020-03-20"))
treatments_long <- treatments_long %>% tidyr::complete(type,date,
                                                       tidyr::nesting(name,country,adm_level,in_country))
#View(treatments_long2 %>% filter(country=="Germany", date=="2020-03-20"))



# make long --------------------------------------------------------------------
treatments_long <-treatments_long %>% tidyr::pivot_longer(type,"a","treatment")
treatments_long$a <- NULL
treatments_long$treatment <- treatments_long$value;treatments_long$value <- NULL


treatments_long


# compute active ----------------------------------------------------------
treatments_long$active <- treatments_long$date>=treatments_long$start
treatments_long$active <- ifelse(is.na(treatments_long$end),
                                 treatments_long$active,
                                 treatments_long$active & treatments_long$date<=treatments_long$end)

# compute meta ------------------------------------------------------------

# extract meta from treatment character
treatments_long$meta <- sapply(treatments_long$treatment,function(x) {
  split.cur <- stringr::str_split(x,"II")[[1]]
  return(split.cur[length(split.cur)])
})
treatments_long$treatment <- sapply(treatments_long$treatment,function(x) {
  stringr::str_split(x,"II")[[1]][1]
})


# merge meta of all actives within treatment and drop all but one
treatments_long <- treatments_long %>% group_by(name,date,treatment) %>%
  mutate(
    meta = ifelse(sum(active)>0,paste0(meta[active]),""),
    rank_temp = rank(active,ties.method = "random"))%>% filter(rank_temp==1) %>%
  select(-rank_temp)%>%
  ungroup()

# drop start and end
treatments_long <- treatments_long %>% select(-c(start,end))


#  join share ----------------------------------------------------------
treatments_long$share <- as.numeric(as.character(treatments_long$share))
#treatments_long %>% filter(share<1)

#dim(treatments_long)
treatments_long <-
  treatments_long %>% group_by(name,date,treatment) %>%
  mutate(share = sum(share[active]),
         rank_temp = rank(date,ties.method = "random")) %>%
  filter(rank_temp==1) %>% select(-rank_temp) %>% ungroup()
#dim(treatments_long)
#View(treatments_long %>% filter(name=="Iran"))


# +++ implications ------------------------------------------------------------


# in_country --------------------------------------------------------------
dim(treatments_long)

# within country and treatment, make region active if country is
treatments_long <- treatments_long %>% group_by(country,date,treatment) %>%
  mutate(
    country_and_active = if_else(adm_level==1,
                             if_else(as.logical(active),
                                     TRUE,
                                     FALSE),
                             FALSE),
    country_is_active = sum(as.logical(country_and_active),na.rm = T)>0,#one observation in group that is country and active
    active = if_else(country_is_active, #if treatment active in country
                          TRUE,
                         as.logical(active)) #else leave as is
  ) %>% select(-c(country_and_active,country_is_active)) %>% ungroup()
dim(treatments_long)
is.numeric(treatments_long$share)
is.logical(treatments_long$active)
#View(treatments_long2 %>% filter(country=="Germany", date=="2020-03-20",!is.na(active)))


# long format -------------------------------------------------------------
wide_temp <- treatments_long %>%
  tidyr::pivot_wider(
    names_from = treatment,
    values_from = c(active,share,meta),
    names_sep = "XXX"
  )



# treatments --------------------------------------------------------------

# show treatments in data
unique(treatments_long$treatment)

#define list of implications
measures_implies <- list(
  c("BanofGroupGatherings","CancelationofLargeEvents"),
  c("CurfewILockdownofAllNonEssentialPublicLife","CurfewIMildOnlyPrivatePublicLife")
)

mean(wide_temp$activeXXXCancelationofLargeEvents,na.rm=T)
mean(wide_temp$activeXXXBanofGroupGatherings,na.rm=T)

# change data
for(cur.implication in measures_implies)
{
  wide_temp[,paste0("activeXXX",cur.implication[2])]<-
    if_else(is.na(wide_temp[,paste0("activeXXX",cur.implication[1])]),
          wide_temp %>% pull(paste0("activeXXX",cur.implication[2])),
          wide_temp %>% pull(paste0("activeXXX",cur.implication[2])) |
            wide_temp %>% pull(paste0("activeXXX",cur.implication[1]))
             )
}
mean(wide_temp$activeXXXCancelationofLargeEvents,na.rm=T)
mean(wide_temp$activeXXXBanofGroupGatherings,na.rm=T)

#change column type for between long format
change <-
  startsWith(colnames(wide_temp),c("active"))|
  startsWith(colnames(wide_temp),c("meta"))|
  startsWith(colnames(wide_temp),c("share"))
wide_temp[,change] <- lapply(wide_temp[,change], as.character)



# redo to long format -------------------------------------------------
long_temp <- wide_temp %>%
  tidyr::pivot_longer(contains("XXX"),
                      names_to = c("type","treatment"),
                      names_sep = "XXX") %>%
  tidyr::pivot_wider(values_from = value,names_from = type)

# change column type back
change <- startsWith(colnames(long_temp),c("active"))
long_temp[,change] <- lapply(long_temp[,change], as.logical)
change <- startsWith(colnames(long_temp),c("share"))
long_temp[,change] <- lapply(long_temp[,change], as.numeric)


# safe
treatments_long <- long_temp

# long_temp
# treatments_long
#
# View(wide_temp %>% filter(name=="Germany")) #%>% select(name,adm_level,in_country,date,contains("Border")))
#
# wide_temp %>% filter(name=="Germany", date=="2020-01-202") %>% select(name,adm_level,in_country,date,contains("Border"))
# View(long_temp %>% filter(name=="Germany", date=="2020-01-202") %>% select(name,adm_level,in_country,date,contains("Border"),contains("treat"),contains("sha"),contains("act")))






# +++ df ------------------------------------------------------------------

# expand treatment ------------------------------------------------------------------
df_long <- merge(df, unique(treatments_long$treatment), all = T)
df_long$treatment <- df_long$y;df_long$y <- NULL


# +++ MERGE ---------------------------------------------------------------
df_long$treatment<-as.character(df_long$treatment)
change <- startsWith(colnames(df_long),c("active"))
df_long[,change] <- lapply(df_long[,change], as.logical)
change <- startsWith(colnames(df_long),c("share"))
df_long[,change] <- lapply(df_long[,change], as.numeric)

all_long <- df_long %>%
  left_join(treatments_long %>% select(date,name,treatment,active,adm_level,share,meta),
             by=c("date","name","treatment","adm_level"))

if(nrow(all_long)!=nrow(df_long))
  stop("Matching not perfect.")

if(typeof(all_long$active)!="logical")
  warning("wrong type")

if(typeof(all_long$share)!="double")
  warning("wrong type")

# check ---------------------------------------------------------
if(mean(duplicated(all_long%>%select(date,name,treatment))))
  stop("Matching not perfect.")

# View(all_long %>% group_by(name,date,treatment) %>%
#   summarise(temp_n = n()) %>%
#   filter(temp_n>1))


#View(all_long%>%filter(name=="Germany",treatment=="BorderClosing")%>%select(date,name,adm_level,treatment,active))
#View(all_long%>%filter(name=="Germany",date==as.Date("2020-02-04"))%>%select(date,name,adm_level,treatment,active))


# all_long%>%filter(name=="Japan",date==as.Date("2020-03-18"))%>%select(date,name,adm_level,treatment,active)
# all_long %>% select(treatment,date,name)

#all_long <- all_long %>% filter(adm_level==1)

#library(ggplot2)
#ggplot(all_long %>%filter(name=="Japan"),aes(x=date,y=treatment,col=active))+geom_point()






# +++ Compute -----------------------------------------------------------




# +++ Export --------------------------------------------------------------

# change column type back
use_data(all_long,overwrite = TRUE)

# widen to "all" ---------------------------------------------------------------------
all_long$tt <- all_long$active
all <- all_long %>% select(-active) %>%tidyr::pivot_wider(
                                       names_from = treatment,
                                       values_from = c(tt,share)
                                       )


# View(all%>%filter(name=="Germany",
#                   date==as.Date("2020-03-18"))%>%
#        select(date,name,
#               contains("CancelationofLargeEvents")))
# View(all_long%>%filter(name=="Germany",
#                   date==as.Date("2020-03-18")))





# make panel data frame pdata.frame
all$t <- all$date-min(all$date)
all <- plm::pdata.frame(all,index = c("name","t"), stringsAsFactors=F)
use_data(all,overwrite = TRUE)


# +++ TESTS -------------------------------------------------------------------

all$pos.growth <- log(all$pos.new)-lag(log(all$pos.new))

plm.cur <- plm::plm(pos.growth~share_BorderClosing+factor(name)*as.numeric(t),all,effect = "twoways")
stargazer::stargazer(type="text",omit="factor",
  lmtest::coeftest(plm.cur,
                   vcov=sandwich::vcovHC(plm.cur,cluster="group")))



# Summary treatments ------------------------------------------------------



# +++ SUMMARY -------------------------------------------------------------


# treatments -------------------------------------------------------------------------
ls(treatments)
# identifier: name country start end

# treatments_long -------------------------------------------------------------------------
ls(treatments_long)
# identifier: name country date treatment

# treatments_long -------------------------------------------------------------------------
ls(df)
# identifier: name country date treatment

# all_long -------------------------------------------------------------------------
ls(all_long)
# identifier: name country date treatment

# all -------------------------------------------------------------------------
ls(all)
# identifier: name country date
# treatments safed as tt_"treatment"





