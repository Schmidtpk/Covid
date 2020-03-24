
source("./data-raw/measures/create measures.R")
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

treatments$type <- paste0(treatments$type,"I",treatments$meta)

mean(duplicated(treatments%>%select(type,name)))

treatments %>% group_by(name,type)%>%
  summarise(num = n())%>%filter(num>1)

# # implications ------------------------------------------------------------
# #define list of implications
# measures_implies <- list(
#   c("BanofGroupGatherings","CancelationofLargeEvents"),
#   c("CurfewILockdownofAllNonEssentialPublicLifeI","CurfewILockdownofAllNonEssentialPublicLifeI")
# )
#
# # change data
# for(cur.implication in measures_implies)
# {
#   df[,paste0(cur.implication[2],"_active")]<-
#     as.vector(df[,paste0(cur.implication[1],"_active")]|
#                 df[,paste0(cur.implication[2],"_active")])
# }


# expand time -----------------------------------------------------------------
date_min <- min(df$date,treatments$start,treatments$end,na.rm=TRUE)
date_max <- max(df$date,treatments$start,treatments$end,na.rm=TRUE)
treatments <- merge(treatments, seq(date_min,date_max,"days"), all = T)



# make long --------------------------------------------------------------------
treatments_long <-treatments %>% tidyr::pivot_longer(type,"a","treatment")
treatments_long$a <- NULL
treatments_long$treatment <- treatments_long$value;treatments_long$value <- NULL
treatments_long$date<-treatments_long$y
treatments_long$y<-NULL

head(treatments_long)


# compute active ----------------------------------------------------------
treatments_long$active <- treatments_long$date>=treatments_long$start
treatments_long$active <- ifelse(is.na(treatments_long$end),
                                 treatments_long$active,
                                 treatments_long$active & treatments_long$date<=treatments_long$end)


# compute meta ------------------------------------------------------------

treatments_long$meta <- sapply(treatments_long$treatment,function(x) {
  split.cur <- stringr::str_split(x,"I")[[1]]
  return(split.cur[length(split.cur)])
})
treatments_long$treatment <- sapply(treatments_long$treatment,function(x) {
  stringr::str_split(x,"I")[[1]][1]
})

treatments_long$meta <- treatments_long$date>=treatments_long$start
treatments_long$active <- ifelse(is.na(treatments_long$end),
                                 treatments_long$active,
                                 treatments_long$active & treatments_long$date<=treatments_long$end)

treatments_long <- treatments_long %>% group_by(name,date,treatment) %>%
  mutate(
    meta = ifelse(sum(active)>0,paste0(meta[active]),""),
    rank_temp = rank(active,ties.method = "random"))%>% filter(rank_temp==1) %>%
  select(-rank_temp)%>%
  ungroup()



#  join share ----------------------------------------------------------
treatments_long$share <- as.numeric(as.character(treatments_long$share))
treatments_long %>% filter(share<1)

dim(treatments_long)
treatments_long <-
  treatments_long %>% group_by(name,date,treatment) %>%
  mutate(share = sum(share[active]),
         rank_temp = rank(date,ties.method = "random")) %>%
  filter(rank_temp==1)
dim(treatments_long)
#View(treatments_long %>% filter(name=="Iran"))
# +++ df ------------------------------------------------------------------

# expand treatment ------------------------------------------------------------------
df_long <- merge(df, unique(treatments_long$treatment), all = T)
df_long$treatment <- df_long$y;df_long$y <- NULL


# +++ MERGE ---------------------------------------------------------------
df_long$treatment<-as.character(df_long$treatment)
all_long <- df_long %>%
  left_join(treatments_long %>% select(date,name,treatment,active,adm_level,share),
             by=c("date","name","treatment","adm_level"))

if(nrow(all_long)!=nrow(df_long))
  stop("Matching not perfect.")


# find duplicates ---------------------------------------------------------
if(mean(duplicated(all_long%>%select(date,name,treatment))))
  stop("Matching not perfect.")

# View(all_long %>% group_by(name,date,treatment) %>%
#   summarise(temp_n = n()) %>%
#   filter(temp_n>1))


#View(all_long%>%filter(name=="Germany",treatment=="BorderClosing")%>%select(date,name,adm_level,treatment,active))
#View(all_long%>%filter(name=="Germany",date==as.Date("2020-02-04"))%>%select(date,name,adm_level,treatment,active))


all_long%>%filter(name=="Japan",date==as.Date("2020-03-18"))%>%select(date,name,adm_level,treatment,active)
all_long %>% select(treatment,date,name)

#all_long <- all_long %>% filter(adm_level==1)

ggplot2::ggplot(all_long %>%filter(name=="Japan"),aes(x=date,y=treatment,col=active))+geom_point()






# +++ Compute -----------------------------------------------------------




# +++ Export --------------------------------------------------------------


# widen to "all" ---------------------------------------------------------------------
all_long$tt <- all_long$active
all <- all_long %>% select(-active) %>%tidyr::pivot_wider(
                                       names_from = treatment,
                                       values_from = c(tt,share),
                                       #values_fill = list(active=FALSE),
                                       # values_fn = list(active = function(x) mean(x,na.rm=T),
                                       #                   meta = function(x) paste0(x)),
                                       #names_prefix = "T_"
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

#use_data(all,overwrite = TRUE)


# tests -------------------------------------------------------------------

all$pos.growth <- log(all$pos.new)-lag(log(all$pos.new))

plm.cur <- plm::plm(pos.growth~share_Curfew+factor(name)*as.numeric(t),all,effect = "twoways")
stargazer::stargazer(type="text",omit="factor",
  lmtest::coeftest(plm.cur,
                   vcov=sandwich::vcovHC(plm.cur,cluster="group")))



# Summary treatments ------------------------------------------------------



