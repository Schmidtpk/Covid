library(dplyr)
#.rs.restartR()
rm(list=ls())


# helper functions --------------------------------------------------------

find_doubles_long <- function(df)
{nrow(df %>%
        count(date,region,country,treatment) %>%
        filter(n!=1))}

find_doubles <- function(df)
{nrow(df %>%
        count(date,region,country) %>%
        filter(n!=1))}

# +++ TREATMENT ------------------------------------------
source("./data-raw/measures/create measures.R")


#todo:stop dropping meta
treatments <- treatments %>% group_by(type,name) %>%
  mutate(
    start=min(start,na.rm=TRUE),
    end=max(end,na.rm=TRUE)
  )%>%
  filter(row_number()==1) %>% select(-c(meta))%>%ungroup()

#View(treatments%>%filter(country=="Germany", type=="SchoolClosings"))

##  expand time -----------------------------------------------------------------
date_min <- min(Covid::jhu$date,treatments$start,na.rm=TRUE)
date_max <- max(Covid::jhu$date,treatments$start,treatments$end,na.rm=TRUE)
treatments_long <- merge(treatments, seq(date_min,date_max,"days"), all = T)
treatments_long$date<-treatments_long$y
treatments_long$y<-NULL

#rename type to treatment
treatments_long$treatment <- treatments_long$type;treatments_long$type<- NULL

find_doubles_long(treatments_long)

# expand treatment (type and date) --------------------------------------------------------


find_doubles_long(treatments_long)



#View(treatments_long %>% filter(country=="Germany", date=="2020-03-20", treatment=="SchoolClosings"))





# compute active and share ----------------------------------------------------------
# active if after start and before end (if exists)
treatments_long$active <- (treatments_long$date>=treatments_long$start)
non_na_end <-!is.na(as.character(treatments_long$end))
mean(non_na_end)
and_smaller <- non_na_end & (treatments_long$date<=treatments_long$end)
mean(and_smaller)
treatments_long$active[and_smaller] <- FALSE

mean(treatments_long$active,na.rm=T)

treatments_long$share <- ifelse(treatments_long$active,treatments_long$share,0)

# View(treatments_long %>% filter(
#   country=="Germany",
#   date=="2020-03-20"))

# drop start and end
treatments_long <- treatments_long %>% select(-c(start,end))

# merge meta (within treatment) (?)------------------------------------------------------------

# View(treatments %>% filter(
#      type=="BanofGroupGatherings"))

# View(treatments_long %>% filter(
#   country=="Germany",
#   date=="2020-03-20",
#   treatment=="BanofGroupGatherings"))
#
# treatments_long <- treatments_long %>%
#   group_by(country,region,date,treatment) %>%
#   mutate(
#     meta = paste0(meta[active],collapse = "+"),
#     share = mean(share[active],na.rm=TRUE),
#     active = any(active,na.rm=TRUE)
#     )
#
# View(treatments_long %>% filter(
#   country=="Germany",
#   date=="2020-03-20",
#   treatment=="BanofGroupGatherings"))
#
#
# treatments_long <- treatments_long%>%distinct(country,region,date,treatment,.keep_all=TRUE)%>%
#   ungroup()

#View(treatments_long %>% filter(country=="Germany", date=="2020-03-20"))



# +++ IMPLICATIONS ------------------------------------------------------------


# active: c -> r --------------------------------------------------------------
dim(treatments_long)

is.logical(treatments_long$active)
is.numeric(treatments_long$share)

# within country and treatment, make region active if country is
treatments_long <- treatments_long %>% group_by(country,date,treatment) %>%
  mutate(
    country_and_active = if_else(adm_level==1,
                             if_else(active,
                                     TRUE,
                                     FALSE),
                             FALSE),
    country_is_active = sum(as.logical(country_and_active),na.rm = T)>0,#one observation in group that is country and active
    country_share = if(sum(adm_level==1)==0) NA else share[adm_level==1],
    active = if_else(country_is_active, #if treatment active in country
                          TRUE,
                         as.logical(active)), #else leave as is
    share = if_else(country_is_active, #if treatment active in country
                     country_share,
                     share) #else leave as is
  ) %>% select(-c(country_and_active,country_is_active)) %>% ungroup()
dim(treatments_long)

#View(treatments_long %>% filter(country=="Germany", date=="2020-03-20",!is.na(active)))

find_doubles_long(treatments_long)

# share: r ratio -> c -------------------------------------------------------------------

#within country and treatment, add share according to populations
treatments_long <- treatments_long %>%
  group_by(country,date,treatment) %>%
  mutate(
    ratio_temp = sum(ratio.pop*share*(adm_level==2),na.rm=TRUE),
    share = ifelse(adm_level==1,ifelse(is.na(share),ratio_temp,share),share)
  ) %>% select(-c(ratio_temp)) %>% ungroup()


# View(treatments_long %>% filter(share_n>0, share_n <1))
# View(treatments_long %>% filter(country=="Germany", date>="2020-03-20"))
# ggplot(treatments_long %>%
#          filter(country=="Germany", treatment=="SchoolClosings"),
#        aes(x=date,col=active,y=region))+geom_point()
#
# ggplot(treatments_long %>%
#          filter(country=="Germany",adm_level==1),
#        aes(x=date,col=treatment,y=share_n))+geom_line()

treatments_long$share<-as.numeric(treatments_long$share)
treatments_long$active<-as.logical(treatments_long$active)

find_doubles_long(treatments_long)
# +++ TREATMENT WIDE ----------------------------------------------------------
ls(treatments_long)
treatments_wide <- treatments_long %>%
  tidyr::pivot_wider(
    id_cols = c(date,country,region),
    names_from = treatment,
    values_from = c(active,share),#c(active,share,meta),
    names_sep = "XXX"
  )

find_doubles(treatments_wide)

# t1 -> t2 --------------------------------------------------------------

# show treatments in data
unique(treatments_long$treatment)

#define list of implications (first entry implies second)
measures_implies <- list(
  c("BanofGroupGatherings","CancelationofLargeEvents"),
  c("CurfewILockdownofAllNonEssentialPublicLifeI","CurfewIMildOnlyPrivatePublicLifeI")
)

mean(treatments_wide$activeXXXCancelationofLargeEvents,na.rm=T)
mean(treatments_wide$activeXXXBanofGroupGatherings,na.rm=T)

mean(treatments_wide$shareXXXCancelationofLargeEvents,na.rm=T)
mean(treatments_wide$shareXXXBanofGroupGatherings,na.rm=T)


### change data
for(cur.implication in measures_implies)
{
  treatments_wide[,paste0("activeXXX",cur.implication[2])]<-
    if_else(is.na(treatments_wide[,paste0("activeXXX",cur.implication[1])]),
          treatments_wide %>% pull(paste0("activeXXX",cur.implication[2])),
          treatments_wide %>% pull(paste0("activeXXX",cur.implication[2])) |
            treatments_wide %>% pull(paste0("activeXXX",cur.implication[1]))
             )

  treatments_wide[,paste0("shareXXX",cur.implication[2])]<-
    if_else(is.na(treatments_wide[,paste0("shareXXX",cur.implication[1])]),
            treatments_wide %>% pull(paste0("shareXXX",cur.implication[2])),
            treatments_wide %>% pull(paste0("shareXXX",cur.implication[1]))
            )

}
mean(treatments_wide$activeXXXCancelationofLargeEvents,na.rm=T)
mean(treatments_wide$activeXXXBanofGroupGatherings,na.rm=T)

mean(treatments_wide$shareXXXCancelationofLargeEvents,na.rm=T)
mean(treatments_wide$shareXXXBanofGroupGatherings,na.rm=T)



# +++ MERGE ---------------------------------------------------------------

find_doubles(treatments_wide)

# get data
df <- Covid::jhu

find_doubles(df)
find_doubles(treatments_wide)

all <- df %>%
  full_join(
    treatments_wide,
    by=c("date",
         "region",
         "country"))

find_doubles(all)

# check ---------------------------------------------------------
# doubles
doubles <-all %>%
  count(date,region,country) %>%
  filter(n!=1)
if(nrow(doubles)>0)
  warning(paste0("Matching not perfect for ",unique((doubles %>%pull(region)))))


# longer to "all_long" ---------------------------------------------------------------------


all_long <- all %>%
  tidyr::pivot_longer(cols = contains("XXX"),
                      names_to = c("type","treatment"),
                      names_sep = "XXX") %>%
  group_by(type) %>%
  mutate(rn = row_number()) %>% # recreated unique identifier column
  tidyr::pivot_wider(names_from ="type",values_from = "value")%>%select(-rn)%>%ungroup()

all_long$active<-as.logical(all_long$active)

dim(all_long)
#all_long

find_doubles_long(all_long)
# +++ Compute -----------------------------------------------------------


# bycountry ---------------------------------------------------------------

all_long <- all_long %>% group_by(country) %>%
  mutate(
    byC_first_treatment = min(date[(active>0)|(share>0)],na.rm=T),
    byC_first_dead = min(date[dead>0],na.rm=T),
    byC_first_infected = min(date[positive>0],na.rm=T),
    byC_num_treatments = length(unique(treatment[active]))-1
  )
all_long %>% select(starts_with("byC_")) %>% filter(byC_num_treatments>0)%>%slice(1)


#all <- all %>% left_join(all_long%>%select(starts_with("byC_"),country),by = c("country"))

# +++ Export --------------------------------------------------------------


all_long <- all_long %>%
  mutate(
    adm_level = ifelse(is.na(region),0,1),
    in_country = ifelse(adm_level==0,NA,country),
    name = ifelse(adm_level==0, country, region)
  )


# change column type back
#use_data(all_long,overwrite = TRUE)


all <- all %>%
  mutate(
    adm_level = ifelse(is.na(region),0,1),
    in_country = ifelse(adm_level==0,NA,country),
    name = ifelse(adm_level==0, country, region)
  )


#rename treatment dummies
dummies <- grepl("activeXXX",colnames(all))
names <- colnames(all)[dummies]
colnames(all)[dummies] <- paste0("tt_",substr(names,10,nchar(names)))

#rename share columns
dummies <- grepl("shareXXX",colnames(all))
names <- colnames(all)[dummies]
colnames(all)[dummies] <- paste0("share_",substr(names,9,nchar(names)))

# make panel data frame pdata.frame
all$t <- all$date-min(all$date)
all$i <- paste0(all$region,all$country)
all <- plm::pdata.frame(all,index = c("i","t"), stringsAsFactors=F)
#use_data(all,overwrite = TRUE)


# +++ TESTS -------------------------------------------------------------------


# non-merged --------------------------------------------------------------

# in treatments but no observation
setdiff(treatments%>%select(region,country),
        Covid::jhu%>%select(region,country)) %>% select(country,region)

setdiff(treatments%>%filter(adm_level==1)%>%select(country),
        Covid::jhu%>%select(country)) %>% pull(country)


# in observation but no treatments
# setdiff(df%>%select(region,country),
#         treatments%>%select(region,country)) %>% filter(is.na(region)) %>% pull(country)



# treatment plots ---------------------------------------------------------

library(ggplot2)
ggplot(all_long %>% filter(country=="Germany"),
       aes(x=date,y=treatment,col=active))+
  geom_point()+facet_wrap(vars(region))

ggplot(all_long %>% filter(country=="Germany",is.na(region)),
       aes(x=date,y=treatment,col=active))+
  geom_point()

ggplot(all_long %>% filter(country=="Germany",is.na(region)),
       aes(x=date,y=treatment,col=share>0))+
  geom_point()






# plm regression ----------------------------------------------------------

all$pos.growth <- log(all$positive+1)-lag(log(all$positive+1))

plm.cur <- plm::plm(pos.growth~1,all,model="pooling")
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

# outcome data -------------------------------------------------------------------------
ls(df)
# identifier: name country date

# all_long -------------------------------------------------------------------------
ls(all_long)
# identifier: name country date treatment

# all -------------------------------------------------------------------------
ls(all)
# identifier: i=region+country date
# treatments safed as tt_"treatment"





