library(dplyr)
#.rs.restartR()
rm(list=ls())


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


# expand treatment (type and date) --------------------------------------------------------

treatments_long <- treatments_long %>%
  tidyr::expand(type,date,
                  tidyr::nesting(country,
                                 region,
                                 country.code,
                                 region.code,
                                 name,
                                 adm_level,
                                 ratio.pop,
                                 population)#KEY: If additional variables in treatments_long, put here!
  )%>%left_join(treatments_long)

#View(treatments_long %>% filter(country=="Germany", date=="2020-03-20", type=="SchoolClosings"))


#rename type to treatment
treatments_long$treatment <- treatments_long$type;treatments_long$type<- NULL




# compute active ----------------------------------------------------------
# active if after start and before end (if exists)
treatments_long$active <- (treatments_long$date>=treatments_long$start)
non_na_end <-!is.na(treatments_long$end)
mean(non_na_end)
and_smaller <- non_na_end & (treatments_long$date<=treatments_long$end)
mean(and_smaller)
treatments_long$active[and_smaller] <- FALSE

mean(treatments_long$active,na.rm=T)


# drop start and end
treatments_long <- treatments_long %>% select(-c(start,end))

# merge meta (within treatment) ------------------------------------------------------------

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


#  join share ----------------------------------------------------------
# treatments_long$share <- as.numeric(as.character(treatments_long$share))
# #treatments_long %>% filter(share<1)
#
# #dim(treatments_long)
# treatments_long <-
#   treatments_long %>% group_by(region,country,date,treatment) %>%
#   mutate(share = sum(share[active],na.rm=T),
#          rank_temp = rank(date,ties.method = "random")) %>%
#   filter(rank_temp==1) %>% select(-rank_temp) %>% ungroup()
# #dim(treatments_long)
# #View(treatments_long %>% filter(name=="Iran"))
#

# +++ IMPLICATIONS ------------------------------------------------------------


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
#is.numeric(treatments_long$share)
is.logical(treatments_long$active)
#View(treatments_long %>% filter(country=="Germany", date=="2020-03-20",!is.na(active)))

mean(treatments_long$active,na.rm=TRUE)

# share -------------------------------------------------------------------



#within country and treatment, add share according to populations
treatments_long <- treatments_long %>%
  group_by(country.code,date,treatment) %>%
  mutate(

    share = if_else(adm_level==1,#if country
                      if_else(is.na(share), #and NA (share of treatment not given in data for country)
                              sum(ratio.pop*active*(adm_level==2),na.rm=TRUE), #weighted share of member regions
                   share),share)
  ) %>% ungroup()



# View(treatments_long %>% filter(adm_level==1,share > 0, is.na(active)))
#

# View(treatments_long %>% filter(share_n>0, share_n <1))
# View(treatments_long %>% filter(country=="Germany", date>="2020-03-16", treatment=="SchoolClosings"))
# ggplot(treatments_long %>%
#          filter(country=="Germany", treatment=="SchoolClosings"),
#        aes(x=date,col=active,y=region))+geom_point()
#
# ggplot(treatments_long %>%
#          filter(country=="Germany",adm_level==1),
#        aes(x=date,col=treatment,y=share_n))+geom_line()

treatments_long$share<-as.numeric(treatments_long$share)
treatments_long$active<-as.logical(treatments_long$active)


# +++ TREATMENT WIDE ----------------------------------------------------------
treatments_wide <- treatments_long %>%
  tidyr::pivot_wider(
    names_from = treatment,
    values_from = c(active,share),#c(active,share,meta),
    names_sep = "XXX"
  )

# treatments --------------------------------------------------------------

# show treatments in data
unique(treatments_long$treatment)

#define list of implications
measures_implies <- list(
  c("BanofGroupGatherings","CancelationofLargeEvents"),
  c("CurfewILockdownofAllNonEssentialPublicLifeI","CurfewIMildOnlyPrivatePublicLifeI")
)

mean(treatments_wide$activeXXXCancelationofLargeEvents,na.rm=T)
mean(treatments_wide$activeXXXBanofGroupGatherings,na.rm=T)

### change data
for(cur.implication in measures_implies)
{
  treatments_wide[,paste0("activeXXX",cur.implication[2])]<-
    if_else(is.na(treatments_wide[,paste0("activeXXX",cur.implication[1])]),
          treatments_wide %>% pull(paste0("activeXXX",cur.implication[2])),
          treatments_wide %>% pull(paste0("activeXXX",cur.implication[2])) |
            treatments_wide %>% pull(paste0("activeXXX",cur.implication[1]))
             )
}
mean(treatments_wide$activeXXXCancelationofLargeEvents,na.rm=T)
mean(treatments_wide$activeXXXBanofGroupGatherings,na.rm=T)




# +++ MERGE ---------------------------------------------------------------

# get data
df <- Covid::jhu


all <- df %>%
  full_join(
    treatments_wide %>%
      select(name,
             country,
             region,
             date,
             adm_level,
             population,
             ratio.pop,
             starts_with("active"),
             starts_with("share")), #KEY
             by=c("date",
                  "region",
                  "country"))



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



dim(all_long)
#all_long


# +++ Compute -----------------------------------------------------------


# bycountry ---------------------------------------------------------------

all_long <- all_long %>% group_by(country) %>%
  mutate(
    byC_first_treatment = min(date[(active>0)],na.rm=T),
    byC_first_dead = min(date[dead>0],na.rm=T),
    byC_first_infected = min(date[positive>0],na.rm=T),
    byC_num_treatments = length(unique(treatment[active]))-1
  )
all_long %>% select(starts_with("byC_")) %>% filter(byC_num_treatments>0)%>%slice(1)


#all <- all %>% left_join(all_long%>%select(starts_with("byC_"),country),by = c("country"))

# +++ Export --------------------------------------------------------------

# change column type back
use_data(all_long,overwrite = TRUE)





mean(is.na(all$t))
mean(is.na(all$name))

# make panel data frame pdata.frame
all$t <- all$date-min(all$date)
all$i <- paste0(all$region,all$country.code)
all <- plm::pdata.frame(all,index = c("i","t"), stringsAsFactors=F)
use_data(all,overwrite = TRUE)


# +++ TESTS -------------------------------------------------------------------

#
# # non-merged --------------------------------------------------------------
#
# # in treatments but no observation
# setdiff(treatments%>%select(region,country),
#         Covid::jhu%>%select(region,country)) %>% select(country,region)
#
# setdiff(treatments%>%filter(adm_level==1)%>%select(country),
#         Covid::jhu%>%select(country)) %>% pull(country)
#
#
# # in observation but no treatments
# # setdiff(df%>%select(region,country),
# #         treatments%>%select(region,country)) %>% filter(is.na(region)) %>% pull(country)
#
#
#
# # treatment plots ---------------------------------------------------------
#
# library(ggplot2)
# ggplot(all_long %>% filter(country=="Germany"),
#        aes(x=date,y=treatment,col=active))+
#   geom_point()+facet_wrap(vars(region))
#
# ggplot(treatments_long %>% filter(adm_level==1),
#        aes(x=date,y=share,col=treatment))+
#   geom_line()+facet_wrap(vars(country))
#
#
# # outcome plots ---------------------------------------------------------
#
#
#
# ggplot(all %>% filter(is.finite(byC_first_treatment),adm_level==1),
#        aes(x=date,y=log(dead)))+
#   geom_point()+facet_wrap(vars(name))
# ggplot(all %>% filter(is.finite(byC_first_treatment),adm_level==1), aes(x=date,y=log(positive)))+
#   geom_point()+facet_wrap(vars(name))
#
#
# # plm regression ----------------------------------------------------------
#
# all$pos.growth <- log(all$positive+1)-lag(log(all$positive+1))
#
# plm.cur <- plm::plm(pos.growth~1,all,model="pooling")
# stargazer::stargazer(type="text",omit="factor",
#   lmtest::coeftest(plm.cur,
#                    vcov=sandwich::vcovHC(plm.cur,cluster="group")))
#
#
#
#
# # Summary treatments ------------------------------------------------------
#
#
#
# # +++ SUMMARY -------------------------------------------------------------
#
#
# # treatments -------------------------------------------------------------------------
# ls(treatments)
# # identifier: name country start end
#
# # treatments_long -------------------------------------------------------------------------
# ls(treatments_long)
# # identifier: name country date treatment
#
# # outcome data -------------------------------------------------------------------------
# ls(df)
# # identifier: name country date
#
# # all_long -------------------------------------------------------------------------
# ls(all_long)
# # identifier: name country date treatment
#
# # all -------------------------------------------------------------------------
# ls(all)
# # identifier: i=region+country date
# # treatments safed as tt_"treatment"
#
#



