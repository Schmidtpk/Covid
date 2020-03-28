rm(list=ls())
library(Covid)

df <- Covid::ita
dflong <- Covid::ita_long

treatments.summary <-  dflong %>% group_by(treatment) %>%
  summarise(first = min(date[active],na.rm=TRUE),
            last = max(date[active],na.rm=TRUE),
            ratio_active = mean(active,na.rm=TRUE),
            num_regions = sum(length(unique(region[active]))),
            enacted_in = list(unique(region[active])))
treatments.summary
treatments.summary$enacted_in[9]

treatments.region.summary <-  dflong %>%
  group_by(treatment,region) %>%
  summarise(n=n(),
            first = min(date[active],na.rm=TRUE),
            last = max(date[active],na.rm=TRUE),
            days_active = sum(active,na.rm=TRUE),
            range_active = last-first+1) %>% filter(days_active>0)
View(treatments.region.summary)


# regions <- dflong %>%
#   group_by(region) %>%
#   summarise(first_case = min(date[positive>0],na.rm=TRUE),
#             first_cluster = min(date[positive>100],na.rm=TRUE),
#             highest_new = max(positive,na.rm=TRUE),
#             first_treatment = min(date[active],na.rm=TRUE),
#             first_treatment_any = min(date[share>0],na.rm=TRUE),
#             in_region = unique(region)[1],
#             in_region_wrong = unique(region)[2])
# regions %>% filter(first_treatment<"2020-04-01")
# regions %>% filter(first_treatment_any<"2020-04-01")

ggplot(dflong,aes(x=date,y=region,col=active))+
  geom_point()+
  facet_wrap(vars(treatment))

ggplot(dflong %>% select(-c(country_it,country.code_it,region.code_it,region_it,lat_it,long_it,date_it)) %>%
         ungroup() %>%
         pivot_longer(cols = contains("_it"),
                      names_to = "vvv",
                      values_to= "vall"),
       aes(x=date,y=vall,col=region))+
  geom_line()+facet_wrap(vars(vvv),scales = "free_y")



ggplot(dflong %>% filter(country=="Italy")%>%filter(treatment %in% unique(treatment[active])), aes(x=date,y=share,col=treatment))+
  geom_line(alpha=.6)+facet_wrap(vars(name))


ggplot(ita_long,aes(x=date,y=region,col=is.na(new.hostpial_or_home_it)))+geom_point()

# plm with R---------------------------------------------------------------------

rm(list=ls())
library(Covid)

df <- Covid::ita


df <- df %>% group_by(region) %>%
  mutate(
    R = as.numeric(addR(pos.total_it,date,total = TRUE))
  ) %>% ungroup() %>% mypanel()

df$i

df$outcome_time <- df$R
df.cur<- df %>% filter(is.finite(outcome_time),!is.na(outcome_time)) %>% mypanel()
treatment_dummies <- paste(paste0("tt_",unique(treatments$type)),collapse = "+")
treatment_dummies <-
  "tt_SchoolClosings+
   tt_CurfewIMildOnlyPrivatePublicLifeI+
   tt_CurfewILockdownofAllNonEssentialPublicLifeI"


panel_reg(
  df=df.cur,trendwith = "region",
  paste0("outcome_time~",
         "factorize(humidity)+",
         treatment_dummies))


# plm with growth rate ---------------------------------------------------------------------


#df$outcome <- df$new.hostpial_or_home_it

df$outcome <- df$pos.total_it
df$outcome <- df$outcome-lag(df$outcome)
df$pos.growth <- log(df$outcome+64)-lag(log(df$outcome+64))

unique(df %>% count(t,i) %>% filter(n!=1))

df <- df %>% filter(!is.na(pos.growth))%>%mypanel()

panel_reg("lead(pos.growth,5)~
  tt_CurfewIMildOnlyPrivatePublicLifeI+
          tt_SchoolClosings",
          df,trendwith = "region")

panel_reg("lead(pos.growth,5)~
  factorize(tMax)+factorize(humidity)+
  tt_CurfewIMildOnlyPrivatePublicLifeI+
          tt_SchoolClosings",
          df,trendwith = "region")
