library(tidyr)
library(rjags)
library(coda)
library(Covid)
library(MCMCvis)
library(runjags)

rm(list = ls())
# DATA --------------------------------------------------------------------


# +international ----------------------------------------------------------
# data = all %>% filter(name%in%c(  "New York",
#                                   "UK",
#                                   "Germany",
#                                   "Iran",
#                                   "Italy",
#                                   "Japan",
#                                   "Spain",
#                                   "Netherlands",
#                                   "Belgium",
#                                   "Sweden",
#                                   "Switzerland",
#                                   "Louisiana",
#                                   "Massachusetts",
#                                   "Michigan",
#                                   "New Jersey"
#                                    ))
# dead_limit <- 10
# tau_max <- 200
# measurement_error_factor<- .1
# frate_min <- 0.001
# frate_max <- 0.01
# frate_sd_min <- 10^(-4)
# frate_sd_max <- 2*10^(-4)

# +Germany ----------------------------------------------------------
data <- all %>% filter(country%in%c("Germany"),
                      adm_level==1)
dead_limit <- 5
tau_max <- 200
measurement_error_factor <- .1
frate_min <- 0.0001
frate_max <- 0.005
frate_sd_min <- 10^(-4)
frate_sd_max <- 5*10^(-4)

# na to zero --------------------------------------------------------------
data <- data %>%
  mutate(
         dead.new = ifelse(is.na(dead.new),0,dead.new),
         pos.new = ifelse(is.na(pos.new),0,pos.new),
         pos.total = ifelse(is.na(pos.total),0,pos.total),
         in_model = dead.total > dead_limit)


# drop names with insufficent observations --------------------------------
data <- data %>% group_by(name) %>%
  mutate(
    sum_in_model = sum(in_model, na.rm = T)
  ) %>% filter(sum_in_model >5) %>% ungroup()


data <- data  %>% arrange(name,date)

d<- data%>%
  pivot_wider(c(name,date),names_from = name,values_from = dead.new)%>%
  select(-c(date))

#d <- simplify2array(by(data%>%pull(dead.new), data$name, as.vector))
##d is time*country
dim(d)

# define time intervals ---------------------------------------------------
T <- data %>% group_by(name) %>% summarize(length(dead.new))%>%pull()
sdat <- data %>% group_by(name,in_model) %>% mutate(count=row_number()) %>% filter(count==1) %>%select(-contains("tt"))
T0 <- sdat %>% group_by(name) %>% summarise(max(date)-min(date)) %>%pull()%>%as.numeric()

transmission_T <- 30
Tinit <- numeric(ncol(d))
Ti <- numeric(ncol(d))
for(c in 1:ncol(d)){
  Tinit[c]<- T0[c]-27
  Ti[c] <- Tinit[c]+7
}

names(T0)<-colnames(d)
names(Ti)<-colnames(d)
names(Tinit)<-colnames(d)


##### now prepare dat for JAGS
dat <- list("T" = T,
            "T0"=T0,
            "dead" = d,
            "transmission_T"=transmission_T,
            "tau_max"=tau_max,
            "measurement_error_factor"=measurement_error_factor,
            "Tinit"=Tinit,
            "Ti"=Ti,
            "frate_min"=frate_min,
            "frate_max"=frate_max,
            "frate_sd_min"=frate_sd_min,
            "frate_sd_max"=frate_sd_max)





# JAGS ----------------------------------------------------------------
jags.m <- jags.model( file = "./jags models/estimateR/stan flexible frate.R",
                      data=dat,
                      #inits=init_fun,
                      n.chains=4,
                      n.adapt=1000)


samps <- coda.samples(jags.m, c( "i",
                                 "iload",
                                 #"dload",
                                 "Ed",
                                 "dead",
                                  "R",
                                  "tau",
                                  "mean_SId",
                                  "sd_SId",
                                  "mean_transmission",
                                  #"sd_transmission",
                                  "frate",
                                 "sd_frate",
                                  "transmission_dist",
                                 #"transmission",
                                  "SId_dist"),
                       thin = 4, n.iter=4000)


 save(samps,file = "./jags models/save/estimateR_Ger")
# save(samps,file = "./jags models/save/estimateR_international")
# load(file = "./jags models/save/estimateR_international")
# load(file = "./jags models/save/estimateR_Ger")

res <- summary(samps)

# RESULTS -----------------------------------------------------------------
round(res$statistics[!(grepl("i\\[",rownames(res$statistics))|
              grepl("frate\\[",rownames(res$statistics))|
              grepl("iload\\[",rownames(res$statistics))|
              grepl("dload\\[",rownames(res$statistics))|
              grepl("d\\[",rownames(res$statistics))|
              grepl("R\\[",rownames(res$statistics))|
              grepl("Ed\\[",rownames(res$statistics))),],2)

res$statistics[(grepl("sd_frate",rownames(res$statistics))),]
frate_sd_max
frate_sd_min

R <- extract_mean(res,"frate\\[")
ggplot(R,aes(x=t,col=factor(country)))+
  geom_ribbon(alpha=.3, aes(ymin=min,ymax=max),col="grey")+
  geom_line(aes(y=frate))+
  geom_vline(data = data.frame(t=T0,country=colnames(d)),aes(xintercept=t,color=country))+
  geom_vline(data = data.frame(t=Ti,country=colnames(d)),aes(xintercept=t,color=country))+
  geom_hline(yintercept=frate_min)+geom_hline(yintercept=frate_max)+
  facet_wrap(vars(country))

R <- extract_mean(res,"R\\[")
ggplot(R,aes(x=t,col=factor(country)))+
  geom_ribbon(alpha=.3,aes(ymin=min,ymax=max),col="grey")+
  geom_line(aes(y=R))+
  geom_vline(data = data.frame(t=T0,country=colnames(d)),aes(xintercept=t,color=country))+
  geom_vline(data = data.frame(t=Ti,country=colnames(d)),aes(xintercept=t,color=country))+
  geom_hline(yintercept=1)+
  geom_hline(yintercept=0)+
  facet_wrap(vars(country),ncol = 3)


R <- extract_mean(res,"R\\[")
ggplot(R,aes(x=date,col=factor(country)))+
  geom_ribbon(alpha=.3, aes(ymin=min,ymax=max),col="grey")+
  geom_line(aes(y=R))+
  geom_vline(data = treatments%>%filter(country %in% unique(R$country)|
                                          region %in% unique(R$country),
                                        type %in% c("SchoolClosings",
                                                    "CurfewIMildOnlyPrivatePublicLifeI",
                                                    "CurfewILockdownofAllNonEssentialPublicLifeI")),
             aes(xintercept=start))+
  geom_hline(yintercept=1)+
  geom_hline(yintercept=0)+
  facet_wrap(vars(country),ncol = 3)





R <- extract_mean(res,"i\\[")
ggplot(R,aes(x=t,col=factor(country)))+
  geom_ribbon(alpha=.3, aes(ymin=min,ymax=max),col="grey")+
  geom_line(aes(y=i))+ scale_y_log10() + annotation_logticks()+
  geom_vline(data = data.frame(t=T0,country=colnames(d)),aes(xintercept=t,color=country))+
  geom_vline(data = data.frame(t=Ti,country=colnames(d)),aes(xintercept=t,color=country))+
  facet_wrap(vars(country),ncol = 3, scales = "free_y")



R <- extract_mean(res,"iload\\[")
ggplot(R,aes(x=date,col=factor(country)))+
  geom_ribbon(alpha=.3, aes(ymin=min,ymax=max),col="grey")+
  geom_line(aes(y=iload))+
  facet_wrap(vars(country),ncol = 3,scales="free_y")

R <- extract_mean(res,"Ed\\[")
ggplot(R,aes(x=date,col=factor(country)))+
  geom_ribbon(alpha=.3, aes(ymin=min,ymax=max),col="grey")+
  geom_line(aes(y=Ed))+ scale_y_log10() + annotation_logticks()+
  geom_vline(data = data.frame(t=T0,country=colnames(d)),aes(xintercept=t,color=country))+
  geom_vline(data = data.frame(t=Ti,country=colnames(d)),aes(xintercept=t,color=country))+
  facet_wrap(vars(country),ncol = 3)



ggplot(data,aes(x=date,col=factor(name)))+
  geom_line(aes(y=dead.new,linetype=dead.total>dead_limit))+
  scale_y_log10() + annotation_logticks()+
  facet_wrap(vars(name),ncol = 3)

ggplot(data.frame(t=as.vector(as.matrix(samps[,"transmission_dist",]))),
       aes(x=abs(t)))+geom_density(fill="grey")+xlab("days since infection")+theme_minimal()


ggplot(data.frame(t=as.vector(as.matrix(samps[,"SId_dist",]))),
       aes(x=abs(t)))+geom_density(fill="grey")+xlab("days since infection")+theme_minimal()


df <- coda::as.array.mcmc.list(samps)
GGally::ggpairs(data.frame(df[,c("transmission_dist","SId_dist"),1]))
GGally::ggpairs(data.frame(df[,c("frate[51,1]","tau[1]"),1]))

GGally::ggpairs(data.frame(df[,c("sd_frate",
                                 "R[60,1]",
                                 "R[60,2]",
                                 "R[60,3]"),1]))

GGally::ggpairs(data.frame(df[,c("frate[60,1]",
                                 "R[60,1]",
                                 "R[61,1]",
                                 "R[70,1]",
                                 "R[80,1]"),1]))



# MCMC diagnostics --------------------------------------------------------

