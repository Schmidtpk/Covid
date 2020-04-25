library(tidyr)
library(rjags)
library(coda)
library(Covid)
library(MCMCvis)
library(runjags)
library(mcmcplots)
library(ggmcmc)
library(lubridate)

rm(list = ls())
# +++ DATA +++ --------------------------------------------------------------------


# Priors ------------------------------------------------------------------

mean_SId_min <- 20
mean_SId_max <- 30

beta_time_spread <- 0.5

Rmax<-4

# +international ----------------------------------------------------------
data = all %>% filter(name%in%c(  "New York",
                                  "UK",
                                  "Germany",
                                  #"Iran",
                                  "Italy",
                                  "Japan",
                                  "Spain",
                                  "Netherlands",
                                  "Belgium",
                                  #"Sweden",
                                  "Switzerland",
                                  "Louisiana",
                                   "Massachusetts",
                                  "Michigan",
                                   "New Jersey"
                                   ))
dead_limit <- 10
tau_max <- 400
measurement_error_factor<- .1
frate_min <- 0.001
frate_max <- 0.01
#frate_sd_min <- 10^(-4)
#frate_sd_max <- 2*10^(-4)
frate_sd_min <- 10^(-6)
frate_sd_max <- 2*10^(-6)


# +Germany ----------------------------------------------------------
# data <- all %>% filter(country%in%c("Germany"),
#                       adm_level==1)
# #data <- data %>% filter(region %in% c("Bayern","Hessen","Berlin"))
#
# dead_limit <- 10
# tau_max <- 200
# measurement_error_factor <- .1
# frate_min <- 0.0001
# frate_max <- 0.005
# # frate_sd_min <- 10^(-4)
# # frate_sd_max <- 5*10^(-4)
# frate_sd_min <- 0
# frate_sd_max <- 10^(-6)



# Choose weather variables ------------------------------------------------

weather_vars <- ls(weather_cams)[-c(1,3,4,5,6)]


# na to zero --------------------------------------------------------------
data <- data %>%
  mutate(
         dead.new = ifelse(is.na(dead.new),0,dead.new),
         pos.new = ifelse(is.na(pos.new),0,pos.new),
         pos.total = ifelse(is.na(pos.total),0,pos.total),
         in_model = dead.total > dead_limit)


# drop recent past --------------------------------------------------------
data <- data %>% filter(date< today()-as.difftime(10,units = "days"))


# drop incomplete weather data --------------------------------------------



# drop names with insufficent observations --------------------------------
data <- data %>% group_by(name) %>%
  mutate(
    sum_in_model = sum(in_model, na.rm = T)
  ) %>% filter(sum_in_model >5) %>% ungroup()


# generate d and D --------------------------------------------------------

### expand data
data <- complete(data, date,name)

### dead d

data <- data  %>% arrange(name,date)

d<- data%>%
  pivot_wider(c(name,date),names_from = name,values_from = dead.new)%>%
  select(-c(date))

#d is time*country
dim(d)


### weather D

tt_data <- data %>%
  select(weather_vars)%>%
  ungroup() %>% as.data.frame()

tail(tt_data)
if(nrow(data %>% filter(is.na(wind), date > "2020-02-01"))>1)
  warning("Recent NAs in weather data")

factors <- apply(tt_data,2,
                 function(x) as.factor(factorize(x,4)))

#head(factors)
options(na.action='na.pass')
dummy_matrix <- model.matrix(~.,data = data.frame(factors))[,-c(1)]
#tail(dummy_matrix)



#D is time x treatment x country
by.cur <- by(dummy_matrix, as.factor(data$name), as.matrix)
D <- simplify2array(by.cur)
dim(D)
tail(D[,1,])
head(D[80,,])

if(mean(is.na(D))>0){
  warning("NA in D")
  #D[is.na(D)]<-0
  }

if(sum(d<0,na.rm = T)>0){
  warning("negative in d")
  d[d<0]<-0
  #View(data%>%filter(dead.new <0)%>%select(dead.new,date,name))
}

if(mean(is.na(d))>0)
  warning("NA in d")

# define time intervals ---------------------------------------------------
T <- data %>% group_by(name) %>% summarize(length(dead.new))%>%pull()
sdat <- data %>% group_by(name,in_model) %>% mutate(count=row_number()) %>% filter(count==1) %>%select(-contains("tt"))
T0 <- sdat %>% group_by(name) %>% summarise(max(date)-min(date)) %>%pull()%>%as.numeric()


Tinit <- numeric(ncol(d))
Ti <- numeric(ncol(d))
for(c in 1:ncol(d)){
  Tinit[c]<- T0[c]-27
  Ti[c] <- Tinit[c]+7
}

names(T0)<-colnames(d)
names(Ti)<-colnames(d)
names(Tinit)<-colnames(d)


# data list ---------------------------------------------------------------
dat <- list("T" = T,
            "T0"=T0,
            "dead" = d,
            "transmission_T"=30,
            "tau_max"=tau_max,
            #"Rmax"= Rmax,
            "D"= D,
            #"measurement_error_factor"=measurement_error_factor,
            "mean_SId_min"=mean_SId_min,
            "mean_SId_max"=mean_SId_max,
            "Tinit"=Tinit,
            "Ti"=Ti,
            #"T_pred"=25,
            #"beta_time_spread" = beta_time_spread,
            "frate_min"=frate_min,
            "frate_max"=frate_max,
            "frate_sd_min"=frate_sd_min,
            "frate_sd_max"=frate_sd_max)





# +++ JAGS +++ ----------------------------------------------------------------


# initial conditions ------------------------------------------------------

init_fun <- function(){

  list.cur <- list(
    "tau" = runif(ncol(d),0,tau_max),
    "mean_SId" = runif(n = 1, mean_SId_min,mean_SId_max),
    "sd_SId" = runif(n = 1,2,5)
    #"R_pred" = runif(ncol(d), 0,2),
    #"beta_time" = runif(ncol(d), -1*beta_time_spread,1*beta_time_spread)
  )

  # add values with country dependent length
  for(c.cur in 1:ncol(d)){
    for(t.cur in Ti[c.cur]:T[c.cur]){
      #list.cur[[paste0("R[",t.cur,",",c.cur,"]")]] <- runif(n = 1,0,Rmax)
    }
  }

  list.cur
}

# model -------------------------------------------------------------------
jags.m <- jags.model( file = "./jags models/weather/stan weather.R",
                      data=dat,
                      inits=init_fun,
                      n.chains=4,
                      n.adapt=20)


samps <- coda.samples(jags.m, c( "i",
                                 "iload",
                                 "beta",
                                 "effect",
                                 "Ed",
                                 "dead",
                                  "R",
                                 "R0",
                                 "Rtime",
                                 "R_pred",
                                  "tau",
                                  "mean_SId",
                                  "sd_SId",
                                  "mean_transmission",
                                  "frate",
                                 "sd_frate",
                                  "transmission_dist",
                                 #"transmission",
                                  "SId_dist"),
                       thin = 4, n.iter=100)


# save(samps,file = "./jags models/save/weather_Ger")
 save(samps,file = "./jags models/save/weather_international")
# load(file = "./jags models/save/weather_international")
# load(file = "./jags models/save/weather_Ger")

res <- summary(samps)

# +++ RESULTS +++ -----------------------------------------------------------------
round(res$statistics[
  grepl("SId_dist\\[",rownames(res$statistics))|
  grepl("mean_",rownames(res$statistics))|
  grepl("sd_",rownames(res$statistics))|
  grepl("dist",rownames(res$statistics)),],1)

res$statistics[(grepl("tau",rownames(res$statistics))),]

effects <- res$statistics[(grepl("beta",rownames(res$statistics))),]
rownames(effects) <- dimnames(D)[2][[1]]
effects

res$quantiles[(grepl("R0",rownames(res$quantiles))),]
res$quantiles[(grepl("Rtime",rownames(res$quantiles))),]


dat <- data.frame(res$quantiles[(grepl("Rtime",rownames(res$quantiles))),])
colnames(dat)<-substr(colnames(dat),2,nchar(colnames(dat))-1)
dat$t <- readr::parse_number(rownames(dat))

ggplot(pivot_longer(dat, cols = -c(t)),
  aes(x=t,y=value,col=factor(name)))+geom_line()+ylab("Rtime")



res$statistics[(grepl("sd_frate",rownames(res$statistics))),]
frate_sd_max
frate_sd_min

res$statistics[(grepl("beta_time",rownames(res$statistics))),]
res$quantiles[(grepl("R_pred",rownames(res$quantiles))),]



# time plots --------------------------------------------------------------

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
ggplot(R,aes(x=t,col=factor(country)))+
  geom_ribbon(alpha=.3,aes(ymin=min,ymax=max),col="grey")+
  geom_line(aes(y=R))+
  geom_vline(data = data.frame(t=T0,country=colnames(d)),aes(xintercept=t,color=country))+
  geom_vline(data = data.frame(t=Ti,country=colnames(d)),aes(xintercept=t,color=country))+
  geom_hline(yintercept=1)+
  geom_hline(yintercept=0)+
  facet_wrap(vars(country),ncol = 3)



R <- extract_mean(res,"frate\\[")
ggplot(R,aes(x=t,col=factor(country)))+
  geom_ribbon(alpha=.3, aes(ymin=min,ymax=max),col="grey")+
  geom_line(aes(y=frate))+
  geom_vline(data = data.frame(t=T0,country=colnames(d)),aes(xintercept=t,color=country))+
  geom_vline(data = data.frame(t=Ti,country=colnames(d)),aes(xintercept=t,color=country))+
  geom_hline(yintercept=frate_min)+geom_hline(yintercept=frate_max)+
  facet_wrap(vars(country))


R <- extract_mean(res,"i\\[")
ggplot(R,aes(x=date,col=factor(country)))+
  geom_ribbon(alpha=.3, aes(ymin=min,ymax=max),col="grey")+
  geom_line(aes(y=i))+
  scale_y_log10() + annotation_logticks()+
  geom_vline(data = data.frame(t=T0,country=colnames(d)),aes(xintercept=t,color=country))+
  geom_vline(data = data.frame(t=Ti,country=colnames(d)),aes(xintercept=t,color=country))+
  facet_wrap(vars(country),ncol = 3, scales = "free_y")

ggplot(data,aes(x=date-as.difftime(10, unit="days"),col=factor(name)))+
  geom_line(aes(y=pos.new))+
  #scale_y_log10() + annotation_logticks()+
  facet_wrap(vars(name),ncol = 3,scales="free_y")



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

GGally::ggpairs(data.frame(df[,c("R_pred[2]",
                                 "tau[2]",
                                 "sd_frate",
                                 "mean_transmission"),1]))



# mcmc diagnostics --------------------------------------------------------

class(samps)


traplot(samps,
        parms = c("beta"))

traplot(samps,
        parms = c("R_pred"))

traplot(samps,
        parms = c("R[40,1]"))

traplot(samps,
        parms = c("tau"))

ggs_density(ggs(samps),family = "beta_time")
