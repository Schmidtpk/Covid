library(tidyr)
library(rjags)
library(coda)
library(Covid)
library(MCMCvis)
library(runjags)
library(zoo)

data<-NULL
data = all %>% filter(name%in%c(  "New York",
                                  "UK",
                                  "Germany",
                                   "Iran",
                                   "Italy",
                                   "Spain",
                                   "Netherlands",
                                   "Belgium",
                                    "Sweden"
                                   ))


# interpolate -------------------------------------------------------------
data <- data %>% group_by(name) %>%
  arrange(name,date) %>%
  mutate(
    pos.new = ifelse(pos.new==0,NA,pos.new),
    pos.new = floor(na.approx(pos.new, maxgap = 15, rule = 2)))

data <- data %>%
  mutate(
    dead.new = ifelse(is.na(dead.new),0,dead.new),
    pos.new = ifelse(is.na(pos.new),0,pos.new),
    pos.total = ifelse(is.na(pos.total),0,pos.total),
    in_model = pos.new > 50)

data <- data  %>% arrange(name,date)

d<- data%>%
  pivot_wider(c(name,date),names_from = name,values_from = pos.new)%>%
  select(-c(date))

#d <- simplify2array(by(data%>%pull(dead.new), data$name, as.vector))
##d is time*country
dim(d)




T <- data %>% group_by(name) %>% summarize(length(dead.new))%>%pull()
sdat <- data %>% group_by(name,in_model) %>% mutate(count=row_number()) %>% filter(count==1) %>%select(-contains("tt"))
T0 <- sdat %>% group_by(name) %>% summarise(max(date)-min(date)) %>%pull()%>%as.numeric()

transmission_T <- 30

Tinit <- numeric(ncol(d))
Ti <- numeric(ncol(d))
for(c in 1:ncol(d)){
  # define time intervals ---------------------------------------------------
  Tinit[c]<- T0[c]-transmission_T
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
            "Tinit"=Tinit,
            "Ti"=Ti)





# run jags ----------------------------------------------------------------
jags.m <- jags.model( file = "./jags models/estimateR_testing.R",
                      data=dat,
                      #inits=init_fun,
                      n.chains=2
                      ,
                      n.adapt=100)


samps <- coda.samples(jags.m, c( "i",
                                 "iload",
                                 "dload",
                                 "Ed",
                                 "dead",
                                 "pzero",
                                  "R",
                                  "tau",
                                  "mean_SId",
                                  "sd_SId",
                                  "mean_transmission",
                                  #"sd_transmission",
                                  "frate",
                                  "transmission_dist",
                                 "transmission",
                                  "SId_dist"),
                       thin = 4, n.iter=1000)


# save(samps,file = "./jags models/save/testing")
# load(file = "./jags models/save/testing")

res <- summary(samps)$statistics

# results -----------------------------------------------------------------
round(res[!(grepl("i\\[",rownames(res))|
              grepl("iload\\[",rownames(res))|
              grepl("dload\\[",rownames(res))|
              grepl("d\\[",rownames(res))|
              grepl("R\\[",rownames(res))|
              grepl("Ed\\[",rownames(res))),],2)

#round(res[(grepl("i\\[",rownames(res))),],2)


R <- extract_mean(res,"R\\[")
ggplot(R,aes(x=date,col=factor(country)))+
  geom_errorbar(aes(ymin=R-sd,ymax=R+sd),col="grey")+
  geom_line(aes(y=R))+
  geom_vline(data = data.frame(t=T0,country=colnames(d)),aes(xintercept=t,color=country))+
  geom_vline(data = data.frame(t=Ti,country=colnames(d)),aes(xintercept=t,color=country))+
  geom_hline(yintercept=1)+
  facet_grid(country~1)


R <- extract_mean(res,"R\\[")
ggplot(R,aes(x=date,col=factor(country)))+
  geom_errorbar(aes(ymin=R-sd,ymax=R+sd),col="grey")+
  geom_line(aes(y=R))+
  geom_vline(data = treatments,aes(xintercept=start))+
  geom_hline(yintercept=1)+
  facet_grid(country~1)


R <- extract_mean(res,"i\\[")
ggplot(R,aes(x=date,col=factor(country)))+
  geom_errorbar(aes(ymin=i-sd,ymax=i+sd),col="grey")+
  geom_line(aes(y=i))+ scale_y_log10() + annotation_logticks()+
  geom_vline(data = data.frame(t=T0,country=colnames(d)),aes(xintercept=t,color=country))+
  geom_vline(data = data.frame(t=Ti,country=colnames(d)),aes(xintercept=t,color=country))+
  facet_grid(country~1)




R <- extract_mean(res,"iload\\[")
ggplot(R,aes(x=date,col=factor(country)))+
  geom_errorbar(aes(ymin=iload-sd,ymax=iload+sd),col="grey")+
  geom_line(aes(y=iload))

R <- extract_mean(res,"Ed\\[")
ggplot(R,aes(x=date,col=factor(country)))+
  geom_errorbar(aes(ymin=Ed-sd,ymax=Ed+sd),col="grey")+
  geom_line(aes(y=Ed))+ scale_y_log10() + annotation_logticks()+
  geom_vline(data = data.frame(t=T0,country=colnames(d)),aes(xintercept=t,color=country))+
  geom_vline(data = data.frame(t=Ti,country=colnames(d)),aes(xintercept=t,color=country))+
  facet_grid(country~1)


R <- extract_mean(res,"dload\\[")
ggplot(R,aes(x=date,col=factor(country)))+
  geom_errorbar(aes(ymin=dload-sd,ymax=dload+sd),col="grey")+
  geom_line(aes(y=dload))+# scale_y_log10() + annotation_logticks()+
  geom_vline(data = data.frame(t=T0,country=colnames(d)),aes(xintercept=t,color=country))+
  geom_vline(data = data.frame(t=Ti,country=colnames(d)),aes(xintercept=t,color=country))+
  facet_grid(country~1)

R <- extract_mean(res,"dead\\[")
ggplot(R,aes(x=date,col=factor(country)))+
  geom_errorbar(aes(ymin=dead-sd,ymax=dead+sd),col="grey")+
  geom_line(aes(y=dead))+ scale_y_log10() + annotation_logticks()+
  geom_vline(data = data.frame(t=T0,country=colnames(d)),aes(xintercept=t,color=country))+
  geom_vline(data = data.frame(t=Ti,country=colnames(d)),aes(xintercept=t,color=country))+
  facet_grid(country~1)


ggplot(data.frame(t=as.vector(as.matrix(samps[,"transmission_dist",]))),
       aes(x=abs(t)))+geom_density(fill="grey")+xlab("days since infection")+theme_minimal()


ggplot(data.frame(t=as.vector(as.matrix(samps[,"SId_dist",]))),
       aes(x=abs(t)))+geom_density(fill="grey")+xlab("days since infection")+theme_minimal()


as.vector(round(res[(grepl("transmission\\[",rownames(res))),1],2))


df <- coda::as.array.mcmc.list(samps)
GGally::ggpairs(data.frame(df[,c("transmission_dist","SId_dist"),1]))
GGally::ggpairs(data.frame(df[,c("frate[1]","tau[1]"),1]))



