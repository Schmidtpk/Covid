library(tidyr)
library(ggplot2)
library(rjags)
library(coda)
library(Covid)
library(MCMCvis)
library(runjags)

rm(list = ls())
# DATA --------------------------------------------------------------------


# +international ----------------------------------------------------------
data = all %>% filter(name%in%c(  "New York",
                                  "UK",
                                  "Germany",
                                  "Iran",
                                  "Italy",
                                  "France",
                                  "Spain",
                                  "Netherlands",
                                  "Belgium",
                                  "Sweden",
                                  "Switzerland"
                                   ))
dead_limit <- 100
tau_max <- 400
measurement_error_factor<- 1

# +Germany ----------------------------------------------------------
# data <- all %>% filter(country%in%c("Germany"),
#                       adm_level==1)
# dead_limit <- 10
# tau_max <- 100
# measurement_error_factor <- .1

# expand to include missing values
data <- data %>% expand(name,date) %>% left_join(data)

# outcome na to zero --------------------------------------------------------------
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



# generate array of treatments --------------------------------------------
data <- data  %>% arrange(name,date)
tt_data <- data %>% select(tt_CurfewIMildOnlyPrivatePublicLifeI,
                           tt_CurfewILockdownofAllNonEssentialPublicLifeI,
                           tt_SchoolClosings,
                           tt_BorderClosing
                           )%>%ungroup() %>% as.data.frame()
# fill expanded with FALSE
tt_data[is.na(tt_data)] <- F


#D is time x treatment x country
D <- simplify2array(by(tt_data, as.factor(data$name), as.matrix))
dim(D)


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
            "D"= D,
            "transmission_T"=transmission_T,
            "tau_max"=tau_max,
            "measurement_error_factor"=measurement_error_factor,
            "Tinit"=Tinit,
            "Ti"=Ti)





# JAGS ----------------------------------------------------------------
jags.m <- jags.model( file = "./jags models/stan treatment effect.R",
                      data=dat,
                      #inits=init_fun,
                      n.chains=4,
                      n.adapt=1000)


samps <- coda.samples(jags.m, c( "i",
                                 "iload",
                                 "dload",
                                 "time_effect",
                                 "Ed",
                                 "dead",
                                  "R",
                                 "R0",
                                  "beta",
                                  "tau",
                                  "mean_SId",
                                  "sd_SId",
                                  "mean_transmission",
                                  "sd_transmission",
                                  "frate",
                                 "sd_frate",
                                  "transmission_dist",
                                 "transmission",
                                  "SId_dist"),
                       thin = 2, n.iter=2000)




res <- summary(samps)$statistics
# save(res,file = "./jags models/save/TE_nations")
# load(file = "./jags models/save/estimateR_Ger")

# RESULTS -----------------------------------------------------------------
round(res[!(grepl("i\\[",rownames(res))|
              grepl("frate\\[",rownames(res))|
              grepl("transmission\\[",rownames(res))|
              grepl("iload\\[",rownames(res))|
              grepl("dload\\[",rownames(res))|
              grepl("time_effect\\[",rownames(res))|
              grepl("d\\[",rownames(res))|
              grepl("R\\[",rownames(res))|
              grepl("Ed\\[",rownames(res))),],2)

#round(res[(grepl("i\\[",rownames(res))),],2)


round(res[(grepl("time_effect",rownames(res))),],2)
round(res[(grepl("beta",rownames(res))),],2)
round(res[(grepl("R0",rownames(res))),],2)


R <- extract_mean(res,"R\\[")
ggplot(R,aes(x=t,col=factor(country)))+
  geom_errorbar(aes(ymin=R-2*sd,ymax=R+2*sd),col="grey")+
  geom_line(aes(y=R))+
  geom_vline(data = data.frame(t=T0,country=colnames(d)),aes(xintercept=t,color=country))+
  geom_vline(data = data.frame(t=Ti,country=colnames(d)),aes(xintercept=t,color=country))+
  geom_hline(yintercept=1)+
  facet_grid(country~1)


R <- extract_mean(res,"R\\[")
ggplot(R,aes(x=date,col=factor(country)))+
  geom_errorbar(aes(ymin=R-2*sd,ymax=R+2*sd),col="grey")+
  geom_line(aes(y=R))+
  geom_vline(data = treatments%>%filter(country %in% unique(data$country),
                                        type %in% c("SchoolClosings",
                                                    "CurfewIMildOnlyPrivatePublicLifeI",
                                                    "CurfewILockdownofAllNonEssentialPublicLifeI")),
             aes(xintercept=start))+
  geom_hline(yintercept=1)+
  facet_grid(country~1)+
  coord_cartesian(ylim = c(0,3))



R <- extract_mean(res,"i\\[")
ggplot(R,aes(x=t,col=factor(country)))+
  geom_errorbar(aes(ymin=i-2*sd,ymax=i+2*sd),col="grey")+
  geom_line(aes(y=i))+
  #scale_y_log10() + annotation_logticks()+
  geom_vline(data = data.frame(t=T0,country=colnames(d)),aes(xintercept=t,color=country))+
  geom_vline(data = data.frame(t=Ti,country=colnames(d)),aes(xintercept=t,color=country))+
  facet_grid(country~1,scales = "free_y")


R <- extract_mean(res,"iload\\[")
ggplot(R,aes(x=date,col=factor(country)))+
  geom_errorbar(aes(ymin=iload-2*sd,ymax=iload+2*sd),col="grey")+
  scale_y_log10() + annotation_logticks()+
  geom_line(aes(y=iload))

R <- extract_mean(res,"frate\\[")
ggplot(R,aes(x=t,col=factor(country)))+
  geom_errorbar(aes(ymin=frate-2*sd,ymax=frate+2*sd),col="grey")+
  geom_line(aes(y=frate))+
  geom_vline(data = data.frame(t=T0,country=colnames(d)),aes(xintercept=t,color=country))+
  geom_vline(data = data.frame(t=Ti,country=colnames(d)),aes(xintercept=t,color=country))+
  geom_hline(yintercept=0.01)+geom_hline(yintercept=0.05)+
  facet_grid(country~1)


R <- extract_mean(res,"Ed\\[")
ggplot(R,aes(x=date,col=factor(country)))+
  geom_errorbar(aes(ymin=Ed-2*sd,ymax=Ed+2*sd),col="grey")+
  geom_line(aes(y=Ed))+ scale_y_log10() + annotation_logticks()+
  geom_vline(data = data.frame(t=T0,country=colnames(d)),aes(xintercept=t,color=country))+
  geom_vline(data = data.frame(t=Ti,country=colnames(d)),aes(xintercept=t,color=country))+
  facet_grid(country~1)


R <- extract_mean(res,"dload\\[")
ggplot(R,aes(x=date,col=factor(country)))+
  geom_errorbar(aes(ymin=dload-2*sd,ymax=dload+2*sd),col="grey")+
  geom_line(aes(y=dload))+
  # scale_y_log10() + annotation_logticks()+
  geom_vline(data = data.frame(t=T0,country=colnames(d)),aes(xintercept=t,color=country))+
  geom_vline(data = data.frame(t=Ti,country=colnames(d)),aes(xintercept=t,color=country))+
  facet_grid(country~1)

ggplot(data,aes(x=date,col=factor(name)))+
  geom_line(aes(y=dead.new,linetype=dead.total>dead_limit))+
  scale_y_log10() + annotation_logticks()+
    facet_grid(name~1,scales="free_y")

ggplot(data.frame(t=as.vector(as.matrix(samps[,"transmission_dist",]))),
       aes(x=abs(t)))+geom_density(fill="grey")+xlab("days since infection")+theme_minimal()
#
#
# ggplot(data.frame(t=as.vector(as.matrix(samps[,"SId_dist",]))),
#        aes(x=abs(t)))+geom_density(fill="grey")+xlab("days since infection")+theme_minimal()


as.vector(round(res[(grepl("transmission\\[",rownames(res))),1],2))


df <- coda::as.array.mcmc.list(samps)
GGally::ggpairs(data.frame(df[,c("transmission_dist","SId_dist"),1]))
GGally::ggpairs(data.frame(df[,c("frate[60,1]","tau[1]"),1]))


GGally::ggpairs(data.frame(df[,c("sd_frate","R[60,1]","R[60,2]"),1]))



TE <- as.data.frame(res[(grepl("beta",rownames(res))),])

TE$treatment <- as.numeric(gsub(".*?([0-9]+).*$", "\\1",
                                stringr::str_split(simplify = TRUE,rownames(TE),",")[,1]))

TE$country <- as.numeric(gsub("([0-9]+).*$", "\\1",
                                stringr::str_split(simplify = TRUE,rownames(TE),",")[,2]))
ls(TE)
ggplot(TE,aes(x=country, y=Mean))+
  geom_point()+
  geom_errorbar(aes(ymin=Mean-2*SD,ymax=Mean+2*SD))+
  facet_grid(1~treatment)+coord_cartesian(ylim=c(0,1))
