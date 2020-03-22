#Time-varying estimates of reproduction numbers were made with a
#7-day sliding window using the R
#ackage EpiEstim, accounting for imported cases and assuming a serial interval of 4.7
#days and a standard deviation of 2.9 days.
library(EpiEstim)
library(Covid)

#define length of interval for R estimation
l.interval <- 1


lombardia <- it %>% filter(region=="Lombardia")

incidence <- lombardia$infected

warning(paste("drop",sum(is.na(incidence)), " NA observations"))
incidence <- incidence[!is.na(incidence)]
t_start <- seq(2, length(incidence)-l.interval)
t_end <- t_start + l.interval

res <- estimate_R(
  incid = incidence,
  method = "parametric_si",
  config = make_config(list(
    t_start = t_start, t_end = t_end,
    mean_si = 2.9, std_si = 4.7
    ))
  )

plot(res)

head(res$R)

for.merge <- res$R[,c("Mean(R)","Std(R)","t_start","t_end")]
colnames(for.merge) <- c("R","sd.R","t1","t2")
for.merge$t <-ceiling(rowMeans(for.merge[,c("t1","t2")]))

lombardia$t
for.merge$t<-as.factor(for.merge$t)



lombardia <- full_join(lombardia,for.merge, by = "t")

lombardia$R

ggplot(lombardia, aes(x=Date))+
  geom_line(aes(y=R))+
  geom_point(aes(y=growth))

ggplot(lombardia)+
  geom_point(aes(y=growth,x=R))

measures.cur <- measures %>%
  filter(ADM1=="Lombardy" | (Country=="Italy" & ADM1==""))

measures.cur <- measures.cur[!duplicated(measures.cur %>% select(Type)),]


ggplot(lombardia)+
  geom_point(aes(x=Date,y=R))+
  geom_line(aes(x=Date,y=R))+
  #geom_line(aes(x=Date,y=growth),col="grey")+
  geom_hline(yintercept = 1)+
  geom_vline(data=subset(lombardia, weekdays(Date)== "Sonntag"), aes(xintercept=Date),color="blue",size=10,alpha=.2)+
  geom_vline(data = measures.cur, aes(xintercept=Start), color ="red")+
  geom_text(data = measures.cur,aes(x=Start+.5,label=Type),
            y=1, angle=90, text=element_text(size=1), color="red")+ylim(c(0,3))


