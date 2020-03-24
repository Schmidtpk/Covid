
#
# df <- world%>%filter(label=="Italy")
#
# measures.cur <- "SchoolClosi"
#
# plot.measure.data <- measures %>% filter(grepl(measures.cur,Type) & Country %in% (df%>%pull(label)))
#
#
# df$outcome <- log(df$dead)
#
# ggplot(df)+
#   geom_point(aes(x=Date,y=outcome))+
#   geom_line(aes(x=Date,y=outcome))+
#   #geom_line(aes(x=Date,y=growth),col="grey")+
#   geom_hline(yintercept = 1)+
#   geom_vline(data=subset(df, weekdays(Date)== "Sonntag"), aes(xintercept=Date),color="blue",size=10,alpha=.2)+
#   geom_vline(data = plot.measure.data, aes(xintercept=Start), color ="red")+
#   geom_text(data = plot.measure.data,aes(x=Start+.5,label=paste(Type,"in",Region)),
#             y=1, angle=90, text=element_text(size=1), color="red")+ylim(c(0,3))
