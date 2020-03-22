library(Covid)


ggplot(it %>% filter(Latitude>quantile(Latitude,.65)), aes(x=Date,y=log(infected),color=region))+geom_point()+geom_line()
ggplot(it %>% filter(Latitude>quantile(Latitude,.65)), aes(x=Date,y=pos.total,color=region))+geom_point()+geom_line()


it$growth[is.nan(it$growth)] <- 0
it$growth[!is.finite(it$growth)] <- 0




stargazer(type = "text",
          plm(paste("diff(growth) ~ ", paste(paste0("(",treatments,"_diff==10)"),collapse = "+")),
              it,model = "pooling"))

stargazer(type = "text",
          plm(paste("growth ~ ", paste(paste0("lag(",treatments,"_active,10)"),collapse = "+")),
            it,model = "pooling"))

stargazer(type = "text",
          plm(paste("growth ~ ", paste(paste0("lag(",treatments,"_active,10)"),collapse = "+")),
              it,model = "pooling"))

stargazer(type = "text",
          plm(paste("lead(growth,7) ~ ", paste(paste0(treatments,"_active"),collapse = "+")),
              it,model = "pooling"),
          plm(paste("lead(growth,7) ~ as.numeric(t)+", paste(paste0(treatments,"_active"),collapse = "+")),
              it,model = "pooling"))

stargazer(type = "text",
          plm(paste("lead(growth,7) ~ ", paste(paste0(treatments,"_active"),collapse = "+")),
              it,effect = "individual"),
          plm(paste("lead(growth,7) ~ ", paste(paste0(treatments,"_active"),collapse = "+")),
              it,effect = "time"),
          plm(paste("lead(growth,7) ~ ", paste(paste0(treatments,"_active"),collapse = "+")),
              it,effect = "twoways")
)


summary(plm("growth ~ SchoolClosings_active",
            it,effect = "twoways"))
