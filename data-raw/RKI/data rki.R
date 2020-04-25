library(dplyr)
library(ggplot2)

a <- read.csv("./data-raw/RKI/rki2.csv")


a$Referenzdatum <- as.Date(a$Referenzdatum,"%m/%d/%Y")

a$Meldedatum <- as.Date(a$Meldedatum,"%m/%d/%Y")

range(a$Meldedatum)
range(a$Referenzdatum)


# Referenzdatum
# b <- a %>% filter(Neuer.Todesfall %in% c(0,1)) %>% mutate(
#   diff = Meldedatum-Referenzdatum
# ) %>% select(diff,Meldedatum)
# ggplot(b%>%filter(diff>1),aes(x=Meldedatum,y=diff))+
#   geom_count()+
#   geom_smooth()


# Bundesland --------------------------------------------------------------

df <- a %>% group_by(Bundesland, Meldedatum) %>%
  summarise(
    dead.new = sum(AnzahlTodesfall[Neuer.Todesfall%in% c(0,1)], na.rm = T),
    pos.new = sum(AnzahlFall[Neuer.Fall%in% c(0,1)], na.rm = T)
  ) %>% ungroup()

df <- df %>% group_by(Bundesland) %>% arrange(Meldedatum, .by_group=TRUE) %>%
  mutate(
  dead.total = cumsum(dead.new),
  pos.total = cumsum(pos.new)
) %>% ungroup()

rki <- df

rki <- rki %>% rename(
  date = Meldedatum,
  region= Bundesland
)

use_data(rki, overwrite = TRUE)
#
# ggplot(rki,aes(x=date,y=dead.total,col=region))+geom_line()
#
# ggplot(Covid::all %>% filter(country=="Germany",adm_level==1),aes(x=date,y=dead.total,col=region))+geom_line()


# landkreis ---------------------------------------------------------------



df <- a %>% group_by(Landkreis, Meldedatum) %>%
  summarise(
    dead.new = sum(AnzahlTodesfall[Neuer.Todesfall%in% c(0,1)], na.rm = T),
    pos.new = sum(AnzahlFall[Neuer.Fall%in% c(0,1)], na.rm = T)
  ) %>% ungroup()

df <- df %>% group_by(Landkreis) %>% arrange(Meldedatum, .by_group=TRUE) %>%
  mutate(
    dead.total = cumsum(dead.new),
    pos.total = cumsum(pos.new)
  ) %>% ungroup()

rki2 <- df

rki2 <- rki2 %>% rename(
  date = Meldedatum,
  region= Landkreis
)

use_data(rki2,overwrite = TRUE)
