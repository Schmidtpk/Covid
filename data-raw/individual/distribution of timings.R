library(tidyverse)
library(lubridate)

df <- read_csv("./data-raw/individual/brewer.txt")
df

df <- df %>% mutate(
  date_symptoms = as.Date(date_onset_symptoms,"%d.%m.%y"),
  date_confirmation = as.Date(date_confirmation,"%d.%m.%y"),
  date_death = as.Date(date_death_or_discharge,"%d.%m.%y"),
  date_hospital = as.Date(date_admission_hospital,"%d.%m.%y"),
  diff_dh = date_death-date_hospital,
  diff_dc = date_death-date_confirmation,
  diff_ds = date_death-date_symptoms,
  diff_sh = date_hospital-date_symptoms,
  diff_ds = if_else(diff_ds<(-100),diff_ds+356,diff_ds),#if too far past, assume that year false entered in symptoms date
  diff_dc = if_else(diff_dc<(-100),diff_dc+356,diff_dc),
  diff_dh = if_else(diff_dh<(-100),diff_dh+356,diff_dh),
  diff_sh = if_else(diff_sh<(-100),diff_sh+356,diff_sh),
)



ss <- df %>% filter(outcome%in%c("death","died","dead"))

ggplot(ss,aes(x=diff_dc))+geom_histogram()
ggplot(ss,aes(x=diff_ds))+geom_histogram()
ggplot(ss,aes(x=diff_dh))+geom_histogram()
ggplot(ss,aes(x=diff_sh))+geom_histogram()


# other data set ----------------------------------------------------------

df <- read_csv("./data-raw/individual/kaggle_line_list_data.csv")
df
ls(df)
df <- df %>% mutate(
  date_symptoms = as.Date(symptom_onset, "%m/%d/%y"),
  date_confirmation = as.Date(`reporting date`,"%m/%d/%y"),
  date_death = as.Date(death,"%m/%d/%y"),
  date_hospital = as.Date(hosp_visit_date,"%m/%d/%y"),
  diff_dh = date_death-date_hospital,
  diff_dc = date_death-date_confirmation,
  diff_ds = date_death-date_symptoms,
  diff_sh = date_hospital-date_symptoms,
  diff_ds = if_else(diff_ds<(-100),diff_ds+356,diff_ds),#if too far past, assume that year false entered in symptoms date
  diff_dc = if_else(diff_dc<(-100),diff_dc+356,diff_dc),
  diff_dh = if_else(diff_dh<(-100),diff_dh+356,diff_dh),
  diff_sh = if_else(diff_sh<(-100),diff_sh+356,diff_sh),
)



ss <- df %>% filter(!is.na(date_death))

ggplot(ss,aes(x=diff_dc))+geom_histogram()
ggplot(ss,aes(x=diff_ds))+geom_histogram()
ggplot(ss,aes(x=diff_dh))+geom_histogram()
ggplot(ss,aes(x=diff_sh))+geom_histogram()
