library(tidyverse)
library(magrittr)
library(lubridate)
library(stringr)
library(tibble)
library(broom)
library(ggplot2)
library(ggthemes)
library(hrbrthemes)
library(rvest)
library(deSolve)
library(EpiEstim)
library(incidence)
library(distcrete)
library(epitrix)
library(projections)
library(earlyR)

# download the wikipedia web page
# we use a specific version of the template page directly
# version of the wikipedia page that is used by this version of this document
chinese_wikipedia_data_url <- paste("https://en.wikipedia.org/wiki/",
                                    "Template:2019–20_coronavirus_outbreak_data/",
                                    "China_medical_cases_by_province",
                                    sep="")
chinese_outbreak_webpage <- read_html(chinese_wikipedia_data_url)
# parse the web page and extract the data from the first table
chinese_provinces_confirmed <- chinese_outbreak_webpage %>%
  html_nodes("table") %>%
  .[[3]] %>%
  html_table(fill = TRUE, header=FALSE) %>%
  slice(-2) %>%
  slice(1:(nrow(.)-3))
colnames(chinese_provinces_confirmed) <- chinese_provinces_confirmed %>%
  slice(nrow(.)) %>%
  unlist()
chinese_provinces_confirmed <- chinese_provinces_confirmed %>%
  rename(Date="Date (CST)") %>%
  select(-contains("sources", ignore.case = TRUE))
# fix up the column names, get rid of footnotes and other non-data
# and convert columns to appropriate data types.
excl_hubei <- chinese_provinces_confirmed %>% select(starts_with("ExcludingHubei")) %>% pull(1)
hubei_sans_wuhan <- chinese_provinces_confirmed %>%
  select(starts_with("Hubei:outside Wuhan")) %>%
  select(-contains("clinical", ignore.case = TRUE)) %>%
  pull(1)
# utility function to remove commas in numbers as character strings
rm_commas <- function(x) gsub(",", "", x)
rm_refs <- function(x) stringr::str_split(x, "\\[", simplify=TRUE)[,1]
chinese_provinces_confirmed <- chinese_provinces_confirmed %>%
  select(-starts_with("ExcludingHubei")) %>%
  select(-starts_with("Hubei:outside Wuhan")) %>%
  rename(Wuhan="Wuhan,Hubei",
         National="National(confirmed)") %>%
  mutate(NationalSansHubei=excl_hubei,
         HubeiSansWuhan=hubei_sans_wuhan) %>%
  select(-contains("clinical", ignore.case = TRUE)) %>%
  select(-contains("inclusive", ignore.case = TRUE)) %>%
  mutate(Date=ymd(Date)) %>%
  mutate_if(is.character, rm_commas) %>%
  mutate_if(is.character, rm_refs) %>%
  mutate_if(is.character, as.integer)
chinese_provinces_confirmed_wp_totals <- chinese_provinces_confirmed %>%
  filter(is.na(Date)) %>%
  pivot_longer(-Date, names_to="province",
               values_to="wp_total") %>%
  select(-Date)
chinese_provinces_confirmed <- chinese_provinces_confirmed %>%
  filter(!is.na(Date))
chinese_provinces_confirmed_calc_totals <- chinese_provinces_confirmed %>%
  pivot_longer(-Date, names_to="province",
               values_to="incident_cases") %>%
  group_by(province) %>%
  summarise(calc_total=sum(incident_cases, na.rm=TRUE)) %>%
  ungroup()

chinese_totals_check <- chinese_provinces_confirmed_calc_totals %>%
  left_join(chinese_provinces_confirmed_wp_totals,
            by="province")
# work out the order for the columns from the data, descending order
chinese_provinces_confirmed %>%
  pivot_longer(-Date, names_to="province",
               values_to="incident_cases") %>%
  group_by(province) %>%
  summarise(total=sum(incident_cases, na.rm=TRUE)) %>%
  arrange(desc(total)) %>%
  pull(province) -> chinese_province_order
chinese_province_labels <- chinese_province_order %>%
  str_replace("Sans", " without ") %>%
  str_replace("InnerMongolia", "Inner Mongolia") %>%
  str_replace("National", "Mainland China")
# re-arrange the columns in the dataset and fill in some
# missing values, but not all, with zeroes.
# Also, lab-confirmed and clinical counts for Wuhan are combined on the
# source Hubei health Commission from 15 Feb so
# set to NA since we cannot split out the lab-confirmed only
chinese_provinces_confirmed <- chinese_provinces_confirmed %>%
  select(c('Date',chinese_province_order)) %>%
  arrange(Date) %>%
  mutate(National=ifelse(is.na(National), 0, National),
         Hubei=ifelse(is.na(Hubei), 0, Hubei),
         Wuhan=ifelse(is.na(Wuhan), 0, Wuhan))
# re#chinese_provinces_confirmed %>%
#    pivot_longer(-Date, names_to="province",
#                 values_to="incident_cases")
