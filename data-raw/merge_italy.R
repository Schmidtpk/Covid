rm(list=c("measures","italy"))
library(Covid)

# subset to relevant measures
measures <- measures %>% filter(Country=="Italy")

# define new columns ------------------------------------------

# extract treatments
treatments <- unique(measures$Type)
#td: formatting?

# take italy dataset
it <- italy


# define dummies for treatments (true if treatment currently active)
it[,paste0(treatments,"_active")] <- FALSE

# define variable for saving time of first enactement
it[,paste0(treatments,"_t")] <- NA

# define variable for difference to first enactement
it[,paste0(treatments,"_diff")] <- NA

# for comparison necessary
it$region <- as.character(it$region)

# loop over all measures and safe if active and safe first enactement date
# sort before by ADM1 (region overwrites nation) when on same date
measures <- measures %>% arrange(desc(ADM1))
for (i in 1:nrow(measures))
{

  if(measures$Type[i]!="")
  {
    # right region or nationwide
    correct.i <- (it$region==measures$ADM1[i] | measures$ADM1[i]=="")

    # after starting date
    correct.t <- it$Date >= measures$Start[i] &
      # if end date exists, before end date
      (if(is.na(measures$End[i])) TRUE else (it$Date < measures$End[i]))

    # check if each measure has fit
    if(sum(correct.i)==0)
    {

      correct.i <- grepl(substr(measures$ADM1[i],2,5),it$region)  | measures$ADM1[i]==""
      if(sum(correct.i)!=0)
      {
        warning(paste("region-country",measures$ADM1[i],measures$Country[i],"has only fit after loosening matching"))
      } else {
        warning(paste("region-country",measures$ADM1[i],measures$Country[i],"has no fit"))
      }

      if(length(unique(it$region[correct.i]))>1)
        warning(paste("Multiple regions were assigned one measure with mild matching"))
    } else if(sum( correct.i & correct.t)==0)
    {
      warning(paste("mesaure",measures$Type[i],"in",measures$ADM1[i],measures$Country[i]," at date", measures$Start[i],"has no fit"))
    }

    # for each measure find dates and regions that currently enact it
    it[
      correct.t & correct.i,
      paste0(measures$Type[i],"_active")
    ] <- TRUE

    # for each measure save enacting date
    it[
        correct.i,
        paste0(measures$Type[i],"_t")
      ] <- measures$Start[i]

    # for each measure save difference to enacting date
    it[
      correct.i,
      paste0(measures$Type[i],"_diff")
      ] <-
      it$Date[correct.i] - measures$Start[i]
  }
}

subset(it, region=="Lombardia",
       select = c("Date",
                  "UniversityClosings_active",
                  "SchoolClosings_active",
                  "SchoolClosings_diff",
                  "SchoolClosings_t"))

use_data(it,overwrite = TRUE)
it.measures <- treatments
use_data(it.measures,overwrite = T)
