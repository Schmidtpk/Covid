

# subset to relevant measures
measures <- subset(measures,country=="Italy")

# define new columns ------------------------------------------

# extract treatments
treatments <- unique(measures$type)
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
# sort before by region.code (region overwrites nation) when on same date
measures <- measures %>% arrange(desc(region.code))
for (i in 1:nrow(measures))
{

  if(measures$type[i]!="")
  {
    # right region or nationwide
    correct.i <- (it$region==measures$region[i] | is.na(measures$region[i]))

    # after starting date
    correct.t <- it$Date >= measures$start[i] &
      # if end date exists, before end date
      (if(is.na(measures$end[i])) TRUE else (it$Date <= measures$end[i]))



    # for each measure find dates and regions that currently enact it
    it[
      correct.t & correct.i,
      paste0(measures$type[i],"_active")
    ] <- TRUE

    # for each measure save enacting date
    it[
        correct.i,
        paste0(measures$type[i],"_t")
      ] <- measures$start[i]

    # for each measure save difference to enacting date
    it[
      correct.i,
      paste0(measures$type[i],"_diff")
      ] <-
      it$Date[correct.i] - measures$start[i]
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
