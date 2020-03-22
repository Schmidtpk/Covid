#   Data roadmap
- each folder contains different data sources
- package data sets are created in those folders
- subsequently treatments/measures are merged into the data sets in merge_italy/merge_world
- Additionally, a large data set is created containing all regional units in the different data sets

# measures/create measures.R
- downloads the google document with measures and does some formating
- saves data as "measures" in package

# /kaggle italy/make_data_italy
- creates data set "italy" containing observations by region and day

# novel-corona-virus-2019-dataset/make_data_world
- creates data set "world" containing observations by nation and day

# merge_italy.R
- takes the italy data generated in kaggle italy and merges the measures to it
- generates for each treatment
  - column "treatmeant_active": positive, when treatment is active
  - column "treatmeant_t": stating date of first enactement
  - column "treatment_diff": specifiying time to/since first enactement (before negative, afterwards positive)
- new data safed as "it" in package

# merge_world.R (todo)
- takes the international data and merges the measures to it
- new data safes as "world" in package
- same rules as in merge_italy.R apply for the new columns
