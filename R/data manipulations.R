
#' Drops any units (countries) that do not have measures imposed
#'
#' @param df data frame to be subsetted
#' @param measures vector of measures to be considered (keeps any measures that submatch string)
#'
#' @return pdata.frame
#' @export
#'
#' @examples
#' units_with()
units_with<-function(df=world, measures=world.measures)
{

  df$any_measures <- rowSums(
    df %>% select(matches(measures) & ends_with("_active"))
    )

  df$any_measures_in_label <- df %>% group_by(label)%>%
    mutate(any_measures_in_label=sum(any_measures)>0)%>%pull(any_measures_in_label)

  message(paste("Found ",length(unique(df$label[!df$any_measures_in_label])), "units of observation without measures"))

  df$any_measures_in_label
}
