
#' Drops any units (countries) that do not have measures imposed
#'
#' @param df data frame to be subsetted
#' @param measures vector of measures to be considered
#'
#' @return pdata.frame
#' @export
#'
#' @examples
#' keep_units_with()
keep_units_with<-function(df=world, measures=world.measures)
{
  df$any_measures <- rowSums(df[paste0(measures,"_active")])
  df <- df %>% group_by(label)%>%
    mutate(any_measures_in_label=sum(any_measures)>0) %>%
    ungroup() %>% pdata.frame(index=c("t","label"))

  message(paste("Drop ",length(unique(df$label[!df$any_measures_in_label])), "units of observation"))

  df <- subset(df,any_measures_in_label)
  df$any_measures_in_label <- NULL
  df$any_measures <- NULL

  df
}
