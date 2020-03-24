#' Show time series of region/province
#'
#' @param df data.frame
#' @param regions regions to be considered
#' @param outcome outcome shown
#' @param measures measures that are plotted
#'
#' @return ggplot
#' @export
#'
#' @examples
#' show_ts()
show_ts <- function(df=it,
                    regions = c("Lombardia","Lombardy"),
                    outcome="growth",
                    measures=it.measures) {

  if(!is.null(regions))
    df <- df %>% filter(region %in% regions | country %in% regions)

  df.measures <- Covid::measures %>% filter(ADM1 %in% regions | Country %in% regions)
  df.measures$region <- df.measures$ADM1

  ggplot(df)+
    geom_point(aes_string(x="Date",y=outcome))+
    geom_line(aes_string(x="Date",y=outcome))+
    geom_vline(data = df.measures,
               aes(xintercept=Start), color ="red")+
    geom_text(data =  df.measures,
              aes(x=Start+.5,label=Type),
              y=1, angle=90,
              color="red")

}

#' Plot timeline of all measures of a country/region
#'
#' @param units name of country or region (also takes vector of names)
#'
#' @return ggplot
#' @export
#'
#' @examples
#' show_measures_of(units="Ger")
#' show_measures_of(units=c("Iran","Spain","Italy","Germ"))
#' #show_measures_of(df=world %>% filter(in_country=="Mainland China"))
show_measures_of <- function(units=NULL,measures=world.measures, df=world)
{
    if(!is.null(units))
      {
        keep <- F
        for(name.cur in units){
          keep <- keep | grepl(name.cur, df$label,fixed=F)
        }
        df <- df %>% filter(keep)
      }

  if(length(unique(df$label))>32)
    stop(paste(length(unique(df$label)),"labels selected. subset df or use name parameter."))

  ggplot(df %>% tidyr::pivot_longer(matches(measures) & ends_with("_active")),
         aes(x=Date,y=name,color=value))+geom_point()+facet_wrap(vars(label))
}

#' Plot timeline of all measures with countries
#'
#' Country/region lables without any measure enacted are droped
#'
#' @param measure keeps all measures where substring matches (also takes vector of measures)
#' @param df data frame
#'
#' @return ggplot
#' @export
#'
#' @examples
#' show_countries_of()
show_countries_of <- function(measure=c("University", "School","Curfew"),df=world%>%filter(is.na(in_country)))
{
  df.cur <- df %>% tidyr::pivot_longer(matches(measure) & ends_with("_active"))
  df.cur <- df.cur %>% group_by(label) %>%
    mutate(any_measure = sum(value)) %>% filter(any_measure>0)
  ggplot(df.cur,
         aes(x=Date,y=label,color=value))+geom_point()+facet_wrap(vars(measure))
}
