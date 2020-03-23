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
#' @param name of country or region (also takes vector of names)
#'
#' @return ggplot
#' @export
#'
#' @examples
#' show_measures_of()
show_measures_of <- function(name="Germany",df=world)
{
  ggplot(df %>% filter(label%in%name) %>% tidyr::pivot_longer(matches(world.measures) & ends_with("_active")),
         aes(x=Date,y=name,color=value))+geom_point()+facet_wrap(vars(label))
}

#' Plot timeline of all measures with countries
#'
#' Country/region lables without any measure enacted are droped
#'
#' @param name of measure (also takes vector of names) keeps all measures where substring matches
#' @param df data frame
#'
#' @return ggplot
#' @export
#'
#' @examples
#' show_countries_of()
show_countries_of <- function(name=c("University", "School","Curfew"),df=world%>%filter(is.na(in_country)))
{
  df.cur <- df %>% tidyr::pivot_longer(matches(name) & ends_with("_active"))
  df.cur <- df.cur %>% group_by(label) %>%
    mutate(any_measure = sum(value)) %>% filter(any_measure>0)
  ggplot(df.cur,
         aes(x=Date,y=label,color=value))+geom_point()+facet_wrap(vars(name))
}
