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
#' @param units name of country or region (also takes vector of names). As standard shows only unit lables that are countries and had more than 1000 new cases at some point.
#' @param measures vector of measures. standard are all in world.measures
#' @param df data frame. standard is world.
#'
#' @return ggplot
#' @export
#'
#' @examples
#' show_measures_of()
#' show_measures_of(units="Ger")
#' show_measures_of(units=c("Iran","Spain","Italy","Germ"))
#' show_measures_of(units=NULL, df=world %>% filter(in_country=="Mainland China"))
show_measures_of <- function(units=NULL,
                             measures=world.measures,
                             df=world)
{
  #if not given take all that have measures
  if(is.null(units))
  {
    units <- world %>% tidyr::pivot_longer(cols = matches(world.measures) & ends_with("_active"),
                                c("measure"),values_to = "active")%>%
    group_by(country)%>%
    summarise(measures_pos=sum(active)>0)%>%
    filter(measures_pos==TRUE)%>%
    pull(country)
    }


    keep <- F
    for(name.cur in units){
        keep <- keep | grepl(name.cur, df$label,fixed=F)
    }
    df <- df %>% filter(keep)


    #stop if too many labels
    if(length(unique(df$label))>32)
      stop(paste(length(unique(df$label)),"labels selected. subset df or use measure parameter."))

  ggplot(df %>% tidyr::pivot_longer(matches(measures) & ends_with("_active")),
         aes(x=Date,y=name,color=value))+geom_point()+facet_wrap(vars(label))
}

#' Plot timeline of all measures with countries
#'
#' Country/region lables without any measure enacted are droped
#'
#' @param measure_name (also takes vector of names) keeps all measures where substring matches
#' @param df data frame
#'
#' @return ggplot
#' @export
#'
#' @examples
#' show_countries_of()
show_countries_of <- function(measure_name=c("University", "School","Curfew"),df=world%>%filter(is.na(in_country)))
{
  df.cur <- df %>% tidyr::pivot_longer(matches(measure_name) & ends_with("_active"))
  df.cur <- df.cur %>% group_by(label) %>%
    mutate(any_measure = sum(value)) %>% filter(any_measure>0)
  ggplot(df.cur,
         aes(x=Date,y=label,color=value))+geom_point()+facet_wrap(vars(name))
}


#' show outcome as time series including measures taken
#'
#' @param select_region region to show
#' @param select_country country to show
#' @param outcome character variable describing outcome in all data frame
#' @param df alternative data frame to all that contains outcome
#' @param total is the total number given and should be transformed to change (standard: TRUE)
#' @param rate should the growth rate be computed and shown
#' @param smooth should a geom_smooth be added with span=smooth (standard f,smooth=.2 recommended)
#'
#' @return
#' @export
#'
#' @examples
#' show_outcome()
#' show_outcome(outcome="dead.new",select_country="Germany")
#' show_outcome(outcome="dead.total",select_country="Germany")
#' show_outcome(outcome="dead_it")
#' show_outcome(select_country="Italy",outcome="positive",smooth=.2)
show_outcome <- function(select_region="Lombardy",
                         select_country=NULL,
                         outcome="pos.new",
                         df=NULL,
                         total=F,
                         rate=F,
                         smooth=FALSE)
{

  #select subset
  if(is.null(select_country)){
    df.cur <- Covid::all %>% dplyr::filter(region==select_region)
    treat.cur <- Covid::all_long %>% dplyr::filter(region==select_region)}
  else{
    df.cur <- Covid::all %>% dplyr::filter(country==select_country, adm_level==0)
    treat.cur <- Covid::all_long %>% dplyr::filter(country==select_country, adm_level==0)
  }

  if(!is.null(df))
    df.cur <- df


  #reformat to change
  if(total)
    df.cur[,outcome]<-df.cur[,outcome]-lag(df.cur[,outcome],order_by = df.cur$date)

  if(rate)
    df.cur[,outcome]<-log(df.cur[,outcome])/log(lag(df.cur[,outcome],order_by = df.cur$date))

  # select first date for each treatment
  treat.cur <- treat.cur %>%
     group_by(treatment) %>%
     filter(!is.na(active)) %>%
     filter(active) %>%
     arrange(date)%>%
     slice(1L)


  #plot
  p<- ggplot(df.cur)+
    geom_point(aes_string(x="date",y=outcome))+
    geom_line(aes_string(x="date",y=outcome))+
    geom_vline(data = treat.cur,
               aes(xintercept=date), color ="red")+
    ggrepel::geom_text_repel(data =  treat.cur,
              aes(x=date,label=substr(treatment,1,6)),
              y=3, angle=90,
              color="red")

  if(smooth)
    p <- p + geom_smooth(aes_string(x="date",y=outcome),span=smooth)

  p
}
