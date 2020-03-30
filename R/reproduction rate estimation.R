#' Estimate reproduction rate for a time series of incidence
#'
#' Standards are based on paper Serial interval of novel coronavirus (COVID-19) infections
#' "while the mean and standard deviation (SD) of the serial interval were estimated at 4.7 days (95% CrI: 3.7, 6.0) and 2.9 days (95% CrI: 1.9, 4.9), respectively"
#'
#' @param incidence
#' @param total
#' @param l.interval
#' @param warnings
#' @param plot
#' @param mean.si
#' @param sd.si
#'
#' @return
#' @export
#'
#' @examples
#' estimateR(plot=TRUE)
#' estimateR(plot=TRUE,mean.si=7.5,sd.si=3.4,l.interval=2)
estimateR <- function(incidence = ita %>% filter(region=="Lombardy")%>%pull(pos.total_it),
                      total = TRUE,
                      l.interval = 2,
                      warnings = FALSE,
                      plot = FALSE,
                      mean.si = 4.7,
                      sd.si = 2.9) {

  if(total)
    incidence <- incidence - lag(incidence)

  if(warnings)
    warning(paste("drop",sum(is.na(incidence)), " NA observations."))

  if(
    (max(which(!is.na(incidence)))-
     min(which(!is.na(incidence))))> (sum(!is.na(incidence))-1)){
    warning("Missing values in between. Consider interpolation")
    return(NA)
  }

  incidence <- incidence[!is.na(incidence)]

  #return NA if too short for valid
  if(length(incidence)-l.interval<=2){
    warning(paste0("Not enough observations"))
    return(NA)
  }

  t_start <- seq(2, length(incidence)-l.interval)
  t_end <- t_start + l.interval

  res <- EpiEstim::estimate_R(
    incid = incidence,
    method = "parametric_si",
    config = EpiEstim::make_config(list(
      t_start = t_start, t_end = t_end,
      mean_si = mean.si, std_si = sd.si
    ))
  )

  if(plot)
    plot(res)


  return(res)
}


#' Add R (reproduction rate estimate) to data frame
#'
#' Should be used by group as in example
#'
#' @param x
#' @param total
#'
#' @return
#' @export
#'
#' @examples
#' res <- ita %>%
#'    group_by(i) %>%
#'    mutate(R = addR(pos.total_it,date,l.interval=7)) %>% ungroup()
#' res %>% select(R,pos.total_it,date,region)%>%filter(!is.na(R))
addR <- function(x,date=NULL,total=TRUE,...)
{
  if(!is.null(date)){
    if(!identical(sort(date),date))
      stop("Not ordered by date. Estimation of R would be flawed.")
  }

  if(sum(!is.na(x)<3))
    return(NA)

  if(total)
    x <- x-lag(x)

  if(sum(x<0,na.rm = TRUE)>0)
  {
    x[x<0]<-0
    warning("negative incidence values were assigned zero.")
  }
  res.cur <- estimateR(x,total = FALSE,...)


  #return NA if estimateR does so
  if(all(is.na(res.cur)))
    return(NA)
  else
    return(
      c(rep(NA,length(x)-length(res.cur$R$`Mean(R)`)),
        res.cur$R$`Mean(R)`)
    )
}


