
#' Extract mean from MCMC summary object
#'
#' @param res mcmc object
#' @param var string descirbing variables to extract
#'
#' @return data.frame with t,country, and var
#' @export
extract_mean<- function(res,var="R\\[",without=NULL)
{
  res <- res$quantiles
  data$date <- lubridate::ymd(data$date)

  if(is.null(without)){
    m <- res[grepl(var,rownames(res)),3]
    min <- res[grepl(var,rownames(res)),1]
    max <- res[grepl(var,rownames(res)),5]
  } else{
    m <- res[grepl(var,rownames(res)) & (!grepl(without,rownames(res))),3]
    min <- res[grepl(var,rownames(res)) & (!grepl(without,rownames(res))),1]
    max <- res[grepl(var,rownames(res)) & (!grepl(without,rownames(res))),5]
  }
  R <- NULL
  for(i in 1:ncol(d))
  {
    country.i <- colnames(d)[i]
    R.cur <- grepl(paste0(",",i,"\\]"), names(m))
    R.cur <- data.frame(t=(nrow(d)-sum(R.cur)+1):nrow(d),
                        date=seq.Date(min(data$date),max(data$date),by=1)[(nrow(d)-sum(R.cur)+1):nrow(d)],
                        country=rep(country.i,sum(R.cur)),
                        m=as.numeric(m[R.cur]),
                        min=as.numeric(min[R.cur]),
                        max=as.numeric(max[R.cur]))
    R <- rbind(R,R.cur)
  }

  colnames(R)[4]<-sub("^([[:alpha:]]*).*", "\\1",var)
  R
}
