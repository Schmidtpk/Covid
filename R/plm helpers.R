
#' make to pdata.frame with index i and t
#'
#' Use after dplyr commands on data frames
#'
#' @param df data frame to be coverted
#'
#' @return pdata.frame
#' @export
mypanel <- function(df) plm::pdata.frame(df,index = c("i","t"))

#' Run panel regressios with fixed effects
#'
#' @param formula.cur a valid formular
#' @param df a data frame
#' @param trendwith factor to be interacted with time trend. standard is "i". Viable alternative is "region" or "country".
#' @param hide.date hides all coefficients with date (standard is TRUE)
#'
#' @return output of stargazer
#' @export
panel_reg <- function(formula.cur, df,
                      trendwith="i",
                      hide.date=TRUE) {


  plm.pool <- plm::plm(
    formula = formula.cur,
    data = df,model ="pooling")

  plm.ind<- plm::plm(
    formula = formula.cur,
    data = df,effect ="individual")

  plm.trend<- plm::plm(
    formula = paste0(formula.cur,"+date*factor(",trendwith,")",sep=""),
    data = df,effect ="individual")

  plm.trend2<- plm::plm(
    formula = paste0(formula.cur,"+poly(date,2)*factor(",trendwith,")",sep=""),
    data = df,effect ="individual")

  plm.both <- plm::plm(
    formula = formula.cur,
    data = df,effect ="twoways")


  stargazer::stargazer(type="text",
            omit=c("^poly\\(date, 2\\)",if(hide.date) "date" else NULL),
            omit.labels = c("countr squared trend","date trend"),
            lmtest::coeftest(plm.pool,vcov=sandwich::vcovHC(plm.pool,cluster="group")),
            lmtest::coeftest(plm.ind,vcov=sandwich::vcovHC(plm.ind,cluster="group")),
            lmtest::coeftest(plm.trend,vcov=sandwich::vcovHC(plm.trend,cluster="group")),
            lmtest::coeftest(plm.trend2,vcov=sandwich::vcovHC(plm.trend2,cluster="group")),
            lmtest::coeftest(plm.both,vcov=sandwich::vcovHC(plm.both,cluster="group")),add.lines =
              list(
                c("effects","pooling","individual","countryttrend","countryttrend2","both")
              ))
}


#' Factorize continuous variables by quantiles of ecdf
#'
#' @param x variable to be factorized
#' @param length number of factors
#'
#' @return vector
#' @export
#'
#' @examples
#' unique(factorize(all$tMax,7))
factorize <- function(x,length=5) {

  quantiles.cur <- quantile(x, seq(0+1/length,1-1/length,length.out = length-1),na.rm=TRUE)


  factor(
    findInterval(x,quantiles.cur)-(length-1)/2,
    labels = paste(c("<",quantiles.cur),"-",c(quantiles.cur,">")),
    levels = seq(-length/2+.5, length/2-.5,length.out = length))
  }
