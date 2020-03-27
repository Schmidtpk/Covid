
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
#'
#' @return output of stargazer
#' @export
panel_reg <- function(formula.cur, df) {

  plm.pool <- plm::plm(
    formula = formula.cur,
    data = df,model ="pooling")

  plm.time<- plm::plm(
    formula = formula.cur,
    data = df,effect ="time")

  plm.ind<- plm::plm(
    formula = formula.cur,
    data = df,effect ="individual")

  plm.both <- plm::plm(
    formula = formula.cur,
    data = df,effect ="twoways")


  stargazer::stargazer(type="text",
            lmtest::coeftest(plm.pool,vcov=sandwich::vcovHC(plm.pool,cluster="group")),
            lmtest::coeftest(plm.time,vcov=sandwich::vcovHC(plm.time,cluster="group")),
            lmtest::coeftest(plm.ind,vcov=sandwich::vcovHC(plm.ind,cluster="group")),
            lmtest::coeftest(plm.both,vcov=sandwich::vcovHC(plm.both,cluster="group")),add.lines =
              list(
                c("effects","pooling","time","individual","both")
              ))
}
