#' Expands formulas for plm
#'
#' @param form Formula where ~. is substituted
#' @param dataframe for colnames
#'
#' @return
#' @export
#'
#' @examples
expand_formula <-
  function(form="pos.growth ~.",df){

    varNames <- colnames(df)
    has_dot <- any(grepl('~.',form,fixed=TRUE))
    if(has_dot){
      ii <- intersect(as.character(as.formula(form)),
                      varNames)
      varNames <- varNames[!grepl(paste0(ii,collapse='|'),varNames)]

      exp <- paste0("~",paste0(varNames,collapse='+'))
      as.formula(gsub('~.',exp,form,fixed=TRUE))

    }
    else as.formula(form)
  }
