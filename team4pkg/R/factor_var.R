#' Factor Variables
#'
#' This function converts numeric variables into factor variables for the purposes of imputation.  
#' The first step is to supply the list of variables needed to be converted into factors.
#' @param dat is the data be imputed, where we want to produce factor variables
#' @param x is the list of variables to be factored
#' @examples
#'\dontrun{
#'v <- c("job","marital","education")
#'to.factor(data,v)
#'}
#' @export
to.factor <- function(dat,x){
    dat[,x] <- lapply(dat[,x],factor)
    str(dat)
}
