#' Utilization Function
#'
#' Utilization (work load) of the system
#' @param c number of servers
#' @param mu service/processing rate
#' @param lambda arrival/incoming rate
#' @return numeric vector, Returns the value of the utilization of the system.
#' @examples utilization(c=2, mu=3, lambda=4)
#' @examples utilization(2, 3, 4)
#' @export
#'
utilization <- function(c, mu, lambda){
  return(lambda / (c*mu))
}
