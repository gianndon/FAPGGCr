#' EWq Function
#'
#' Returns the value of the expected value for the queue value time for GGC-Systems
#' !ATTENTION! cv_lam and cv_mu do NOT have to be passed squared!
#' @param cv_lam coefficient of variation of arrival rate lambda
#' @param cv_mu coefficient of variation of service rate mu
#' @param c number of server
#' @param mu service rate
#' @param lambda arrival rate
#' @param type character, type in Kingman for the Kingman's approximation or Whitt for the Whitt et al. approximation
#'
#' @return expected waiting time in queues
#' @export
#'
#' @examples EWq(cv_lam=0.25, cv_mu=1, c=7, mu=1, lambda=0.5, type="Kingman")
#' @examples EWq(cv_lam=0.25, cv_mu=1, c=7, mu=1, lambda=0.5, type="Whitt")
EWq <- function(cv_lam, cv_mu, c, mu, lambda, type=""){

  # utilization
  u <- lambda/(c*mu)

  # decision between the king and whit method
  if (type == "Kingman"){
    return(((cv_lam^2+cv_mu^2)/2)*(u/(1-u))*(u^(sqrt(2*(c+1))-2))*(1/(c*mu)))
  }
  else if (type == "Whitt"){
    # caluclation of b
    b <- if (cv_lam <= 1){
      exp(((-2*(1-u))/(3*u)) * ((1-cv_lam^2)^2/(cv_lam^2+cv_mu^2)))
    }
    else if (cv_lam > 1){
      1
    }

    # caluclation of zeta
    n <- c-1
    x <- u*c
    psi <- (x^n/factorial(n)) / (sum(x^(0:n) / factorial(0:n)))
    z <- (u*psi) / (1-u+u*psi)

    # caluclation of expected value for queue
    l <- b*((cv_lam^2+cv_mu^2)/2)*(z/(c*mu*(1-u)))
    return(l)
  }
  else if (type == ""){
    print("Please enter Kingman or Whitt for type!", quote=FALSE)
  }
  else{
    print("Please enter Kingman or Whitt for type!", quote=FALSE)
  }
}
