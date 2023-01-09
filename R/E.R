#' E Function
#'
#' Depending on the type specification, the function returns the expected values of the waiting time in the system, objects in queue or objects in the system for GGC-Systems.
#' @param ewq Expected value of queue waiting time
#' @param lambda arrival rate
#' @param mu service rate
#' @param type character, type in 'Obj Q' for E(Nq), 'Obj S' for E(N) or 'Wait S' for E(W)
#' @return expected values
#' @export

E <- function(ewq, lambda, mu, type=""){
  if (type == "Obj Q"){
    return(lambda*ewq)
  }
  else if (type == "Obj S"){
    return((ewq+1/mu)*lambda)
  }
  else if (type == "Wait S"){
    return(ewq+1/mu)
  }
  else if (type == ""){
    print("Please type in Obj Q, Obj S or Wait S for type!", quote=FALSE)
  }
  else{
    print("Please type in Obj Q, Obj S or Wait S for type!", quote=FALSE)
  }
}
