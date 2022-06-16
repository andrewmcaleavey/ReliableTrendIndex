# simple RCI calculators

#' Calculate the Reliable Change Index according to Jacobson & Truax (1991).
#' 
#' Using a number of possible input combinations, most commonly time 1, 
#' time 2, reliability, and SD value
#'
#' @param rxx Numeric.  Reliability coefficient to use in calculation
#' @param sd1 Numeric. Standard deviation 1. Should be a group representative of the target group. Calculated if not provided. 
#' @param sdiff Numeric.  Standard error of the difference score. If provided, sem, rxx, sd1 are all ignored. Calculated if not provided.
#' @param sem Numeric. Standard error of measurement. Calculated if not provided.
#' @param prob Numeric 0-1. Defaults to .975 to provide 95% two-sided confidence.  
#' @param verbose Logical. If TRUE, will display additional information about the calculation.
#'
#' @return Numeric. The value of the RCI for the scale, meaning how large a change is required to be observed to be considered reliable.
#' @export
#'
#' @examples RCI1 <- rci_calc_jt(42, 30, .8, 15)
rci_calc_jt <- function(rxx = NULL, 
                        sd1 = NULL, 
                        sdiff = NULL, 
                        sem = NULL, 
                        prob = .975,
                        verbose = FALSE){
  # check for the correct type of data presented
  # if there is t1, there needs to be t2 and can set diff
  # if rxx is null, can't do anything
  if(is.null(rxx) & is.null(sem) & is.null(sdiff)){
    print(simpleError("No reliability values provided"))
    return(NULL)
  }
  # if there is no sd1 provided, need to compute it and give warning
  if(is.null(sd1) & is.null(sem) & is.null(sdiff)){
    simpleError("No SD provided and cannot be inferred.")
  } 
  # now should have sd1
  
  # if don't have sem yet, compute it
  if(is.null(sem) & !is.null(sd1) & !is.null(rxx)){
    sem <- sd1 * sqrt(1 - rxx)
  }
  # if don't have sdiff, compute it. 
  if(is.null(sdiff)){
    sdiff <- sqrt(2 * sem^2)
  }
  # there should never be a way to get here without sdiff, but if we do, break.
  if(is.null(sdiff)){
    return(errorCondition("broken"))
  }
  # should have sdiff
  RCI <- qnorm(prob) * sdiff
  if(verbose == FALSE){
    return(RCI)
  }
  if(verbose){
    # print(RCI)
    return(list(RCI = RCI, 
                sdiff = sdiff, 
                sem = sem, 
                rxx = rxx, 
                sd1 = sd1, 
                prob = prob))
  }
}
# simple tests
# rci_calc_jt(sdiff = 4.74)  # minimum is sdiff
# rci_calc_jt(.8, 10) # minimum needed is reliability and SD
# rci_calc_jt() # error
# rci_calc_jt(.8, 10, .2) # uses sdiff, reliability ignored
# rci_calc_jt(.8, 10, .2, .5) # uses sdiff value
# rci_calc_jt(c(15))
# rci_calc_jt(.8, 10, sem = .3)
