# simple RCI calculators

#' Calculate the Reliable Change Index according to Jacobson & Truax (1991).
#' 
#' Using a number of possible input combinations, most commonly time 1, 
#' time 2, reliability, and SD value
#'
#' @param rxx Numeric.  Reliability coefficient to use in calculation
#' @param sdiff Numeric.  Standard error of the difference score. Calculated if not provided.
#' @param sem Numeric. Standard error of measurement. Calculated if not provided.
#' @param sd1 Numeric. Standard deviation 1. Should be a group representative of the target group. Calculated if not provided. 
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
  }
  # if there is no sd1 provided, need to compute it and give warning
  if(is.null(sd1) & is.null(sem) & is.null(sdiff)){
    simpleError("No SD provided and cannot be inferred.")
  } 
  # now should have sd1
  
  # if don't have sem yet, compute it
  if(is.null(sem)){
    sem <- sd1 * sqrt(1 - rxx)
  }
  # if don't have sdiff, compute it. 
  if(is.null(sdiff)){
    sdiff <- sqrt(2 * sem^2)
  }
  # should have sdiff
  RCI <- prob * sdiff
  RCI
}
rci_calc_jt(.8, 10)
rci_calc_jt()
rci_calc_jt(.8, 10, .2)
rci_calc_jt(.8, 10, .2, .2)
rci_calc_jt(c(15))

# temporary storage: 
# calc_rci() takes minimally two vectors of data and computes the RCI for them. 
# in the minimal case, the reliability coefficient is computed as the test-retest correlation
# between the two PAIRED data vectors. 
# You can optionally supply different vectors for computing this reliability
# Or you can provide the reliability directly, for instance an internal consistency
# If verbose = TRUE, returns a list of calculated variables

calc_rci <- function(pre, post, t1_r = pre, t2_r = post, r = NA, verbose = FALSE){
  # pre is a vector of pre-treatment scores
  # post is a vector of PAIRED post-treatment scores
  # t1_r is a vector of pre-treatment scores for computing reliability, defaults to pre
  # t2_r is a vector of post-treatment scores for computing reliability, defaults to post
  # those two can be different than the test variables if desired
  # r is the reliability coefficient to use
  
  sd_pre <- sd(pre)
  
  # check if need to calculate reliability
  if(is.na(r)){
    r <- cor(as.numeric(t1_r), as.numeric(t2_r))
  }
  
  # calculations
  s_E <- sd_pre * sqrt(1 - r)
  s_diff <- sqrt(2 * s_E^2)
  RCI <- 1.96 * s_diff
  
  # Different returns depending on verbose
  if(verbose == FALSE){
    return(RCI)
  }
  if(verbose == TRUE){
    output <- list("RCI" = RCI, "SE_diff" = s_diff, "SE" = s_E, "reliability" = r, "SD_pre" = sd_pre)
    return(output)
  }
}