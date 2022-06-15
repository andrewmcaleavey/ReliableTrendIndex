# simple RCI calculators

#' Calculate the Reliable Change Index according to Jacobson & Truax (1991).
#' 
#' Using a number of possible input combinations, most commonly time 1, 
#' time 2, reliability, and SD value
#'
#' @param t1 Numeric.  Value or vector of values at time 1.  
#' @param t2 Numeric.  Value or vector of values at time 2.  
#' @param rxx Numeric.  Reliability coefficient to use in calculation
#' @param diff 
#' @param sdiff 
#' @param sem 
#' @param sd1 
#' @param sd2 
#' @param prob Numeric 0-1. Defaults to .95 to provide 95% two-sided confidence.  
#' @param verbose Logical. If TRUE, will display additional information about the calculation.
#'
#' @return Numeric. The value of the 
#' @export
#'
#' @examples
rci_calc_jt <- function(t1 = NULL, 
                     t2 = NULL, 
                     rxx = NULL, 
                     sd1 = NULL, 
                     diff = NULL, 
                     sdiff = NULL, 
                     sem = NULL, 
                     sd2 = NULL,
                     prob = .95,
                     verbose = FALSE){
  # check for the correct type of data presented
  # if there is t1, there needs to be t2 and can set diff
  # if there is diff, don't need t1 or t2
  # if there is sdiff and diff, don't need anything  else
}

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