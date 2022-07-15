# rci() fucntion def, which should  deprecate jt_rci_calc(). 


#' Compute the RCI value as in Jacobson & Truax (1991) and/or alternative 
#' two-observation estimands.
#' 
#' This provides one RCI value for every observed difference score, 
#' essentially intended to be interpreted as Z-scores.
#'
#' @param difference Numeric. Difference score(s). Computed from \code{t1} and \code{t2} if not provided.
#' @param t1 Numeric. Time 1 value(s). Ignored if \code{difference} is provided.
#' @param t2 Numeric. Time 2 value(s). Ignored if \code{difference} is provided.
#' @param scale_rci Numeric. RCI for the scale, meaning the smallest change considered reliable.
#' @param r1 Numeric.  First (or only) reliability coefficient to use in calculation
#' @param r2 Numeric.  Optional second reliability coefficient to use in calculation
#' @param sd1 Numeric. Standard deviation 1. Should be a group representative of the target group. Calculated/assumed if not provided. 
#' @param sd1 Numeric. Optional standard deviation 2. Should be a group representative of the target group. Calculated if not provided. 
#' @param sdiff Numeric.  Standard error of the difference score. If provided, sem, rxx, sd1 are all ignored. Calculated if not provided.
#' @param sem Numeric. Standard error of measurement. Calculated if not provided.
#' @param prob Numeric 0-1. Defaults to .975 to provide 95% two-sided confidence.  
#' @param verbose Logical. If TRUE, will return additional information about the calculation. 
#'
#' @return Individual RCI value per difference score provided.
#' @export
#' 
#' @details 
#' While the Jacobson & Truax (1991) definition (which is actually the Christensen & Mendoza, 
#' 1986 correction) has become the dominant version of providing statistical tests to the
#' difference between two scores from one person, it is merely the simplest possible
#' version of this estimand. To achieve simplicity, it relies on assumptions that will not
#' generally be true and that you may be able to relax by including more information. 
#' 
#' For instance, it is possible that the SD among a group of individuals at time 1 is different
#' than the SD at time 2, for instance because treatment will de-homogenize individuals or
#' induce greater heterogeneity depending on the circumstance. Additionally, the reliability
#' coefficient may be meaningfully different at the two timepoints. Accommodating these
#' possibilities is not difficult when computing the RCI, and will only serve to make
#' the assumptions less wildly unjustified. 
#'
#' @examples jt_rci_calc(difference = 15, sdiff = 4.74)
#' jt_rci_calc(difference = 1, sdiff = .707)
rci <- function(difference = NULL,
                t1 = NULL, 
                t2 = NULL,
                scale_rci = NULL, 
                r1 = NULL, 
                r2 = NULL,
                sd1 = NULL, 
                sd2 = NULL,
                sdiff = NULL, 
                sem = NULL, 
                prob = .975,
                verbose = FALSE){
  # check for the correct type of data presented
  # if there is t1, there needs to be t2 and can set diff
  
  # if there is no RCI provided for the scale, need to compute it. 
  if(is.null(scale_rci)){
    scale_rci <- scale_rci_calc(sdiff = sdiff, 
                                rxx = rxx,
                                sd1 = sd1,
                                sem = sem,
                                prob = prob, 
                                verbose = FALSE)
  }
  # should have scale_rci now
  
  # if only the scale_rci is provided, need to compute sdiff
  if(!is.null(scale_rci) & is.null(sdiff)){
    sdiff <- scale_rci / qnorm(prob)
  }
  # need to compute difference if not provided
  if(is.null(difference)){
    if(length(t1) != length(t2)){
      warning("t1 and t2 not the same length. Values recycled if possible. Check data entry.")
    }
    difference = t2 - t1
  }
  # now can calculate score-based RCIs. 
  RCI <- difference/sdiff
  if(verbose == FALSE){
    return(RCI)
  }
  if(verbose){
    # print(RCI)
    return(list(RCI = RCI, 
                difference = difference,
                scale_rci = scale_rci,
                sdiff = sdiff, 
                sem = sem, 
                rxx = rxx, 
                sd1 = sd1, 
                prob = prob))
  }
}