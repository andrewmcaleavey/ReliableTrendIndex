# simple RCI calculators

#' Calculate the Reliable Change Index for a scale
#' 
#' This takes basic psychometric data and returns a value applicable
#' to difference scores, in theory. This is explicitly NOT what Jacobson & Truax (1991)
#' did, but it is common. The resulting value is the scale's RCI. 
#' 
#' @param rxx Numeric.  Reliability coefficient to use in calculation
#' @param sd1 Numeric. Standard deviation 1. Should be a group representative of the target group. Calculated if not provided. 
#' @param sdiff Numeric.  Standard error of the difference score. If provided, sem, rxx, sd1 are all ignored. Calculated if not provided.
#' @param sem Numeric. Standard error of measurement. Calculated if not provided.
#' @param prob Numeric 0-1. Defaults to .975 to provide 95% two-sided confidence.  
#' @param verbose Logical. If TRUE, will return additional information about the calculation.
#'
#' @return Numeric. The value of the RCI for the scale, meaning how large a change is required to be observed to be considered reliable.
#' @export
#' 
#' @details It is important to note that the \code{sdiff} value overwrites any other parameter if it is provided.
#' Typical inputs include: 
#' sdiff with anything else; sem with anything else; rxx and sd1.
#'
#' @examples RCI1 <- scale_rci_calc(42, 30, .8, 15)
scale_rci_calc <- function(rxx = NULL, 
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
# scale_rci_calc(sdiff = 4.74)  # minimum is sdiff
# scale_rci_calc(.8, 10) # minimum needed is reliability and SD
# scale_rci_calc() # error
# scale_rci_calc(.8, 10, .2) # uses sdiff, reliability ignored
# scale_rci_calc(.8, 10, .2, .5) # uses sdiff value
# scale_rci_calc(c(15))
# scale_rci_calc(.8, 10, sem = .3)

#' Compute the RCI value as in Jacobson & Truax (1991).
#' 
#' This provides one RCI value for every observed difference score, 
#' essentially intended to be interpreted as Z-scores.
#'
#' @param difference Numeric. Difference score(s). Computed from \code{t1} and \code{t2} if not provided.
#' @param t1 Numeric. Time 1 value(s). Ignored if \code{difference} is provided.
#' @param t2 Numeric. Time 2 value(s). Ignored if \code{difference} is provided.
#' @param scale_rci Numeric. RCI for the scale, meaning the smallest change considered reliable.
#' @param rxx Numeric.  Reliability coefficient to use in calculation
#' @param sd1 Numeric. Standard deviation 1. Should be a group representative of the target group. Calculated if not provided. 
#' @param sdiff Numeric.  Standard error of the difference score. If provided, sem, rxx, sd1 are all ignored. Calculated if not provided.
#' @param sem Numeric. Standard error of measurement. Calculated if not provided.
#' @param prob Numeric 0-1. Defaults to .975 to provide 95% two-sided confidence.  
#' @param verbose Logical. If TRUE, will return additional information about the calculation. 
#'
#' @return Individual RCI value per difference score provided.
#' @export
#'
#' @examples jt_rci_calc(difference = 15, sdiff = 4.74)
jt_rci_calc <- function(difference = NULL,
                        t1 = NULL, 
                        t2 = NULL,
                        scale_rci = NULL, 
                        rxx = NULL, 
                        sd1 = NULL, 
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

# simple tests
# jt_rci_calc(difference = 15, sdiff = 4.74)
# jt_rci_calc(difference = 15, sem = 3.35)
# jt_rci_calc(difference = 15, scale_rci = scale_rci_calc(sdiff = 4.74))
# jt_rci_calc(t1 = 32.5, t2 = 47.5, sdiff = 4.74)
# jt_rci_calc(t1 = c(32.5, 31.5), t2 = c(47.5, 48), sdiff = 4.74)
# jt_rci_calc(t1 = c(32.5, 31.5), t2 = 47.5, sdiff = 4.74)

# function to classify observations in a standard way, or more than one, perhaps. 

#' Classify scores using Reliable Change Index values
#'
#' This function assumes you only want categories of reliable change based on 
#' Jacobson & Truax (1991). You probably don't want this.
#' 
#' @param difference Difference score(s)
#' @param scale_rci Scale RCI value
#'
#' @return A character vector with the standard values of "Reliably Increased", "Reliably Decreased", or "No Reliable Difference".
#' Also prints a warning to screen indicating that this is probably a terrible idea
#' @export
#'
#' @examples rci_classifier(difference = c(5, 3, -10), scale_rci = 4)
rci_classifier <- function(difference = NULL,
                           scale_rci = NULL){
  # the final real step is this: 
  outcome <- dplyr::case_when(difference > scale_rci ~ "Reliably Increased", 
                              difference < -scale_rci ~ "Reliably Decreased", 
                              TRUE ~ "No Reliable Difference")
  warning("Categorizing continuous data is almost always a mistake and using the RCI
to do so is particularly problematic in many cases. Are you sure you want to use this function?
(The function worked fine but it's not a good idea.)")
  outcome
}
# rci_classifier(difference = c(5, 3, -10), 
#                scale_rci = 4)
