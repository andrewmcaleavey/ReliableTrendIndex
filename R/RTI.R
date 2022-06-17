## RTI calculators

#' Compute the RCI and RTI for a single person's observations..
#' 
#' This is essentially a wrapper for \code{metafor::rma()} with some specific behavior. 
#' It takes one or more observations with an indication of error variance, and computes
#' the RCI and/or RTI as applicable.  
#'
#' @param values Numeric. Either a single difference value, two observations, or more 
#' than two observations of the same variable for one person. While the three cases are 
#' treated separately under the hood and return slightly different values and text, 
#' the computation is the same and all involve primarily \code{metafor::rma.uni()}.
#' @param variance Numeric. The error variance. NOT the Sdiff directly, but if you have
#' the Sdiff previously computed, you can use its square. 
#' @param digits Integer. Number of digits to print for metafor::rma(). Defaults to 2.
#' @param cutpoint Cutpoint on z-scale to use for "reliability." Defaults to 1.96.
#' @param ... Additional arguments passed on.
#'
#' @return A list including the JT RCI value per observation in difs, 
#' A classification per observation in difs
#' @export
#'
#' @examples output <- rti_calc_simple(c(47.5, 32.5), 4.74^2)
#' output
#' metafor::forest(output$rmaObj)
#' output2 <- rti_calc_simple(c(5, 4, 3, 2), 1)
#' output2
#' metafor::forest(output2$rmaObj)
#' summary(output2$rmaObj)
#' forest_to_reg_plot(output2$rmaObj, StError = output2$variance)
#' 
rti_calc_simple <- function(values, variance, digits = 2, cutpoint = 1.96, ...){
  # if values is 2 or longer, should make sure it's a trend, otherwise the RCI works. 
  if(length(values) == 1){
    rmaobj <- metafor::rma.uni(yi = values, 
                               vi = variance, 
                               method = "FE")
    JT_rci = rmaobj$zval
    JT_rci_classification = ifelse(JT_rci > cutpoint, 
                                   "Reliable Increase", 
                                   ifelse(JT_rci < -cutpoint, 
                                          "Reliable Decrease", 
                                          "No Reliable Change"))
    return(list(JT_rci = JT_rci, 
                RTI = NA, 
                category = JT_rci_classification,
                rmaObj = rmaobj,
                values = values, 
                variance = variance, 
                cutpoint = cutpoint))
  } else if(length(values == 2)) {
    difs <- values[-1] - values[1]
    time_linear <- seq(from = 1, 
                       to = length(values) - 1, 
                       by = 1)
    rmaobj <- metafor::rma.uni(yi = difs, 
                               mods = ~ 0 + time_linear,
                               vi = variance, 
                               method = "FE")
    RTI = rmaobj$zval
    RTI_classification = ifelse(RTI > cutpoint, 
                                "Reliable Increase", 
                                ifelse(RTI < -cutpoint, 
                                       "Reliable Decrease", 
                                       "No Reliable Change"))
    return(list(JT_rci = RTI, 
                RTI = RTI,
                category = RTI_classification,
                rmaObj = rmaobj,
                values = values, 
                variance = variance, 
                cutpoint = cutpoint))
  } else {
    print("More than two values provided, assuming they are evenly spaced in time.")
    difs <- values[-1] - values[1]
    time_linear <- seq(from = 1, 
                       to = length(values) - 1, 
                       by = 1)
    rmaobj <- metafor::rma.uni(yi = difs, 
                               mods = ~ 0 + time_linear,
                               vi = variance, 
                               method = "FE")
    RTI = rmaobj$zval
    RTI_classification = ifelse(RTI > cutpoint, 
                                "Reliable Increase", 
                                ifelse(RTI < -cutpoint, 
                                       "Reliable Decrease", 
                                       "No Reliable Change"))
    return(list(JT_rci = NA,
                RTI = RTI, 
                category = RTI_classification,
                rmaObj = rmaobj,
                values = values, 
                variance = variance, 
                cutpoint = cutpoint))
  }
}

# output <- rti_calc_simple(c(47.5, 32.5), 4.74^2)
# output
# 
# rti_calc_simple(c(15, 5), 4.74^2)
# metafor::forest(output$rmaObj)
# output2 <- rti_calc_simple(c(5, 4, 3, 2), 1)
# output2
# metafor::forest(output2$rmaObj)
# summary(output2$rmaObj)
# forest_to_reg_plot(output2$rmaObj, StError = output2$variance)
# 

# now would like a way to do it for a lot of people in a long-format dataset. 
# need a data set on which to do it. 

rti_on_data <- function(data, 
                        id, 
                        time_var = NULL, 
                        values, 
                        variance, 
                        ...){
  
}