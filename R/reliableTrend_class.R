# new S3 class: reliableTrend

#' Basing on S3, following [Advanced R](https://adv-r.hadley.nz/s3.html#s3-classes). 

#' Constructor function for reliableTrend class objects
#' 
#' Internal function to create objects that are of class `reliableTrend`.
#' This is an S3 class from the package `ReliableTrendIndex`.
#' Following [Advanced R](https://adv-r.hadley.nz/s3.html#s3-classes).
#' All reliableTrend objects use this (internally).
#'
#' @param RCI Numeric. Jacobson & Truax RCI, a transformed difference score.
#' @param RTI Numeric. Reliable Trend Index, the z-value of a linear trend
#'   considering a fixed measurement error at each observation.
#' @param pd.RCI Numeric.
#' @param pd.RTI Numeric.
#' @param category.RTI Character. Default is "Less than reliable". One of "Less
#'   than reliable", "Reliable Increase", or "Reliable Decrease".
#' @param category.RCI Character. Default is "Less than reliable". One of "Less
#'   than reliable", "Reliable Increase", or "Reliable Decrease".
#' @param sign.RTI Character. Indicates whether the RTI estimates the trend to
#'   be most likely increasing, decreasing or flat, regardless of confidence
#'   level. Possible values are `"Not calculated"` (the default), `"Increase"`,
#'   `"Decrease"`, or `"Flat"`.
#' @param sign.difference Character. Indicates whether the pre-post comparison
#'   is increasing, decreasing or flat, based on simple subtraction of observed
#'   scores. Possible values are `"Not calculated"` (the default), `"Increase"`,
#'   `"Decrease"`, or `"Flat"`.
#' @param rmaObj An object of type `rma` and/or `rma.uni`, derived from the
#'   `{metafor}` package.
#' @param values Numeric vector. The values used to compute the RTI.
#' @param values.prepost Numeric vector of length 2. The first and last values.
#' @param error_var Numeric. The error variance used to compute the RCI and RTI.
#'   This is on the variance scale, so is the squared standard error of the
#'   difference.
#' @param cutpoint Numeric. Z-scale cutpoint to use for reliability
#'   categorization. Default is 1.96.
#' @param observed Character. Name of the observed variable. Defaults to 
#' `"obs_score"`.
#' @param scale_RCI Numeric. The "RCI for a scale," meaning how many scale
#'   points need to be observed in a difference score for that difference to be
#'   considered "reliable" under J&T.
#'
#' @export
#' @return An object of type `reliableTrend`. 
#' This object is a glorified list with the above-defined parameters as values. 
#' @seealso \code{\link{reliableTrend}}
#' 
#' @examples 
#' new_reliableTrend()
new_reliableTrend <- function(RCI = double(), 
                              RTI = double(), 
                              pd.RCI = double(),
                              pd.RTI = double(),
                              category.RTI = "Less than reliable", 
                              category.RCI = "Less than reliable",
                              sign.RTI = "Not calculated",
                              sign.difference = "Not calculated",
                              rmaObj = list(), 
                              values = vector(mode = "numeric"), 
                              values.prepost = vector(mode = "numeric"), 
                              error_var = double(),
                              cutpoint = double(),
                              observed = "obs_score",
                              scale_RCI = double() 
                              ){
  
  stopifnot(is.double(RCI))
  stopifnot(is.double(RTI))
  stopifnot(is.double(pd.RCI))
  stopifnot(is.double(pd.RTI))
  category.RTI <- match.arg(category.RTI, 
                            c("Less than reliable", 
                              "Reliable Increase", 
                              "Reliable Decrease"))
  category.RCI <- match.arg(category.RCI, 
                            c("Less than reliable", 
                              "Reliable Increase", 
                              "Reliable Decrease"))
  sign.RTI <- match.arg(sign.RTI, 
                        c("Not calculated", 
                          "Flat", 
                          "Increase", 
                          "Decrease"))
  sign.difference <- match.arg(sign.difference, 
                               c("Not calculated", 
                                 "Flat", 
                                 "Increase", 
                                 "Decrease"))
  stopifnot(is.list(rmaObj))
  stopifnot(is.vector(values))
  stopifnot(is.vector(values.prepost))
  stopifnot(is.double(error_var))
  stopifnot(is.double(cutpoint))
  stopifnot(is.double(scale_RCI))
  stopifnot(is.character(observed))
  
  structure(list(RCI = RCI, 
                 RTI = RTI, 
                 pd.RCI = pd.RCI, 
                 pd.RTI = pd.RTI, 
                 category.RTI = category.RTI,
                 category.RCI = category.RCI,
                 sign.RTI = sign.RTI,
                 sign.difference = sign.difference,
                 values = values, 
                 values.prepost = values.prepost, 
                 error_var = error_var, 
                 cutpoint = cutpoint, 
                 observed = observed,
                 scale_RCI = scale_RCI,
                 rmaObj = rmaObj), 
            class = "reliableTrend")
}

# need to update all of these functions, then the other package features. 



#' Validate reliableTrend objects
#' 
#' An internal function
#'
#' @param x reliableTrend object.
#'
#' @return The same reliable trend object or errors.
#' @seealso \code{\link{reliableTrend}}
#' @export
#'
#' @examples 
#' validate_reliableTrend(new_reliableTrend())
validate_reliableTrend <- function(x) {
  # x is a reliableTrend
  # this should really check that each element is the type it should be. 
  content <- unclass(x)
  names_vals <- names(x)
  
  if(!identical(names_vals, names(new_reliableTrend()))) {
    stop(
      "Based on the names, I am guessing this isn't an object of type `reliableTrend`.",
      call. = FALSE
    )
  }
  x
}

# helper function

#' Create a new reliableTrend object
#' 
#' This exported function creates `reliableTrend` objects using either direct input, 
#' list input, or a standard `rma` object from `{metafor}`. 
#'
#' @param x An object, likely with class `list` or `rma` (or `rma.uni`). If `x` is 
#' provided, several other arguments are overwritten even if they are provided. 
#' @param RCI Numeric. Jacobson & Truax RCI, a transformed difference score.
#' @param RTI Numeric. Reliable Trend Index, the z-value of a linear trend considering a
#' fixed measurement error at each observation. 
#' @param pd.RCI Numeric. 
#' @param pd.RTI Numeric. 
#' @param category.RTI Character. Default is "Less than reliable". One of "Less than reliable", 
#' "Reliable Increase", or "Reliable Decrease".
#' @param category.RCI Character. Default is "Less than reliable". One of "Less than reliable", 
#' "Reliable Increase", or "Reliable Decrease".
#' @param sign.RTI Character. Indicates whether the RTI estimates the trend to be 
#' most likely increasing, decreasing or flat, regardless of confidence level. 
#' Possible values are `"Not calculated"` (the default), `"Increase"`, `"Decrease"`, or `"Flat"`.
#' @param sign.difference Character. Indicates whether the pre-post comparison 
#' is increasing, decreasing or flat, based on simple subtraction of observed scores. 
#' Possible values are `"Not calculated"` (the default), `"Increase"`, `"Decrease"`, or `"Flat"`.
#' @param rmaObj An object of type `rma` and/or `rma.uni`, derived from the `{metafor}`
#' package. 
#' @param values Numeric vector. The values used to compute the RTI.
#' @param values.prepost Numeric vector of length 2. The first and last values.
#' @param error_var Numeric. The error variance used to compute the RCI and RTI. 
#' This is on the variance scale, so is the squared standard error of measurement. 
#' If the Sdiff is available, this value should be `(sdiff/sqrt(2))^2`.
#' @param cutpoint Numeric. Z-scale cutpoint to use for reliability categorization. 
#' Default is 1.96.
#' @param scale_RCI Numeric. The "RCI for a scale," meaning how many scale points
#' need to be observed in a difference score for that difference to be considered
#' "reliable" under J&T. 
#' @param observed Character. Name of the variable of data frame `x` with the observed scores.
#'
#' @return an object of class `reliableTrend`, which is a glorified list. 
#' 
#' To print a very simple summary use print(). For the full values, use summary().
#' @export
#' 
#' 
#'
#' @examples 
#' output <- rti_calc_simple(c(47.5, 32.5), 4.74^2)
#' output
#' reliableTrend(output$rmaObj)
#' output2 <- rti_calc_simple(c(98,98,98,99,99,99), .7071068^2)
#' reliableTrend(output2$rmaObj)
#' reliableTrend()
reliableTrend <- function(x = NULL, 
                          # x is a possible list
                          RCI = double(), 
                          RTI = double(), 
                          pd.RCI = double(),
                          pd.RTI = double(),
                          category.RTI = "Less than reliable", 
                          category.RCI = "Less than reliable",
                          sign.RTI = "Not calculated",
                          sign.difference = "Not calculated",
                          rmaObj = list(), 
                          values = vector(mode = "numeric"), 
                          values.prepost = vector(mode = "numeric"), 
                          error_var = double(),
                          cutpoint = 1.96,
                          scale_RCI = double(), 
                          observed = "obs_score"
) {
  # if x exists but is not an rma object
  # then take the provided variables in x and make them environment
  # variables in the function.
  if(!is.null(x) & !"rma" %in% class(x)) {
    # if x exists but is NOT an rma object
    # need to check for missing inputs and assign the values in x if they exist
    if(exists("RCI", x))                    RCI <- x$RCI 
    if(exists("RTI", x))                    RTI <- x$RTI 
    if(exists("pd.RCI", x))                 pd.RCI <- x$pd.RCI 
    if(exists("pd.RTI", x))                 pd.RTI <- x$pd.RTI 
    if(exists("category.RCI", x))           category.RCI <- x$category.RCI
    if(exists("category.RTI", x))           category.RTI <- x$category.RTI 
    if(exists("sign.RTI", x))               sign.RTI <- x$sign.RTI
    if(exists("sign.difference", x))        sign.difference <- x$sign.difference
    if(exists("rmaObj", x))                 rmaObj <- x$rmaObj 
    # these two rely on each other
    if(exists("values", x) & !exists("values.prepost", x)){
      values <- x$values 
      values.prepost <- c(values[1], values[length(values)])
    }                
    if(exists("values.prepost", x)  & !exists("values", x)){
      values.prepost <- x$values.prepost 
      values <- x$values.prepost 
    }         
    if(exists("values", x) & exists("values.prepost", x)){
      values <- x$values 
      values.prepost <- x$values.prepost 
    } 
    if(exists("error_var", x))              error_var <- x$error_var 
    if(exists("cutpoint", x))               cutpoint <- x$cutpoint 
    if(exists("scale_RCI", x))              scale_RCI <- x$scale_RCI
  }
  # Now all PROVIDED arguments are available as environment variables
  # as long as x is not "rma" class.
  

  # what if x is a data frome?
  # if(missing(rmaObj) & is.null(x$rmaObj)) {
  #   # determine if x or x[1] or x[[1]] is a data.frame.
  #   if(is.data.frame(x[1])){
  #     x <- x[1]
  #   } else if(is.data.frame(x[[1]])){
  #     x <- x[[1]]
  #   }
  #   # function to compute rma object required.
  #   # STILL NEED THIS TO WORK. 
  #   rmaObj <- metafor::rma(data = x, 
  #                            yi = x[[observed]], 
  #                            vi = error_var, 
  #                            method = "FE")
  # }
  
  # what if x is an rma object, and other arguments also provided?
  # what if x is an rma object, but no other arguments are provided?
  
 
    
    # at this point, x is a named list or another reliableTrend object.
    # the environtment variables have taken the entered values
    # there could still be missing values. 
  
  # if x is an rma object
  if(!is.null(x) & any("rma" %in% class(x),
                       "rma" %in% class(x[1]), 
                       "rma" %in% class(x[[1]]))){
    # print("Inferring reliableTrend values from rma object.")
    # in this case x is an rma object. So need to extract the values from it.
    # if they are not provided by the arguments, for some of them
    RTI = x$zval[length(x$zval)] 
    # RCI = NA_real_,
    # pd.RCI = NA_real_, 
    pd.RTI = pnorm(abs(RTI))
    rmaObj = x 
    # for values, if they are provided, check that they are the same
    # as the rma version, then 
    if(missing(values)) {
      values <- as.numeric(x$yi)
    } 
    values.prepost <- c(values[1], values[length(values)])
    if(missing(error_var)) error_var <-  sqrt(unique(x$vi))
    category.RTI <- ifelse(RTI > cutpoint, 
                          "Reliable Increase", 
                          ifelse(RTI < -cutpoint, 
                                 "Reliable Decrease", 
                                 "Less than reliable")) 
    scale_RCI <- error_var*sqrt(2)*cutpoint 
    RCI <- jt_rci_calc(difference = values.prepost[2] - values.prepost[1], 
                           # sdiff = sqrt(error_var))
                       sdiff = error_var * sqrt(2))
    pd.RCI <- pnorm(abs(RCI)) 
    category.RCI <- ifelse(RCI > cutpoint, 
                           "Reliable Increase", 
                           ifelse(RCI < -cutpoint, 
                                  "Reliable Decrease", 
                                  "Less than reliable")) 
    sign.RTI <- ifelse(RTI > 0, 
                           "Increase", 
                           ifelse(RTI < 0, 
                                  "Decrease", 
                                  ifelse(RTI == 0, 
                                         "Flat", 
                                         "Not calculated")))
    sign.difference <- ifelse(values[length(values)] - values[1] > 0, 
                       "Increase", 
                       ifelse(values[length(values)] - values[1] < 0, 
                              "Decrease", 
                              ifelse(values[length(values)] - values[1] == 0, 
                                     "Flat", 
                                     "Not calculated")))
  } 
  
  ## FINAL STEPS
  # convert type for some of the parameters
  # this just makes sure that the inputs are of the right kind, if they are provided. 
  RCI <- as.double(RCI)
  RTI <- as.double(RTI)
  pd.RCI <- as.double(pd.RCI)
  pd.RTI <- as.double(pd.RTI)
  category.RCI <- as.character(category.RCI)
  category.RTI <- as.character(category.RTI)
  sign.RTI <- as.character(sign.RTI)
  sign.difference <- as.character(sign.difference)
  values <- as.numeric(values)
  values.prepost <- c(values[1], values[length(values)])
  error_var <- as.double(error_var)
  cutpoint <- as.numeric(cutpoint)
  scale_RCI <- as.numeric(scale_RCI)
  # return the object as requested.
  validate_reliableTrend(new_reliableTrend(RCI = RCI,
                                           RTI = RTI,
                                           pd.RCI = pd.RCI,
                                           pd.RTI = pd.RTI,
                                           category.RCI = category.RCI,
                                           category.RTI = category.RTI,
                                           sign.RTI = sign.RTI,
                                           sign.difference = sign.difference,
                                           rmaObj = rmaObj,
                                           values = values,
                                           values.prepost = values.prepost,
                                           error_var = error_var,
                                           cutpoint = cutpoint,
                                           observed = observed, 
                                           scale_RCI = scale_RCI))
  
}
# tests
# output2 <- rti_calc_simple(c(98,98,98,99,99,99), .5^2)
# output2
# reliableTrend(output2$rmaObj)
# reliableTrend()
# reliableTrend(list()) # runs into the problem that all of the parameters need to be provided. 
# tester <- rti_calc_simple(values = c(5, 3, 2, 3, 1, 1, 1), .5)
# reliableTrend(tester$rmaObj)
# print function

#' Text summary of a `reliableTrend` object
#'
#' @param object A `reliableTrend` object
#' @param ... Additional arguments. 
#'
#' @return (invisibly) `object`.
#' @export
#'
#' @examples output <- rti_calc_simple(c(98,98,98,99,99,99), .7071068^2)
#' # gives a warning:
#' summary(output)
#' # compute the missing values using this:
#' output2 <- reliableTrend(output$rmaObj)
#' # no warning:
#' summary(output2)
summary.reliableTrend <- function(object, ...){
  if(is.na(object$RCI)) {
    warning("Only parts of this reliableTrend object are valid. Do not trust the summary.")
  }
  # these are articles for use in a text description
  article.RTI <- ifelse(object$category.RTI == "Less than reliable", 
                        " a ", 
                        " a ")
  article.RCI <- ifelse(object$category.RCI == "Less than reliable", 
                        " a ", 
                        " a ")
  cat("\nReliable Trend Analysis:\n\n")
  cat(paste0("This sequence of ", 
             length(object$values), 
             " values has",
             article.RTI,
             object$category.RTI, 
             " using the RTI.\nThe likelihood of an overall ", 
             object$sign.RTI, 
             " in true score is ", 
             round(object$pd.RTI, 5), 
             " using the RTI.\n"))
  cat(paste0("\nA pre-post analysis would have", 
             article.RCI,
             object$category.RCI, 
             " change using the RCI.\nThe likelihood of ", 
             object$sign.difference, 
             " given just the pre-post values is ", 
             round(object$pd.RCI, 5), 
             ".\n"))
  cat("\n")
  invisible(object)
}


#' Print a `reliableTrend` object
#'
#' @param x An object of class `reliableTrend`.
#' @param ... Additional arguments.
#'
#' @return (invisibly) `x`.
#' @export 
#'
#' @examples output <- rti_calc_simple(c(98,98,98,99,99,99), .7071068^2)
#' print(output)
print.reliableTrend <- function(x, ...){
  print.default(unclass(x))
  invisible(x)
}

# method to strip the non-atomic values from the object and return a list
# not a reliableTrend object

#' Strip a `reliableTrend` object of non-atomic entries
#' 
#' Takes an object of type `reliableTrend` and returns a list of atomic values.
#'
#' @param x An object of class `reliableTrend`.
#'
#' @return A list. 
#' @export
#'
#' @examples
#' rti_to_stripped_list(reliableTrend())
rti_to_stripped_list <- function(x) {
  stopifnot(is.reliableTrend(x))
  # x is a reliableTrend
  list(RCI = x$RCI ,
       RTI = x$RTI ,
       pd.RCI = x$pd.RCI ,
       pd.RTI = x$pd.RTI ,
       category.RCI = x$category.RCI,
       category.RTI = x$category.RTI ,
       sign.RTI = x$sign.RTI,
       sign.difference = x$sign.difference,
       error_var = x$error_var ,
       cutpoint = x$cutpoint ,
       scale_RCI = x$scale_RCI, 
       slope.est = x$rmaObj$beta[length(x$rmaObj$beta)], 
       slope.lb = x$rmaObj$ci.lb[length(x$rmaObj$ci.lb)],
       slope.ub = x$rmaObj$ci.ub[length(x$rmaObj$ci.ub)])
}

# same but to a data.frame

#' Convert a `reliableTrend` object to a data.frame via rci_to_stripped_list().
#' 
#' Given an object of class `reliableTrend`, returns a single-row data.frame.
#' Loses all non-atomic values of the `reliableTrend` object.
#'
#' @param x An object of class `reliableTrend`.
#'
#' @return A `data.frame` with 1 row and 12 columns.
#' @export
#'
#' @examples
#' output <- rti_calc_simple(c(98,98,98,99,99,99), .7071068^2)
#' output
#' output2 <- reliableTrend(output$rmaObj)
#' output2
#' rti_to_df(output2)
rti_to_df <- function(x){
  # ensure all values are real before conversion to df, it won't like missings
  data.frame(rti_to_stripped_list(x))
}

#' Verify object claims to be `reliableTrend` class
#'
#' @param x An object to check
#'
#' @return Logical.
#' @export
#'
#' @examples
#' is.reliableTrend(reliableTrend())
#' is.reliableTrend("NOPE")
is.reliableTrend <- function(x){
  "reliableTrend" %in% class(x)
}
