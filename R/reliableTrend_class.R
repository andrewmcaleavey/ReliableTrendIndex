# new S3 class: reliableTrend


#' Basing on S3, following [Advanced R](https://adv-r.hadley.nz/s3.html#s3-classes). 

# 


#' Constructor function for reliableTrend class objects
#' 
#' Internal function to create objects that are of class `reliableTrend`.
#' This is an S3 class from the package `ReliableTrendIndex`.
#' Following [Advanced R](https://adv-r.hadley.nz/s3.html#s3-classes).
#'
#' @param RCI Numeric. Jacobson & Truax RCI, a transformed difference score.
#' @param RTI Numeric. Reliable Trend Index, the z-value of a linear trend considering a
#' fixed measurement error at each observation. 
#' @param pd.RCI Numeric. 
#' @param pd.RTI Numeric. 
#' @param category.RTI Character. Default is "Unspecified". One of "Unspecified", 
#' "Reliable Increase", or "Reliable Decrease".
#' @param category.RCI Character. Default is "Unspecified". One of "Unspecified", 
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
#' This is on the variance scale, so is the squared standard error of the difference. 
#' @param cutpoint Numeric. Z-scale cutpoint to use for reliability categorization. 
#' Default is 1.96.
#' @param scale_RCI Numeric. The "RCI for a scale," meaning how many scale points
#' need to be observed in a difference score for that difference to be considered
#' "reliable" under J&T.
#'
#' @return An object of type `reliableTrend`. 
#' This object is a glorified list with the above-defined parameters as values. 
#' 
#' @examples 
#' new_reliableTrend()
new_reliableTrend <- function(RCI = double(), 
                              RTI = double(), 
                              pd.RCI = double(),
                              pd.RTI = double(),
                              category.RTI = "Unspecified", 
                              category.RCI = "Unspecified",
                              sign.RTI = "Not calculated",
                              sign.difference = "Not calculated",
                              rmaObj = list(), 
                              values = vector(mode = "numeric"), 
                              values.prepost = vector(mode = "numeric"), 
                              error_var = double(),
                              cutpoint = double(),
                              scale_RCI = double() 
                              ){
  
  stopifnot(is.double(RCI))
  stopifnot(is.double(RTI))
  stopifnot(is.double(pd.RCI))
  stopifnot(is.double(pd.RTI))
  category.RTI <- match.arg(category.RTI, 
                        c("Unspecified", 
                          "Reliable Increase", 
                          "Reliable Decrease"))
  category.RCI <- match.arg(category.RCI, 
                            c("Unspecified", 
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
#'
#' @examples 
#' validate_reliableTrend(new_reliableTrend())
validate_reliableTrend <- function(x) {
  # x is a reliableTrend
  content <- unclass(x)
  names_vals <- names(x)
  
  if(!identical(names_vals, names(new_reliableTrend()))) {
    stop(
      "Based on the names, I am guessing this isn't an object of type `reliableTrend`.",
      call. = FALSE
    )
  }
  
  if(length(x) != length(new_reliableTrend())) {
    stop(
      "Based on the length, I am guessing this isn't an object of type `reliableTrend`.",
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
#' The function will print a message to screen (NOT a warning or error) if this 
#' may happen. 
#' @param RCI Numeric. Jacobson & Truax RCI, a transformed difference score.
#' @param RTI Numeric. Reliable Trend Index, the z-value of a linear trend considering a
#' fixed measurement error at each observation. 
#' @param pd.RCI Numeric. 
#' @param pd.RTI Numeric. 
#' @param category.RTI Character. Default is "Unspecified". One of "Unspecified", 
#' "Reliable Increase", or "Reliable Decrease".
#' @param category.RCI Character. Default is "Unspecified". One of "Unspecified", 
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
#' This is on the variance scale, so is the squared standard error of the difference. 
#' @param cutpoint Numeric. Z-scale cutpoint to use for reliability categorization. 
#' Default is 1.96.
#' @param scale_RCI Numeric. The "RCI for a scale," meaning how many scale points
#' need to be observed in a difference score for that difference to be considered
#' "reliable" under J&T.
#'
#' @return an object of class `reliableTrend`, which is a glorified list. 
#' 
#' To print a very simple summary use print(). For the full values, use summary().
#' @export
#' 
#' 
#'
#' @examples 
#' output2 <- rti_calc_simple(c(98,98,98,99,99,99), .7071068^2)
#' reliableTrend(output2$rmaObj)
#' reliableTrend()
reliableTrend <- function(x = NULL, 
                          # x is a possible list
                          RCI = double(), 
                          RTI = double(), 
                          pd.RCI = double(),
                          pd.RTI = double(),
                          category.RTI = "Unspecified", 
                          category.RCI = "Unspecified",
                          sign.RTI = "Not calculated",
                          sign.difference = "Not calculated",
                          rmaObj = list(), 
                          values = vector(mode = "numeric"), 
                          values.prepost = vector(mode = "numeric"), 
                          error_var = double(),
                          cutpoint = 1.96,
                          scale_RCI = double() 
) {
  # if x exists but is not an rma object
  # then take the provided variables in x and make them environment
  # variables in the function.
  if(!is.null(x) & !"rma" %in% class(x)) {
    # if x exists but is NOT an rma object
    # need to check for missing inputs and assign the values in x if they exist
    if(!missing(x$RCI))                    RCI <- x$RCI 
    if(!missing(x$RTI))                    RTI <- x$RTI 
    if(!missing(x$pd.RCI))                 pd.RCI <- x$pd.RCI 
    if(!missing(x$pd.RTI))                 pd.RTI <- x$pd.RTI 
    if(!missing(x$category.RCI))           category.RCI <- x$category.RCI
    if(!missing(x$category.RTI))           category.RTI <- x$category.RTI 
    if(!missing(x$sign.RTI))               sign.RTI <- x$sign.RTI
    if(!missing(x$sign.difference))        sign.difference <- x$sign.difference
    if(!missing(x$rmaObj))                 rmaObj <- x$rmaObj 
    if(!missing(x$values))                 values <- x$values 
    if(!missing(x$values.prepost))         values.prepost <- x$values.prepost 
    if(!missing(x$error_var))              error_var <- x$error_var 
    if(!missing(x$cutpoint))               cutpoint <- x$cutpoint 
    if(!missing(x$scale_RCI))              scale_RCI <- x$scale_RCI
  }
  # Now all PROVIDED arguments are available as environment variables
  # as long as x is not "rma" class.
  

  
  # what if x is an rma object, and other arguments also provided?
  # what if x is an rma object, but no other arguments are provided?
  
 
    
    # at this point, x is a named list or another reliableTrend object.
    # the environtment variables have taken the entered values
    # there could still be missing values. 
  
  
  if(!is.null(x) & "rma" %in% class(x)){
    print("Inferring reliableTrend values from rma object.")
    # in this case x is an rma object. So need to extract the values from it.
    #if they are not provided by the arguments, for some of them
    RTI = x$zval 
    # RCI = NA_real_,
    # pd.RCI = NA_real_, 
    pd.RTI = pnorm(x$zval)
    rmaObj = x 
    # for values, if they are provided, check that they are the same
    # as the rma version, then 
    if(missing(values)) values <- as.numeric(x$yi) 
    values.prepost <- c(values[1], values[length(values)])
    if(missing(error_var)) error_var <-  unique(x$vi) 
    category.RTI <- ifelse(RTI > cutpoint, 
                          "Reliable Increase", 
                          ifelse(RTI < -cutpoint, 
                                 "Reliable Decrease", 
                                 "Unspecified")) 
    scale_RCI <- sqrt(error_var) * cutpoint 
    RCI <- (values[length(values)] - values[1]) / sqrt(error_var) 
    pd.RCI <- pnorm(RCI) 
    category.RCI <- ifelse(RCI > cutpoint, 
                           "Reliable Increase", 
                           ifelse(RCI < -cutpoint, 
                                  "Reliable Decrease", 
                                  "Unspecified")) 
    sign.RTI <- ifelse(RTI > 0, 
                           "Increase", 
                           ifelse(RTI < 0, 
                                  "Decrease", 
                                  "Flat"))
    sign.difference <- ifelse(RCI > 0, 
                       "Increase", 
                       ifelse(RCI < 0, 
                              "Decrease", 
                              "Flat"))
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
                                           scale_RCI = scale_RCI))
  
}
# tests
# output2 <- rti_calc_simple(c(98,98,98,99,99,99), .7071068^2)
# output2
# reliableTrend(output2$rmaObj)
# reliableTrend()
# reliableTrend(list())# runs into the problem that all of the parameters need to be provided. 

# print function

print.reliableTrend <- function(x){
  cat("\nReliable Trend Analysis:\n\n")
  cat(paste0("This sequence of ", length(x$values), " values has a ", 
             x$category.RTI), "using the RTI.\n")
  cat(paste0("A pre-post analysis would have a(n) ", x$category.RCI), 
      "difference using the RCI.\n")
  cat("\n")
  invisible(x)
}
summary.reliableTrend <- function(x){
  print.default(unclass(x))
}
