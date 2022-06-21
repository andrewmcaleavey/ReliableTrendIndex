# new S3 class: reliableTrend

#' Want a class to capture my list output, with at least these components:
#' RCI
#' RTI
#' pval
#' direction
#' category
#' rmaObj
#' values
#' prepost values
#' variance
#' cutpoint
#' 
#' Basing on S3, following https://adv-r.hadley.nz/s3.html#s3-classes. 

new_reliableTrend <- function(RCI = double(), 
                              RTI = double(), 
                              pd.RCI = double(),
                              pd.RTI = double(),
                              category = "No reliable change", 
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
  category <- match.arg(category, 
                        c("No reliable change", 
                          "Reliable Increase", 
                          "Reliable Decrease"))
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
                 category = category, 
                 rmaObj = rmaObj, 
                 values = values, 
                 values.prepost = values.prepost, 
                 error_var = error_var, 
                 cutpoint = cutpoint, 
                 scale_RCI = scale_RCI), 
            class = "reliableTrend")
}

# validator
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

# helper

reliableTrend <- function(x = NULL, 
                          # x is a possible list
                          RCI = double(), 
                          RTI = double(), 
                          pd.RCI = double(),
                          pd.RTI = double(),
                          category = "No reliable change", 
                          rmaObj = list(), 
                          values = vector(mode = "numeric"), 
                          values.prepost = vector(mode = "numeric"), 
                          error_var = double(),
                          cutpoint = 1.96,
                          scale_RCI = double()) {
  # convert type for some of the parameters
  RCI <- as.double(RCI)
  RTI <- as.double(RTI)
  pd.RCI <- as.double(pd.RCI)
  pd.RTI <- as.double(pd.RTI)
  category <- as.character(category)
  values <- as.numeric(values)
  values.prepost <- c(values[1], values[length(values)])
  
  
  # if x exists, check if is rma or just a list. 
  # use the rma object to do computations, 
  # or use the list values, otherwise use the parameters from the function
  if(!is.null(x) & !"rma" %in% class(x)) {
    # if x exists but is not an rma object
    # need to check for missing inputs and make them appropriately 
    
    return(validate_reliableTrend(new_reliableTrend(RCI = x$RCI, 
                                                    RTI = x$RTI, 
                                                    pd.RCI = x$pd.RCI, 
                                                    pd.RTI = x$pd.RTI, 
                                                    category = x$category, 
                                                    rmaObj = x$rmaObj, 
                                                    values = x$values, 
                                                    values.prepost = x$values.prepost, 
                                                    error_var = x$error_var, 
                                                    cutpoint = x$cutpoint, 
                                                    scale_RCI = x$scale_RCI)))
  } else if(!is.null(x) & "rma" %in% class(x)){
    # in this case x is an rma object. So need to extract the values from it.
    new_reliableTrend(RTI = x$zval, 
                      # RCI = NA_real_,
                      pd.RCI = NA_real_, 
                      pd.RTI = NA_real_,
                      rmaObj = x, 
                      values = as.numeric(x$yi), 
                      values.prepost <- c(as.numeric(x$yi)[1], as.numeric(x$yi)[length(as.numeric(x$yi))]),
                      error_var = unique(x$vi), 
                      category = ifelse(x$zval > cutpoint, 
                                         "Reliable Increase", 
                                         ifelse(x$zval < -cutpoint, 
                                                "Reliable Decrease", 
                                                "No Reliable Change")), 
                      scale_RCI = sqrt(unique(x$vi)) * cutpoint, 
                      RCI = (as.numeric(x$yi)[length(as.numeric(x$yi))] - as.numeric(x$yi)[1]) / sqrt(unique(x$vi)))
  } else {
    # if the arguments are passed directly without a list, 
    # should be able to take them in. 
    return(validate_reliableTrend(new_reliableTrend(RCI = as.double(RCI), 
                                                    RTI = as.double(RTI), 
                                                    pd.RCI = as.double(pd.RCI), 
                                                    pd.RTI = as.double(pd.RTI), 
                                                    category = as.character(category), 
                                                    rmaObj = rmaObj, 
                                                    values = values, 
                                                    values.prepost = values.prepost, 
                                                    error_var = error_var, 
                                                    cutpoint = cutpoint, 
                                                    scale_RCI = scale_RCI)))
  }
}
# tests
# output2 <- rti_calc_simple(c(98,98,98,99,99,99), .7071068^2)
# output2
# reliableTrend(output2$rmaObj)
# reliableTrend()
# reliableTrend(list())# runs into the problem that all of the parameters need to be provided. 
