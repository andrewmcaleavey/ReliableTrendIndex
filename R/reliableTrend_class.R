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
                              values.prepost = vector(mode = "numeric", length = 2), 
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
                          values.prepost = vector(mode = "numeric", 
                                                  length = 2), 
                          error_var = double(),
                          cutpoint = double(),
                          scale_RCI = double()) {
  # if x exists, use those values, otherwise use the parameters
  if(!is.null(x)) {
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
  } else {
    return(validate_reliableTrend(new_reliableTrend(RCI = RCI, 
                                                    RTI = RTI, 
                                                    pd.RCI = pd.RCI, 
                                                    pd.RTI = pd.RTI, 
                                                    category = category, 
                                                    rmaObj = rmaObj, 
                                                    values = values, 
                                                    values.prepost = values.prepost, 
                                                    error_var = error_var, 
                                                    cutpoint = cutpoint, 
                                                    scale_RCI = scale_RCI)))
  }
}
