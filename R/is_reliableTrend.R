#' Is object a reliableTrend?
#'
#' Predicate returning \code{TRUE} if \code{x} is an object of class
#' \code{"reliableTrend"} (as produced by \code{\link{rti}}), otherwise \code{FALSE}.
#'
#' @param x Any R object.
#'
#' @return A single logical value.
#'
#' @examples
#' fit <- rti(c(12, 14), sd = 5, r = 0.8)
#' is.reliableTrend(fit)     # TRUE
#' is.reliableTrend(list())  # FALSE
#'
#' @export
is.reliableTrend <- function(x) {
  inherits(x, "reliableTrend")
}
