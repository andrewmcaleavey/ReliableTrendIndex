#' Print method for reliableTrend objects
#'
#' @param x A \code{reliableTrend} object (from \code{rti()} or \code{reliableTrend()}).
#' @param digits Number of significant digits to show.
#' @param ... Ignored.
#'
#' @export
#' @method print reliableTrend
print.reliableTrend <- function(x,
                                digits = max(3L, getOption("digits") - 3L),
                                ...) {
  if (!inherits(x, "reliableTrend")) return(NextMethod())
  
  fmt <- function(z) formatC(z, digits = digits, format = "fg")
  
  cat("Reliable Trend Index (reliableTrend)\n")
  cat("n =", x$n,
      " | sd =", if (!is.null(x$sd) && is.finite(x$sd)) fmt(x$sd) else "NA",
      " | r =",  if (!is.null(x$r)  && is.finite(x$r))  fmt(x$r)  else "NA",
      " | sem =", if (!is.null(x$sem) && is.finite(x$sem)) fmt(x$sem) else "NA",
      "\n")
  
  cat("Slope (beta1):", fmt(x$estimate),
      "SE:", fmt(x$se),
      "Z:", fmt(x$z),
      "p:", fmt(x$p), "\n")
  
  invisible(x)
}
