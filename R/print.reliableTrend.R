#' Print method for reliableTrend objects
#'
#' @param x A \code{reliableTrend} object (from \code{rti()}).
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
  
  # Reconstruct sem and sdiff from sigma2 when needed
  sigma2 <- x$sigma2
  
  sem <- if (is.numeric(x$sem) && length(x$sem) == 1L && is.finite(x$sem)) {
    x$sem
  } else if (is.numeric(sigma2) && length(sigma2) == 1L && is.finite(sigma2) && sigma2 > 0) {
    sqrt(sigma2)
  } else {
    NA_real_
  }
  
  sdiff <- if (is.numeric(x$sdiff) && length(x$sdiff) == 1L && is.finite(x$sdiff)) {
    x$sdiff
  } else if (is.finite(sem)) {
    sqrt(2) * sem
  } else if (is.numeric(sigma2) && length(sigma2) == 1L && is.finite(sigma2) && sigma2 > 0) {
    sqrt(2 * sigma2)
  } else {
    NA_real_
  }
  
  cat("Reliable Trend Index (reliableTrend)\n")
  cat("n =", x$n,
      "| sd =",   if (is.numeric(x$sd)    && is.finite(x$sd))    fmt(x$sd)    else "NA",
      "| r =",    if (is.numeric(x$r)     && is.finite(x$r))     fmt(x$r)     else "NA",
      "| sem =",  if (is.numeric(sem)     && is.finite(sem))     fmt(sem)     else "NA",
      "| sdiff =",if (is.numeric(sdiff)   && is.finite(sdiff))   fmt(sdiff)   else "NA",
      "\n")
  
  cat("Slope:", fmt(x$estimate),
      "| SE:",  fmt(x$se),
      "| z:",   fmt(x$z),
      "| p:",   fmt(x$p), "\n")
  
  if (is.numeric(x$ci) && length(x$ci) == 2L && all(is.finite(x$ci))) {
    lvl <- if (is.numeric(x$level) && length(x$level) == 1L && is.finite(x$level)) {
      round(100 * x$level)
    } else {
      NA_integer_
    }
    if (is.finite(lvl)) {
      cat(sprintf("%d%% CI for slope: (%s, %s)\n",
                  lvl, fmt(x$ci[1L]), fmt(x$ci[2L])))
    } else {
      cat("CI for slope:", "(", fmt(x$ci[1L]), ",", fmt(x$ci[2L]), ")\n")
    }
  }
  
  invisible(x)
}
