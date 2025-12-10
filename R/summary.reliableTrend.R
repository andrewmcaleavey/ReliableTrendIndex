#' Summarize a reliableTrend analysis
#'
#' Pretty summary for objects returned by \code{\link{rti}}. Reports the RTI
#' decision and a one-sided probability in the direction of the estimated trend,
#' and compares it to a pre-post (RCI) analysis using the first and last time
#' points. Inference is z-based.
#'
#' @param object An object of class \code{"reliableTrend"} produced by \code{\link{rti}}.
#' @param ... Unused; included for S3 compatibility. Can pass \code{digits}.
#' @param digits Integer number of digits to display for probabilities (default \code{5}).
#'
#' @return Invisibly returns a list with RTI and RCI details:
#'   \code{list(rti = list(z, zcrit, prob, decision, direction),
#'              rci = list(z, zcrit, prob, decision, direction, sdiff))}.
#'
#' @export
#' @method summary reliableTrend
summary.reliableTrend <- function(object, ..., digits = 5) {
  if (!inherits(object, "reliableTrend")) {
    stop("`object` must be of class 'reliableTrend' (from rti()).", call. = FALSE)
  }
  
  n      <- object$n
  z      <- object$z
  level  <- if (!is.null(object$level) && is.finite(object$level)) object$level else 0.95
  zcrit  <- stats::qnorm(1 - (1 - level) / 2)
  
  ## --- RTI decision and one-sided probability in the estimated direction ---
  rti_decision <- if (!is.finite(z)) {
    "Undetermined"
  } else if (z >= zcrit) {
    "Reliable Increase"
  } else if (z <= -zcrit) {
    "Reliable Decrease"
  } else {
    "Less than reliable trend"
  }
  rti_dir <- if (!is.finite(z)) NA_character_ else if (z >= 0) "Increase" else "Decrease"
  rti_prob <- if (!is.finite(z)) NA_real_ else stats::pnorm(abs(z))  # one-sided
  
  ## --- Pre-post (RCI) using first and last time points ---
  ord <- order(object$t)
  y_first <- object$y[ord][1L]
  y_last  <- object$y[ord][length(ord)]
  diff_y  <- y_last - y_first
  
  # SE of the difference (RCI): sqrt(2 * sigma2)
  sdiff_used <- if (is.finite(object$sigma2)) sqrt(2 * object$sigma2) else NA_real_
  
  if (is.finite(sdiff_used) && sdiff_used > 0) {
    z_rci <- diff_y / sdiff_used
    rci_decision <- if (z_rci >= zcrit) {
      "Reliable Increase"
    } else if (z_rci <= -zcrit) {
      "Reliable Decrease"
    } else {
      "Less than reliable change"
    }
    rci_dir  <- if (z_rci >= 0) "Increase" else "Decrease"
    rci_prob <- stats::pnorm(abs(z_rci))  # one-sided
  } else {
    z_rci <- NA_real_
    rci_decision <- "RCI unavailable (missing or non-finite variance)"
    rci_dir <- NA_character_
    rci_prob <- NA_real_
  }
  
  ## ---- Printed summary ----
  cat("\nReliable Trend Analysis\n")
  cat("-----------------------\n")
  
  if (is.finite(n)) {
    cat(sprintf("Sequence length (n): %d\n\n", n))
  }
  
  cat("RTI (trend across all time points):\n")
  cat(sprintf("  z = %.3f, z_crit = %.3f\n", z, zcrit))
  cat(sprintf("  Decision: %s\n", rti_decision))
  if (is.finite(rti_prob)) {
    cat(sprintf("  One-sided probability of %s: %.*f\n\n",
                rti_dir, digits, rti_prob))
  } else {
    cat("  RTI probability could not be evaluated.\n\n")
  }
  
  cat("RCI (pre-post, first vs last time point):\n")
  cat(sprintf("  Difference (last - first): %.3f\n", diff_y))
  if (is.finite(z_rci)) {
    cat(sprintf("  z = %.3f, z_crit = %.3f\n", z_rci, zcrit))
    cat(sprintf("  Decision: %s\n", rci_decision))
    if (is.finite(rci_prob)) {
      cat(sprintf("  One-sided probability of %s: %.*f\n",
                  rci_dir, digits, rci_prob))
    }
  } else {
    cat("  RCI unavailable (missing or non-finite variance).\n")
  }
  
  invisible(list(
    rti = list(z = z, zcrit = zcrit, prob = rti_prob,
               decision = rti_decision, direction = rti_dir),
    rci = list(z = z_rci, zcrit = zcrit, prob = rci_prob,
               decision = rci_decision, direction = rci_dir, sdiff = sdiff_used)
  ))
}
