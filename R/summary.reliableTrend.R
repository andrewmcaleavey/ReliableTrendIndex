#' @title Summarize a reliableTrend analysis
#' @description
#' Pretty summary for objects returned by \code{\link{rti}}. Reports the RTI
#' decision and a one-sided probability in the direction of the estimated trend,
#' and compares it to a pre–post (RCI) analysis using the first and last time
#' points. Inference is z-based.
#'
#' @param object An object of class \code{"reliableTrend"} produced by \code{\link{rti}}.
#' @param digits Integer number of digits to display for probabilities (default \code{5}).
#' @param ... Unused; included for S3 compatibility.
#'
#' @return Invisibly returns a list with RTI and RCI details:
#'   \code{list(rti = list(z, zcrit, prob, decision, direction),
#'              rci = list(z, zcrit, prob, decision, direction, sdiff))}.
#'
#' @examples
#' fit <- rti(y = c(12, 11, 13, 16), sd = 8, r = 0.85)
#' summary(fit)
#'
#' # With sdiff precedence (matches your rti() logic):
#' fit2 <- rti(y = c(12, 11, 13, 16), sdiff = 8 * sqrt(2 * (1 - 0.85)))
#' summary(fit2, digits = 4)
#'
#' @export
#' @method summary reliableTrend
summary.reliableTrend <- function(object, digits = 5, ...) {
  if (!inherits(object, "reliableTrend")) {
    stop("`object` must be of class 'reliableTrend' (from rti()).", call. = FALSE)
  }
  
  n      <- object$n
  z      <- object$z
  level  <- if (!is.null(object$level) && is.finite(object$level)) object$level else 0.95
  zcrit  <- stats::qnorm(1 - (1 - level) / 2)
  
  # RTI decision and one-sided probability in the estimated direction
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
  rti_prob <- if (!is.finite(z)) NA_real_ else stats::pnorm(abs(z))  # one-sided, in estimated direction
  
  # --- Pre–post (RCI) using first and last time points ---
  # Use earliest and latest by actual time values (not row order)
  ord <- order(object$t)
  y_first <- object$y[ord][1L]
  y_last  <- object$y[ord][length(ord)]
  diff_y  <- y_last - y_first
  
  # SE of the difference (RCI): sqrt(2 * sigma2).
  # If rti() was called with sdiff, then sigma2 = sdiff^2 / 2, so this reconstructs sdiff exactly.
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
    rci_prob <- stats::pnorm(abs(z_rci))  # one-sided, in estimated direction
  } else {
    z_rci <- NA_real_
    rci_decision <- "RCI unavailable (missing or non-finite variance)"
    rci_dir <- NA_character_
    rci_prob <- NA_real_
  }
  
  # ---- Print summary ----
  cat("\nReliable Trend Analysis:\n\n")
  if (is.finite(n)) {
    cat(sprintf("This sequence of %d values has a %s using the RTI.\n", n, rti_decision))
  } else {
    cat(sprintf("This sequence has a %s using the RTI.\n", rti_decision))
  }
  if (is.finite(rti_prob)) {
    cat(sprintf("The likelihood of an overall %s in true score is %.*f using the RTI.\n\n",
                rti_dir, digits, rti_prob))
  } else {
    cat("The RTI could not be evaluated.\n\n")
  }
  
  cat(sprintf("A pre-post analysis would have a %s using the RCI.\n", rci_decision))
  if (is.finite(rci_prob)) {
    cat(sprintf("The likelihood of %s given just the pre-post values is %.*f.\n",
                rci_dir, digits, rci_prob))
  }
  
  invisible(list(
    rti = list(z = z, zcrit = zcrit, prob = rti_prob,
               decision = rti_decision, direction = rti_dir),
    rci = list(z = z_rci, zcrit = zcrit, prob = rci_prob,
               decision = rci_decision, direction = rci_dir, sdiff = sdiff_used)
  ))
}
