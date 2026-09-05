#' Reliable Change Index (RCI)
#'
#' Compute Jacobson–Truax style RCI values (and variants).
#' This is the supported RCI interface. For the simplest and preferred path,
#' provide \code{difference} and externally derived \code{sdiff}. The other
#' arguments retain supported formula variants and input derivations.
#'
#' @param difference Numeric difference(s). If NULL, computed as `t2 - t1`.
#' @param t1,t2 Numeric vectors for time 1 and time 2.
#' @param scale_rci Numeric. Reliable-change threshold (`qnorm(prob) * sdiff`).
#' @param r1,r2 Numeric in \eqn{[0, 1]}. Reliabilities at time 1 and (optionally) time 2.
#' @param sd1,sd2 Numeric (>0). Group SDs at time 1 and (optionally) time 2.
#' @param sdiff Numeric (>0). Standard error of the difference.
#' @param sem Numeric (>0). Standard error of measurement (single-occasion).
#' @param prob Numeric in \eqn{(0, 1)}. Default 0.975 (≈95% two-sided).
#' @param verbose Logical; if TRUE, return inputs/derivatives.
#' @param rc.type One of \code{"jt"}, \code{"maassen"}, \code{"mcnemar"}
#' @param x1,x2 Optional raw scores; used only by `rci_from_scores()` to
#'   derive `difference = x2 - x1` when `difference` is NULL.
#' @param sd,r Optional aliases for `sd1` and `r1` used by `rci_from_scores()`.
#'
#' @name rci
#' @aliases rci rci_from_scores
#' @section Lifecycle:
#' `rci()` is supported. The aliases `jt_rci_calc()` and
#' `rci_from_scores()` are deprecated and retained for backward compatibility.
#' @export
rci <- function(difference = NULL, t1 = NULL, t2 = NULL,
                scale_rci = NULL, r1 = NULL, r2 = NULL,
                sd1 = NULL, sd2 = NULL, sdiff = NULL, sem = NULL,
                prob = 0.975, verbose = FALSE, rc.type = "jt") {
  
  # ---- basic checks ----
  if (!is.numeric(prob) || length(prob) != 1L || !is.finite(prob) || prob <= 0 || prob >= 1) {
    stop("`prob` must be a single number in (0, 1).", call. = FALSE)
  }
  rc.type <- match.arg(rc.type, c("jt", "maassen", "mcnemar"))
  
  difference <- .rci_resolve_difference(difference, t1, t2)
  resolved <- .rci_resolve_sdiff(sdiff, sem, sd1, sd2, r1, r2, scale_rci,
                                 prob, rc.type)
  sdiff <- resolved$sdiff
  sem <- resolved$sem
  scale_rci <- resolved$scale_rci
  
  # ---- compute RCI ----
  RCI <- difference / sdiff
  
  if (!isTRUE(verbose)) return(RCI)
  
  list(
    RCI        = RCI,
    difference = difference,
    scale_rci  = scale_rci,
    sdiff      = sdiff,
    sem        = sem,
    r1         = r1,
    r2         = r2,
    sd1        = sd1,
    sd2        = sd2,
    prob       = prob,
    rc.type    = rc.type
  )
}
