#' Reliable Change Index (RCI)
#'
#' Compute Jacobson–Truax style RCI values (and variants).
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
  
  # ---- difference from t1/t2 if needed ----
  if (is.null(difference)) {
    if (is.null(t1) || is.null(t2)) {
      stop("Provide either `difference` or both `t1` and `t2`.", call. = FALSE)
    }
    if (!is.numeric(t1) || !is.numeric(t2)) stop("`t1` and `t2` must be numeric.", call. = FALSE)
    if (length(t1) != length(t2)) {
      warning("`t1` and `t2` lengths differ; recycling will be applied.", call. = FALSE)
    }
    difference <- t2 - t1
  }
  if (!is.numeric(difference)) stop("`difference` must be numeric.", call. = FALSE)
  
  # ---- determine sdiff (core) ----
  # precedence: explicit sdiff > (rc.type-specific derivations) > sem > sd1,r1 > scale_rci
  # Note: we do NOT overwrite a provided `sdiff`.
  if (is.null(sdiff)) {
    if (rc.type == "maassen") {
      # heteroscedastic SDs, equal reliability r1 for both times
      if (any(is.null(c(sd1, sd2, r1)))) {
        stop("rc.type = 'maassen' requires `sd1`, `sd2`, and `r1`.", call. = FALSE)
      }
      if (!is.numeric(sd1) || !is.numeric(sd2) || !is.numeric(r1))
        stop("`sd1`, `sd2`, and `r1` must be numeric.", call. = FALSE)
      sdiff <- sqrt((sd1^2 + sd2^2) * (1 - r1))
    } else if (rc.type == "mcnemar") {
      # heteroscedastic SDs and reliabilities
      if (any(is.null(c(sd1, sd2, r1, r2)))) {
        stop("rc.type = 'mcnemar' requires `sd1`, `sd2`, `r1`, and `r2`.", call. = FALSE)
      }
      if (!is.numeric(sd1) || !is.numeric(sd2) || !is.numeric(r1) || !is.numeric(r2))
        stop("`sd1`, `sd2`, `r1`, and `r2` must be numeric.", call. = FALSE)
      sdiff <- sqrt(sd1^2 * (1 - r1) + sd2^2 * (1 - r2))
    }
  }
  
  if (is.null(sdiff)) {
    # JT-style via sem or sd1+r1
    if (!is.null(sem)) {
      if (!is.numeric(sem) || any(!is.finite(sem)) || any(sem <= 0))
        stop("`sem` must be positive numeric.", call. = FALSE)
      sdiff <- sqrt(2) * sem
    } else if (!is.null(sd1) && !is.null(r1)) {
      if (!is.numeric(sd1) || !is.numeric(r1)) stop("`sd1` and `r1` must be numeric.", call. = FALSE)
      if (any(r1 < 0 | r1 > 1)) stop("`r1` must be in [0, 1].", call. = FALSE)
      sem <- sd1 * sqrt(1 - r1)
      sdiff <- sqrt(2) * sem
    }
  }
  
  # fallback: derive sdiff from a provided scale_rci
  if (is.null(sdiff) && !is.null(scale_rci)) {
    if (!is.numeric(scale_rci) || !is.finite(scale_rci) || scale_rci <= 0)
      stop("`scale_rci` must be a single positive number.", call. = FALSE)
    sdiff <- scale_rci / stats::qnorm(prob)
  }
  
  # if still missing, try user-defined helper if present (keeps prior behavior)
  if (is.null(sdiff) && is.null(scale_rci)) {
    if (exists("scale_rci_calc", mode = "function")) {
      scale_rci <- scale_rci_calc(sdiff = sdiff, rxx = r1, sd1 = sd1, sem = sem, prob = prob, verbose = FALSE)
    }
  }
  
  # ensure we now have sdiff (or derive from now-known scale_rci)
  if (is.null(sdiff)) {
    if (!is.null(scale_rci)) {
      sdiff <- scale_rci / stats::qnorm(prob)
    } else {
      stop("Unable to compute `sdiff`. Provide one of: `sdiff`, `sem`, `sd1`+`r1`, or `scale_rci` (with `prob`).", call. = FALSE)
    }
  }
  
  if (!is.numeric(sdiff) || !is.finite(sdiff) || sdiff <= 0) {
    stop("`sdiff` must be a positive numeric value.", call. = FALSE)
  }
  
  # fill scale_rci if absent
  if (is.null(scale_rci)) {
    scale_rci <- stats::qnorm(prob) * sdiff
  }
  
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
