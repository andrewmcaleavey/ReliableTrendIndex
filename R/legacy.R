# ---- legacy.R : Back-compat helpers and metafor-based utilities ----------------

#' @title Legacy Compatibility Layer (metafor + RCI/SDiff helpers)
#' @description
#' Functions retained for backward compatibility with older versions of the
#' package and materials. Prefer the new API:
#' - Use [rti()] for single-case RTI.
#' - Use [rti_by()] for grouped data.
#' - Use [plot.reliableTrend()] for visualization.
#'
#' This file provides:
#' - Simple SEM/SDiff calculators and RCI helpers.
#' - A legacy `reliableTrend()` wrapper (supports `sem=` argument).
#' - `rti_to_df()` to coerce a fit to a 1-row data frame (used in older vignettes).
#' - A thin `simple_rma()` wrapper around metafor (optional backend).
#'
#' @keywords internal
NULL

# ---- SDiff / SEM / RCI utilities ---------------------------------------------

#' Standard Error of Measurement from sd & r
#' @param sd single-occasion SD
#' @param r reliability in [0,1]
#' @return SEM = sd * sqrt(1 - r)
#' @export
sem_from_sd_r <- function(sd, r) {
  if (!is.numeric(sd) || length(sd) != 1L || sd <= 0 || !is.finite(sd))
    stop("`sd` must be a single positive number.", call. = FALSE)
  if (!is.numeric(r) || length(r) != 1L || r < 0 || r > 1 || !is.finite(r))
    stop("`r` must be a single number in [0,1].", call. = FALSE)
  sd * sqrt(1 - r)
}

#' SDiff from SEM
#' @param sem standard error of measurement
#' @return SDiff = sqrt(2) * SEM
#' @export
sdiff_from_sem <- function(sem) {
  if (!is.numeric(sem) || length(sem) != 1L || sem <= 0 || !is.finite(sem))
    stop("`sem` must be a single positive number.", call. = FALSE)
  sqrt(2) * sem
}

#' SDiff from sd & r
#' @inheritParams sem_from_sd_r
#' @return SDiff = sd * sqrt(2 * (1 - r))
#' @export
sdiff_from_sd_r <- function(sd, r) {
  if (!is.numeric(sd) || length(sd) != 1L || sd <= 0 || !is.finite(sd))
    stop("`sd` must be a single positive number.", call. = FALSE)
  if (!is.numeric(r) || length(r) != 1L || r < 0 || r > 1 || !is.finite(r))
    stop("`r` must be a single number in [0,1].", call. = FALSE)
  sd * sqrt(2 * (1 - r))
}

#' Jacobson–Truax RCI from difference & SDiff
#' @param difference observed pre–post difference (x2 - x1)
#' @param sdiff standard error of the difference score
#' @return numeric RCI = difference / sdiff
#' @export
jt_rci_calc <- function(difference, sdiff) {
  if (!is.numeric(difference) || length(difference) != 1L || !is.finite(difference))
    stop("`difference` must be a single finite number.", call. = FALSE)
  if (!is.numeric(sdiff) || length(sdiff) != 1L || sdiff <= 0 || !is.finite(sdiff))
    stop("`sdiff` must be a single positive number.", call. = FALSE)
  difference / sdiff
}

#' Flexible RCI helper
#' @param x1,x2 optional two raw scores; if provided and \code{difference} is NULL,
#'   then \code{difference = x2 - x1}.
#' @param difference optional pre–post difference (used if provided).
#' @param sd,r optional to compute SDiff via \code{sdiff_from_sd_r()}.
#' @param sem,sdiff optional direct error terms; precedence: \code{sdiff} > \code{sem} > (\code{sd},\code{r}).
#' @return numeric RCI
#' @export
rci_from_scores <- function(x1 = NULL, x2 = NULL, difference = NULL,
                            sd = NULL, r = NULL, sem = NULL, sdiff = NULL) {
  if (is.null(difference)) {
    if (is.null(x1) || is.null(x2))
      stop("Provide either `difference` or both `x1` and `x2`.", call. = FALSE)
    difference <- x2 - x1
  }
  if (!is.null(sdiff)) {
    return(jt_rci_calc(difference, sdiff))
  }
  if (!is.null(sem)) {
    return(jt_rci_calc(difference, d <- sdiff_from_sem(sem)))
  }
  if (!is.null(sd) && !is.null(r)) {
    return(jt_rci_calc(difference, d <- sdiff_from_sd_r(sd, r)))
  }
  stop("Need one of: `sdiff`, `sem`, or both `sd` & `r`.", call. = FALSE)
}

#' Convenience RCI wrapper
#' @inheritParams rci_from_scores
#' @export
rci <- function(x1 = NULL, x2 = NULL, difference = NULL,
                sd = NULL, r = NULL, sem = NULL, sdiff = NULL) {
  rci_from_scores(x1 = x1, x2 = x2, difference = difference, sd = sd, r = r, sem = sem, sdiff = sdiff)
}

# ---- Legacy reliableTrend() wrapper (supports `sem=`) -------------------------

#' Legacy constructor: reliableTrend()
#'
#' Backwards-compatible wrapper that accepts either `sd` & `r` *or* `sem`
#' (standard error of measurement). Returns a `"reliableTrend"` object shaped
#' like [rti()], so new methods (plot/predict/print) will work.
#'
#' @param values,y numeric vector of outcomes
#' @param time,t optional numeric vector of time indices (equal spacing used if NULL)
#' @param sd,r external single-occasion SD and reliability
#' @param sem external standard error of measurement (SEM). If provided, takes precedence
#'   over \code{sd}/\code{r} and uses \eqn{\sigma^2 = \mathrm{SEM}^2} directly.
#' @param na.rm drop non-finite pairs?
#' @param level confidence level for slope intervals
#' @return object of class `"reliableTrend"`
#' @export
reliableTrend <- function(values = NULL, y = NULL, time = NULL, t = NULL,
                          sd = NULL, r = NULL, sem = NULL,
                          na.rm = FALSE, level = 0.95) {
  # prefer `values`/`time` if given
  if (!is.null(values) && is.null(y)) y <- values
  if (!is.null(time)   && is.null(t)) t <- time
  if (is.null(y)) stop("Provide `values` or `y`.", call. = FALSE)
  if (is.null(t)) t <- seq_along(y)
  
  # handle missingness
  keep <- is.finite(y) & is.finite(t)
  if (!all(keep)) {
    if (!na.rm) stop("Missing or non-finite values in `y`/`t`. Set `na.rm = TRUE` to drop.", call. = FALSE)
    y <- y[keep]; t <- t[keep]
    warning("Dropped ", sum(!keep), " non-finite observations.", call. = FALSE)
  }
  n <- length(y)
  if (n < 2L) stop("Need at least 2 finite observations.", call. = FALSE)
  
  # center time and compute OLS slope
  tbar <- mean(t)
  tc   <- t - tbar
  Sxx  <- sum(tc^2)
  if (Sxx <= 0) stop("Degenerate time vector: Sxx = 0.", call. = FALSE)
  
  beta1 <- sum(tc * y) / Sxx
  beta0 <- mean(y)
  
  # choose sigma^2 source
  sigma2 <- if (!is.null(sem)) {
    if (!is.numeric(sem) || length(sem) != 1L || sem <= 0 || !is.finite(sem))
      stop("`sem` must be a single positive number.", call. = FALSE)
    sem^2
  } else {
    if (is.null(sd) || is.null(r))
      stop("Provide either `sem`, or both `sd` and `r`.", call. = FALSE)
    if (!is.numeric(sd) || length(sd) != 1L || sd <= 0 || !is.finite(sd))
      stop("`sd` must be a single positive number.", call. = FALSE)
    if (!is.numeric(r) || length(r) != 1L || r < 0 || r > 1 || !is.finite(r))
      stop("`r` must be a single number in [0,1].", call. = FALSE)
    sd^2 * (1 - r)
  }
  
  se_beta1 <- sqrt(sigma2 / Sxx)
  z <- beta1 / se_beta1
  p <- 2 * stats::pnorm(-abs(z))
  zcrit <- stats::qnorm(1 - (1 - level) / 2)
  ci <- c(beta1 - zcrit * se_beta1, beta1 + zcrit * se_beta1)
  
  out <- list(
    estimate = beta1,
    intercept = beta0,
    se = se_beta1,
    z = z,
    p = p,
    ci = ci,
    sigma2 = sigma2,
    t = as.numeric(t),
    t_centered = as.numeric(tc),
    y = as.numeric(y),
    Sxx = Sxx,
    n = n,
    sd = if (is.null(sem)) sd else NA_real_,
    r = if (is.null(sem)) r else NA_real_,
    level = level,
    call = match.call()
  )
  class(out) <- "reliableTrend"
  out
}

# ---- Legacy summarizer used in old pipelines ----------------------------------

#' Coerce a reliableTrend to a (legacy) one-row data frame
#' @param x reliableTrend object
#' @return data.frame with slope estimate, CI, test stats and convenience fields
#' @export
rti_to_df <- function(x) {
  if (!inherits(x, "reliableTrend")) stop("`x` must be a reliableTrend.", call. = FALSE)
  data.frame(
    slope.est = x$estimate,
    slope.lb  = x$ci[1L],
    slope.ub  = x$ci[2L],
    z         = x$z,
    p         = x$p,
    n         = x$n,
    Sxx       = x$Sxx,
    sigma2    = x$sigma2,
    # legacy categorical label used in examples
    category.RTI = if (x$ci[1L] > 0) "Reliable Increase" else if (x$ci[2L] < 0) "Reliable Decrease" else "Less than reliable",
    stringsAsFactors = FALSE
  )
}

# ---- Optional metafor shim ----------------------------------------------------

#' Thin wrapper around metafor::rma()
#'
#' Provided for legacy workflows that expected a simple rma frontend.
#' This function only runs if the **metafor** package is available.
#'
#' @param yi numeric vector of effects
#' @param vi,sei sampling variances or standard errors (one of them)
#' @param method estimation method (default "FE")
#' @param ... passed to \code{metafor::rma()}
#' @return the \code{metafor::rma} fit object (invisible if metafor not available)
#' @export
simple_rma <- function(yi, vi = NULL, sei = NULL, method = "FE", ...) {
  if (!requireNamespace("metafor", quietly = TRUE)) {
    stop("`metafor` is not installed. Install it to use `simple_rma()`.", call. = FALSE)
  }
  if (is.null(vi) && is.null(sei)) stop("Provide either `vi` or `sei`.", call. = FALSE)
  if (!is.null(vi) && !is.null(sei)) stop("Provide only one of `vi` or `sei` (not both).", call. = FALSE)
  if (!is.null(sei)) vi <- sei^2
  metafor::rma(yi = yi, vi = vi, method = method, ...)
}

# ---- Very small adapter for older grouped helper names ------------------------

#' Legacy alias: rti_by_person()
#'
#' Older materials referred to a grouped helper named \code{rti_by_person()}.
#' This adapter calls [rti_by()]. If you supply a scalar \code{sem}, it will be
#' used for all ids (equivalent to \eqn{\sigma^2=\mathrm{SEM}^2}).
#'
#' @param data,id,time,y see [rti_by()]
#' @param sd,r per-id error parameters (scalars or columns)
#' @param sem optional scalar SEM (ignored if \code{sd}/\code{r} are supplied)
#' @param ... ignored
#' @export
rti_by_person <- function(data, id, time, y, sd = NULL, r = NULL, sem = NULL, ..., na.rm = FALSE, level = 0.95) {
  if (!is.null(sem) && (is.null(sd) || is.null(r))) {
    # SEM provided; compute per-id via reliableTrend() with sem
    id_sym   <- substitute(id)
    time_sym <- substitute(time)
    y_sym    <- substitute(y)
    id_vec   <- .legacy_get_col(data, id_sym)
    time_vec <- .legacy_get_col(data, time_sym)
    y_vec    <- .legacy_get_col(data, y_sym)
    
    idx_list <- split(seq_len(nrow(data)), id_vec, drop = TRUE)
    rows <- lapply(names(idx_list), function(k) {
      idx <- idx_list[[k]]
      fit <- reliableTrend(values = y_vec[idx], time = time_vec[idx], sem = sem, na.rm = na.rm, level = level)
      df  <- rti_to_df(fit)
      df$id <- k
      df
    })
    out <- do.call(rbind, rows)
    out$fit <- lapply(seq_len(nrow(out)), function(i) {
      # reconstruct minimal fit for convenience
      reliableTrend(values = y_vec[idx_list[[ out$id[i] ]]], time = time_vec[idx_list[[ out$id[i] ]]], sem = sem, na.rm = na.rm, level = level)
    })
    # order minimal columns
    out <- out[, c("id","slope.est","slope.lb","slope.ub","z","p","n","Sxx","sigma2","category.RTI","fit")]
    rownames(out) <- NULL
    return(out)
  }
  # else: just defer to the modern helper
  rti_by(data, id = id, time = time, y = y, sd = sd, r = r, na.rm = na.rm, level = level)
}

# internal: bare/character column accessor without evaluation
.legacy_get_col <- function(data, expr) {
  if (is.symbol(expr)) {
    nm <- as.character(expr)
    if (!nm %in% names(data)) stop("Column `", nm, "` not found in `data`.", call. = FALSE)
    return(data[[nm]])
  }
  if (is.character(expr) && length(expr) == 1L) {
    nm <- expr
    if (!nm %in% names(data)) stop("Column `", nm, "` not found in `data`.", call. = FALSE)
    return(data[[nm]])
  }
  stop("Provide a column name (bare or string).", call. = FALSE)
}

# ---- End legacy.R -------------------------------------------------------------
