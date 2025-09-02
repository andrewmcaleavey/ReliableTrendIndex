# ---- legacy.R : Back-compat helpers and metafor-based utilities --------------

#' Legacy Compatibility Layer (metafor + RCI/SDiff helpers)
#'
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
#' @name legacy_compat
#' @noRd
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
#'
#' @name rci               # <<—— canonical page name
#' @aliases rci_from_scores rci
#' #' @export
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

#' RCI convenience wrapper
#'
#' @rdname rci
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
  if (!is.null(values) && is.null(y)) y <- values
  if (!is.null(time)   && is.null(t)) t <- time
  if (is.null(y)) stop("Provide `values` or `y`.", call. = FALSE)
  if (is.null(t)) t <- seq_along(y)
  
  keep <- is.finite(y) & is.finite(t)
  if (!all(keep)) {
    if (!na.rm) stop("Missing or non-finite values in `y`/`t`. Set `na.rm = TRUE` to drop.", call. = FALSE)
    y <- y[keep]; t <- t[keep]
    warning("Dropped ", sum(!keep), " non-finite observations.", call. = FALSE)
  }
  n <- length(y)
  if (n < 2L) stop("Need at least 2 finite observations.", call. = FALSE)
  
  tbar <- mean(t)
  tc   <- t - tbar
  Sxx  <- sum(tc^2)
  if (Sxx <= 0) stop("Degenerate time vector: Sxx = 0.", call. = FALSE)
  
  beta1 <- sum(tc * y) / Sxx
  beta0 <- mean(y)
  
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
#'
#' Adds legacy fields expected by older vignettes:
#' - category.RTI (from slope CI)
#' - category.RCI (from pre-post RCI using SEM or sd/r)
#' - pd.RTI, pd.RCI (simple "confidence in either change" = 1 - p_two_sided)
#'
#' @param x reliableTrend object
#' @return data.frame with slope estimate, CI, test stats and legacy fields
#' @export
rti_to_df <- function(x) {
  if (!inherits(x, "reliableTrend")) stop("`x` must be a reliableTrend.", call. = FALSE)
  
  # RTI pieces
  slope.est <- x$estimate
  slope.lb  <- x$ci[1L]
  slope.ub  <- x$ci[2L]
  z_rti     <- x$z
  p_rti     <- x$p
  category.RTI <- if (slope.lb > 0) "Reliable Increase" else if (slope.ub < 0) "Reliable Decrease" else "Less than reliable"
  pd.RTI <- 1 - p_rti  # "confidence" in either change (legacy plots used this)
  
  # -- RCI pieces from pre vs post (order by time) --
  ord <- order(x$t, method = "radix")
  y_ord <- x$y[ord]
  if (length(y_ord) >= 2L && all(is.finite(y_ord))) {
    diff_prepost <- y_ord[length(y_ord)] - y_ord[1L]
    
    # prefer SEM if available; else compute from sd & r; else NA
    if (!is.null(x$sem) && is.finite(x$sem)) {
      sem  <- x$sem
      sdiff <- sqrt(2) * sem
    } else if (!is.null(x$sd) && is.finite(x$sd) && !is.null(x$r) && is.finite(x$r)) {
      sem  <- x$sd * sqrt(1 - x$r)
      sdiff <- sqrt(2) * sem
    } else {
      sem <- NA_real_; sdiff <- NA_real_
    }
    
    if (is.finite(sdiff) && sdiff > 0) {
      rci_val <- diff_prepost / sdiff
      p_rci   <- 2 * stats::pnorm(-abs(rci_val))
      pd.RCI  <- 1 - p_rci
      category.RCI <- if (rci_val >  1.96) "Reliable Increase"
      else if (rci_val < -1.96) "Reliable Decrease"
      else "Less than reliable"
    } else {
      rci_val <- NA_real_; p_rci <- NA_real_; pd.RCI <- NA_real_; category.RCI <- NA_character_
    }
  } else {
    rci_val <- NA_real_; p_rci <- NA_real_; pd.RCI <- NA_real_; category.RCI <- NA_character_
  }
  
  data.frame(
    slope.est = slope.est,
    slope.lb  = slope.lb,
    slope.ub  = slope.ub,
    z         = z_rti,
    p         = p_rti,
    n         = x$n,
    Sxx       = x$Sxx,
    sigma2    = x$sigma2,
    # legacy classification + "confidence" columns expected in old vignettes
    category.RTI = category.RTI,
    category.RCI = category.RCI,
    pd.RTI = pd.RTI,
    pd.RCI = pd.RCI,
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
#' @return the \code{metafor::rma} fit object
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

# ---- Adapter for older grouped helper names -----------------------------------

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
      reliableTrend(values = y_vec[idx_list[[ out$id[i] ]]], time = time_vec[idx_list[[ out$id[i] ]]], sem = sem, na.rm = na.rm, level = level)
    })
    out <- out[, c("id","slope.est","slope.lb","slope.ub","z","p","n","Sxx","sigma2","category.RTI","fit")]
    rownames(out) <- NULL
    return(out)
  }
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

#' Legacy helper: rti_calc_simple
#'
#' Minimal wrapper used in old examples and vignettes. Computes an RTI fit
#' from a vector of scores and a single SEM (standard error of measurement).
#' Returns a list containing fields older examples expect:
#' - rmaObj: the model object (here we store the reliableTrend fit)
#' - error_var: sigma^2 used (i.e., sem^2)
#'
#' @param values Numeric vector of within-person observations (length >= 2).
#' @param sem Positive numeric scalar: standard error of measurement.
#' @param time Optional numeric vector of time indices (same length as \code{values});
#'   if omitted, uses \code{1:length(values)}.
#' @param level Confidence level for slope intervals (default 0.95).
#' @param na.rm Logical; drop non-finite \code{(values, time)} pairs? Default \code{FALSE}.
#'
#' @return A list with \code{rmaObj} (the reliableTrend fit), \code{error_var} (sem^2),
#'   and \code{fit} (alias to the reliableTrend), for legacy compatibility.
#' @examples
#' rti_calc_simple(c(47.5, 32.5), sem = 3.35)
#' @export
rti_calc_simple <- function(values, sem, time = NULL, level = 0.95, na.rm = FALSE) {
  if (is.null(time)) time <- seq_along(values)
  fit <- rti(values = values, time = time, sem = sem, level = level, na.rm = na.rm)
  out <- list(
    rmaObj    = fit,            # legacy name expected by examples
    error_var = fit$sigma2,     # sem^2
    fit       = fit             # convenient alias
  )
  class(out) <- "rti_calc_simple"
  out
}

#' Turning metafor-style forest results into a regression-style plot
#'
#' Works with either a \strong{metafor} \code{rma*} object (if metafor is installed)
#' or a \code{reliableTrend} object (from \code{rti()} / \code{reliableTrend()}).
#'
#' @param x A \code{reliableTrend} or a \code{metafor::rma*} object.
#' @param StError Optional standard error input used by some legacy calls; ignored
#'   when \code{x} is a \code{reliableTrend}.
#' @param level Confidence level for intervals (default 0.95).
#' @param ... Ignored.
#' @return A \code{ggplot} object.
#' @examples
#' test <- rti_calc_simple(c(47.5, 32.5), 3.35)
#' forest_to_reg_plot(test$rmaObj, StError = sqrt(test$error_var))
#' @export
forest_to_reg_plot <- function(x, StError = NULL, level = 0.95, ...) {
  # reliableTrend path (new backend; predict.reliableTrend handles intervals)
  if (inherits(x, "reliableTrend")) {
    preds <- predict(x, interval = "mean", level = level, include_intercept_uncertainty = TRUE)
    df <- data.frame(
      t   = x$t,
      y   = x$y,
      fit = preds$fit,
      lwr = preds$lwr,
      upr = preds$upr
    )
    p <- ggplot2::ggplot(df, ggplot2::aes(x = t, y = y)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = lwr, ymax = upr), alpha = 0.15) +
      ggplot2::geom_line(ggplot2::aes(y = fit)) +
      ggplot2::geom_point() +
      ggplot2::labs(x = "Time", y = "Outcome") +
      ggplot2::theme_minimal()
    return(p)
  }
  
  # metafor path (legacy backend)
  if (inherits(x, c("rma", "rma.uni", "rma.mv"))) {
    if (!requireNamespace("metafor", quietly = TRUE)) {
      stop("`metafor` not installed but an rma object was provided.", call. = FALSE)
    }
    pr <- as.data.frame(stats::predict(x, level = level))
    # If predict() provides 'cr.lb'/'cr.ub' or other names, fall back sensibly
    lwr <- if ("ci.lb" %in% names(pr)) pr$ci.lb else if ("cr.lb" %in% names(pr)) pr$cr.lb else pr[, grep("\\.lb$", names(pr))[1]]
    upr <- if ("ci.ub" %in% names(pr)) pr$ci.ub else if ("cr.ub" %in% names(pr)) pr$cr.ub else pr[, grep("\\.ub$", names(pr))[1]]
    df <- data.frame(idx = seq_len(nrow(pr)), fit = pr$pred, lwr = lwr, upr = upr)
    
    p <- ggplot2::ggplot(df, ggplot2::aes(x = idx, y = fit)) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
      ggplot2::geom_pointrange(ggplot2::aes(ymin = lwr, ymax = upr)) +
      ggplot2::labs(x = "Study / Time", y = "Effect") +
      ggplot2::theme_minimal()
    return(p)
  }
  
  stop("Unsupported object for forest_to_reg_plot(): provide a 'reliableTrend' or a 'metafor' rma object.", call. = FALSE)
}



# ---- End legacy.R -------------------------------------------------------------
