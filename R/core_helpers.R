# Internal computation helpers for the supported RTI/RCI API.
#
# Keeping validation and formulae here lets the public entry points share one
# implementation while preserving their existing return values.

.rti_prepare_data <- function(y, t, na.rm) {
  if (is.null(y) || !is.numeric(y) || length(y) < 2L) {
    stop("`y` (or legacy `values`) must be numeric with length >= 2.", call. = FALSE)
  }
  if (is.null(t)) t <- seq_along(y)
  if (!is.numeric(t) || length(t) != length(y)) {
    stop("`t` (or legacy `time`) must be numeric and the same length as `y`.", call. = FALSE)
  }

  keep <- is.finite(y) & is.finite(t)
  if (!all(keep)) {
    if (!na.rm) {
      stop("Missing or non-finite values in `y`/`t`. Set `na.rm = TRUE` to drop.", call. = FALSE)
    }
    y <- y[keep]
    t <- t[keep]
    warning("Dropped ", sum(!keep), " non-finite observations.", call. = FALSE)
  }
  if (length(y) < 2L) {
    stop("Need at least 2 finite observations after dropping.", call. = FALSE)
  }

  tc <- t - mean(t)
  Sxx <- sum(tc^2)
  if (Sxx <= 0) {
    stop("Degenerate time vector: Sxx = 0. Time points must vary.", call. = FALSE)
  }

  list(y = as.numeric(y), t = as.numeric(t), t_centered = as.numeric(tc), Sxx = Sxx)
}

.rti_measurement_variance <- function(sd, r, sem, sdiff) {
  # Precedence is part of the public contract: sdiff > sem > sd/r.
  if (!is.null(sdiff)) {
    if (!is.numeric(sdiff) || length(sdiff) != 1L || !is.finite(sdiff) || sdiff <= 0) {
      stop("`sdiff` must be a single positive, finite number.", call. = FALSE)
    }
    return(list(sigma2 = sdiff^2 / 2, sd = NA_real_, r = NA_real_,
                sem = NA_real_, sdiff = sdiff))
  }
  if (!is.null(sem)) {
    if (!is.numeric(sem) || length(sem) != 1L || !is.finite(sem) || sem <= 0) {
      stop("`sem` must be a single positive, finite number.", call. = FALSE)
    }
    return(list(sigma2 = sem^2, sd = NA_real_, r = NA_real_,
                sem = sem, sdiff = NA_real_))
  }
  if (!is.numeric(sd) || length(sd) != 1L || !is.finite(sd) || sd <= 0) {
    stop("`sd` must be a single positive, finite number (or supply `sem`/`sdiff`).", call. = FALSE)
  }
  if (!is.numeric(r) || length(r) != 1L || !is.finite(r) || r < 0 || r > 1) {
    stop("`r` must be a single number in [0, 1] (or supply `sem`/`sdiff`).", call. = FALSE)
  }
  list(sigma2 = sd^2 * (1 - r), sd = sd, r = r,
       sem = NA_real_, sdiff = NA_real_)
}

.rti_compute <- function(y, sd = NULL, r = NULL, t = NULL, na.rm = FALSE,
                         level = 0.95, sem = NULL, sdiff = NULL, call = NULL) {
  series <- .rti_prepare_data(y, t, na.rm)
  error <- .rti_measurement_variance(sd, r, sem, sdiff)
  beta1 <- sum(series$t_centered * series$y) / series$Sxx
  se_beta1 <- sqrt(error$sigma2 / series$Sxx)
  z <- beta1 / se_beta1
  p <- 2 * stats::pnorm(-abs(z))
  zcrit <- stats::qnorm(1 - (1 - level) / 2)

  out <- list(
    estimate = beta1, intercept = mean(series$y), se = se_beta1, z = z, p = p,
    ci = c(beta1 - zcrit * se_beta1, beta1 + zcrit * se_beta1),
    sigma2 = error$sigma2, t = series$t, t_centered = series$t_centered,
    y = series$y, Sxx = series$Sxx, n = length(series$y),
    sd = error$sd, r = error$r, sem = error$sem, sdiff = error$sdiff,
    level = level, call = call
  )
  class(out) <- "reliableTrend"
  out
}

.rci_resolve_difference <- function(difference, t1, t2) {
  if (is.null(difference)) {
    if (is.null(t1) || is.null(t2)) {
      stop("Provide either `difference` or both `t1` and `t2`.", call. = FALSE)
    }
    if (!is.numeric(t1) || !is.numeric(t2)) {
      stop("`t1` and `t2` must be numeric.", call. = FALSE)
    }
    if (length(t1) != length(t2)) {
      warning("`t1` and `t2` lengths differ; recycling will be applied.", call. = FALSE)
    }
    difference <- t2 - t1
  }
  if (!is.numeric(difference)) stop("`difference` must be numeric.", call. = FALSE)
  difference
}

.rci_resolve_sdiff <- function(sdiff, sem, sd1, sd2, r1, r2, scale_rci,
                               prob, rc.type) {
  if (is.null(sdiff) && rc.type == "maassen") {
    if (any(is.null(c(sd1, sd2, r1)))) {
      stop("rc.type = 'maassen' requires `sd1`, `sd2`, and `r1`.", call. = FALSE)
    }
    if (!is.numeric(sd1) || !is.numeric(sd2) || !is.numeric(r1)) {
      stop("`sd1`, `sd2`, and `r1` must be numeric.", call. = FALSE)
    }
    sdiff <- sqrt((sd1^2 + sd2^2) * (1 - r1))
  } else if (is.null(sdiff) && rc.type == "mcnemar") {
    if (any(is.null(c(sd1, sd2, r1, r2)))) {
      stop("rc.type = 'mcnemar' requires `sd1`, `sd2`, `r1`, and `r2`.", call. = FALSE)
    }
    if (!is.numeric(sd1) || !is.numeric(sd2) || !is.numeric(r1) || !is.numeric(r2)) {
      stop("`sd1`, `sd2`, `r1`, and `r2` must be numeric.", call. = FALSE)
    }
    sdiff <- sqrt(sd1^2 * (1 - r1) + sd2^2 * (1 - r2))
  }

  if (is.null(sdiff)) {
    if (!is.null(sem)) {
      if (!is.numeric(sem) || any(!is.finite(sem)) || any(sem <= 0)) {
        stop("`sem` must be positive numeric.", call. = FALSE)
      }
      sdiff <- sqrt(2) * sem
    } else if (!is.null(sd1) && !is.null(r1)) {
      if (!is.numeric(sd1) || !is.numeric(r1)) stop("`sd1` and `r1` must be numeric.", call. = FALSE)
      if (any(r1 < 0 | r1 > 1)) stop("`r1` must be in [0, 1].", call. = FALSE)
      sem <- sd1 * sqrt(1 - r1)
      sdiff <- sqrt(2) * sem
    }
  }

  if (is.null(sdiff) && !is.null(scale_rci)) {
    if (!is.numeric(scale_rci) || !is.finite(scale_rci) || scale_rci <= 0) {
      stop("`scale_rci` must be a single positive number.", call. = FALSE)
    }
    sdiff <- scale_rci / stats::qnorm(prob)
  }
  if (is.null(sdiff) && is.null(scale_rci) && exists("scale_rci_calc", mode = "function")) {
    scale_rci <- scale_rci_calc(sdiff = sdiff, rxx = r1, sd1 = sd1, sem = sem,
                                prob = prob, verbose = FALSE)
  }
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
  list(sdiff = sdiff, sem = sem,
       scale_rci = if (is.null(scale_rci)) stats::qnorm(prob) * sdiff else scale_rci)
}

.rti_group_parameter <- function(input, idx, name, id) {
  if (!is.list(input) || !identical(input$type, "column")) return(input$value)
  values <- input$values[idx]
  unique_values <- unique(values[is.finite(values)])
  if (length(unique_values) != 1L) {
    stop("`", name, "` must be constant within id '", id,
         "' when provided as a column.", call. = FALSE)
  }
  unique_values[[1L]]
}
