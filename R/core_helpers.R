# Internal computation helpers for the supported RTI/RCI API.
#
# Keeping validation and formulae here lets the public entry points share one
# implementation while preserving their existing return values.

.rti_prepare_data <- function(y, t, na.rm) {
  if (is.null(y) || !is.numeric(y) || length(y) < 2L) {
    stop("`y` (or legacy `values`) must be numeric with length >= 2.", call. = FALSE)
  }
  if (is.null(t)) t <- seq_along(y)
  n_original <- length(y)
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

  list(y = as.numeric(y), t = as.numeric(t), t_centered = as.numeric(tc),
       Sxx = Sxx, index = which(keep), n_original = n_original)
}

.rti_resolve_error_input <- function(x, name, t, index, n_original) {
  if (is.function(x)) x <- x(t)
  if (!is.numeric(x) || !(length(x) %in% c(1L, length(t), n_original)) ||
      any(!is.finite(x))) {
    stop("`", name, "` must be finite numeric scalar, length-y vector, or function of `t`.",
         call. = FALSE)
  }
  if (length(x) == n_original && n_original != length(t)) x <- x[index]
  if (length(x) == 1L) x <- rep(x, length(t))
  as.numeric(x)
}

.rti_measurement_variance <- function(sd, r, sem, sdiff, t, index, n_original) {
  # Precedence is part of the public contract: sdiff > sem > sd/r.
  if (!is.null(sdiff)) {
    sdiff <- .rti_resolve_error_input(sdiff, "sdiff", t, index, n_original)
    if (any(sdiff <= 0)) stop("`sdiff` must be positive.", call. = FALSE)
    return(list(sigma2 = sdiff^2 / 2, sd = NA_real_, r = NA_real_,
                sem = NA_real_, sdiff = sdiff))
  }
  if (!is.null(sem)) {
    sem <- .rti_resolve_error_input(sem, "sem", t, index, n_original)
    if (any(sem <= 0)) stop("`sem` must be positive.", call. = FALSE)
    return(list(sigma2 = sem^2, sd = NA_real_, r = NA_real_,
                sem = sem, sdiff = NA_real_))
  }
  sd <- .rti_resolve_error_input(sd, "sd", t, index, n_original)
  r <- .rti_resolve_error_input(r, "r", t, index, n_original)
  if (any(sd <= 0)) stop("`sd` must be positive (or supply `sem`/`sdiff`).", call. = FALSE)
  if (any(r < 0 | r > 1)) stop("`r` must be in [0, 1] (or supply `sem`/`sdiff`).", call. = FALSE)
  list(sigma2 = sd^2 * (1 - r), sd = sd, r = r,
       sem = NA_real_, sdiff = NA_real_)
}

.rti_compute <- function(y, sd = NULL, r = NULL, t = NULL, na.rm = FALSE,
                         level = 0.95, sem = NULL, sdiff = NULL,
                         rc.type = "jt", call = NULL) {
  series <- .rti_prepare_data(y, t, na.rm)
  error <- .rti_measurement_variance(sd, r, sem, sdiff, series$t,
                                     series$index, series$n_original)
  beta1 <- sum(series$t_centered * series$y) / series$Sxx
  se_beta1 <- sqrt(sum(series$t_centered^2 * error$sigma2) / series$Sxx^2)
  z <- beta1 / se_beta1
  p <- 2 * stats::pnorm(-abs(z))
  zcrit <- stats::qnorm(1 - (1 - level) / 2)

  out <- list(
    estimate = beta1, intercept = mean(series$y), se = se_beta1, z = z, p = p,
    ci = c(beta1 - zcrit * se_beta1, beta1 + zcrit * se_beta1),
    sigma2 = if (length(unique(error$sigma2)) == 1L) error$sigma2[[1L]] else error$sigma2,
    t = series$t, t_centered = series$t_centered, y = series$y,
    Sxx = series$Sxx, n = length(series$y),
    sd = if (all(is.na(error$sd))) NA_real_ else if (length(unique(error$sd)) == 1L) error$sd[[1L]] else error$sd,
    r = if (all(is.na(error$r))) NA_real_ else if (length(unique(error$r)) == 1L) error$r[[1L]] else error$r,
    sem = if (all(is.na(error$sem))) NA_real_ else if (length(unique(error$sem)) == 1L) error$sem[[1L]] else error$sem,
    sdiff = if (all(is.na(error$sdiff))) NA_real_ else if (length(unique(error$sdiff)) == 1L) error$sdiff[[1L]] else error$sdiff,
    level = level, rc.type = rc.type, call = call
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
  validate_sd <- function(x, name) {
    if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x <= 0) {
      stop("`", name, "` must be a single positive, finite number.", call. = FALSE)
    }
  }
  validate_r <- function(x, name) {
    if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x < 0 || x > 1) {
      stop("`", name, "` must be a single number in [0, 1].", call. = FALSE)
    }
  }

  if (is.null(sdiff) && rc.type == "maassen") {
    if (any(is.null(c(sd1, sd2, r1)))) {
      stop("rc.type = 'maassen' requires `sd1`, `sd2`, and `r1`.", call. = FALSE)
    }
    validate_sd(sd1, "sd1")
    validate_sd(sd2, "sd2")
    validate_r(r1, "r1")
    sdiff <- sqrt((sd1^2 + sd2^2) * (1 - r1))
  } else if (is.null(sdiff) && rc.type == "mcnemar") {
    if (any(is.null(c(sd1, sd2, r1, r2)))) {
      stop("rc.type = 'mcnemar' requires `sd1`, `sd2`, `r1`, and `r2`.", call. = FALSE)
    }
    validate_sd(sd1, "sd1")
    validate_sd(sd2, "sd2")
    validate_r(r1, "r1")
    validate_r(r2, "r2")
    sdiff <- sqrt(sd1^2 * (1 - r1) + sd2^2 * (1 - r2))
  }

  if (is.null(sdiff)) {
    if (!is.null(sem)) {
      if (!is.numeric(sem) || any(!is.finite(sem)) || any(sem <= 0)) {
        stop("`sem` must be positive numeric.", call. = FALSE)
      }
      sdiff <- sqrt(2) * sem
    } else if (!is.null(sd1) && !is.null(r1)) {
      validate_sd(sd1, "sd1")
      validate_r(r1, "r1")
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

.legacy_deprecate <- function(old, replacement, details = NULL) {
  msg <- paste0(old, "() is deprecated since ReliableTrendIndex 0.3.0; use ",
                replacement, " instead. It will remain available until at least 1.0.0.")
  if (!is.null(details)) msg <- paste(msg, details)
  .Deprecated(new = replacement, package = "ReliableTrendIndex", msg = msg)
}
