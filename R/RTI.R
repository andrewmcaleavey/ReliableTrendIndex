#' Reliable Trend Index (RTI)
#'
#' Compute a reliability-based test of within-person linear trend.
#' Backward compatible with older code that passed \code{values}/\code{time}
#' and/or \code{sem} instead of \code{sd}/\code{r}.
#'
#' @param y,values Numeric vector of within-person observations (length \eqn{n\ge2}).
#'   \code{values} is a legacy alias for \code{y}; if both are supplied, \code{y} is used.
#' @param sd Positive numeric. Single-occasion standard deviation (external).
#' @param r Numeric in [0, 1]. Reliability (external).
#' @param sem Optional positive numeric. Standard error of measurement (external).
#'   If provided, it takes precedence and sets \eqn{\sigma^2 = \mathrm{sem}^2}.
#' @param t,time Optional numeric vector of time indices (same length as \code{y}).
#'   \code{time} is a legacy alias for \code{t}. If both are missing, uses \code{t = 1:n}.
#' @param na.rm Logical. Drop incomplete \code{(y, t)} pairs? Default \code{FALSE}.
#' @param level Confidence level for slope intervals (default \code{0.95}).
#'
#' @return An object of class \code{"reliableTrend"} with elements:
#' \itemize{
#'   \item \code{estimate}, \code{intercept}, \code{se}, \code{z}, \code{p}, \code{ci}
#'   \item \code{sigma2}, \code{t}, \code{t_centered}, \code{y}, \code{Sxx}, \code{n}
#'   \item \code{sd}, \code{r}, \code{sem}, \code{level}, \code{call}
#' }
#' @examples
#' # New style (sd & r)
#' rti(y = c(12, 11, 13, 16), sd = 8, r = 0.85)
#' # Legacy style (values & sem)
#' rti(values = c(12, 11, 13, 16), sem = 3)
#' @export
rti <- function(y = NULL, sd = NULL, r = NULL,
                t = NULL, na.rm = FALSE, level = 0.95,
                values = NULL, time = NULL, sem = NULL) {
  cl <- match.call()
  
  # ---- legacy aliases ----
  if (!is.null(values) && is.null(y)) y <- values
  if (!is.null(time)   && is.null(t)) t <- time
  
  if (is.null(y) || !is.numeric(y) || length(y) < 2L)
    stop("`y` (or legacy `values`) must be numeric with length >= 2.", call. = FALSE)
  
  # default time if missing
  if (is.null(t)) t <- seq_along(y)
  
  if (!is.numeric(t) || length(t) != length(y))
    stop("`t` (or legacy `time`) must be numeric and the same length as `y`.", call. = FALSE)
  
  # Handle missingness
  keep <- is.finite(y) & is.finite(t)
  if (!all(keep)) {
    if (!na.rm) stop("Missing or non-finite values in `y`/`t`. Set `na.rm = TRUE` to drop.", call. = FALSE)
    y <- y[keep]; t <- t[keep]
    warning("Dropped ", sum(!keep), " non-finite observations.", call. = FALSE)
  }
  n <- length(y)
  if (n < 2L) stop("Need at least 2 finite observations after dropping.", call. = FALSE)
  
  # Center time; intercept = mean(y)
  tbar <- mean(t)
  tc   <- t - tbar
  Sxx  <- sum(tc^2)
  if (Sxx <= 0) stop("Degenerate time vector: Sxx = 0. Time points must vary.", call. = FALSE)
  
  # OLS slope (centered time)
  beta1 <- sum(tc * y) / Sxx
  beta0 <- mean(y)
  
  # ---- error variance: prefer SEM if given; else sd/r ----
  if (!is.null(sem)) {
    if (!is.numeric(sem) || length(sem) != 1L || !is.finite(sem) || sem <= 0)
      stop("`sem` must be a single positive, finite number.", call. = FALSE)
    sigma2 <- sem^2
    sd_out <- NA_real_; r_out <- NA_real_; sem_out <- sem
  } else {
    if (!is.numeric(sd) || length(sd) != 1L || !is.finite(sd) || sd <= 0)
      stop("`sd` must be a single positive, finite number (or supply `sem`).", call. = FALSE)
    if (!is.numeric(r) || length(r) != 1L || !is.finite(r) || r < 0 || r > 1)
      stop("`r` must be a single number in [0, 1] (or supply `sem`).", call. = FALSE)
    sigma2 <- sd^2 * (1 - r)
    sd_out <- sd; r_out <- r; sem_out <- NA_real_
  }
  
  # Slope SE and test
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
    sd = sd_out,
    r = r_out,
    sem = sem_out,
    level = level,
    call = cl
  )
  class(out) <- "reliableTrend"
  out
}

