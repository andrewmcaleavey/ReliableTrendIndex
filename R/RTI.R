#' Reliable Trend Index (RTI)
#'
#' Compute a reliability-based test of linear trend (slope) within a person
#' using external single-occasion \code{sd} and \code{r} to define the error
#' variance. For equal spacing \eqn{t = 1,\dots,n}, \eqn{S_{xx} = n(n^2-1)/12};
#' for custom spacing, \eqn{S_{xx} = \sum (t - \bar t)^2}.
#'
#' The test statistic is \eqn{Z = \hat\beta_1 / \mathrm{SE}(\hat\beta_1)} with
#' \eqn{\mathrm{SE}(\hat\beta_1) = \sqrt{\mathrm{sd}^2 (1-r) / S_{xx}}}. This mirrors
#' the Jacobson–Truax logic used in the RCI (the \eqn{n=2} special case) but
#' generalizes it to \eqn{n\ge2} via \eqn{S_{xx}}.
#'
#' @param y Numeric vector of within-person observations (length \code{n} >= 2).
#' @param sd Positive numeric. Single-occasion standard deviation (external).
#' @param r Numeric in [0, 1]. Reliability (external).
#' @param t Optional numeric vector of time indices. If \code{NULL} (default),
#'   equal spacing \code{t = 1:n} is used.
#' @param na.rm Logical. If \code{TRUE}, drop incomplete pairs of \code{(y, t)} with a warning.
#' @param level Confidence level for slope intervals (used by \code{summary} and plots).
#'
#' @return An object of class \code{"reliableTrend"} with elements:
#' \itemize{
#'   \item \code{estimate} – \eqn{\hat\beta_1} (slope with centered time)
#'   \item \code{intercept} – \eqn{\hat\beta_0} at \eqn{\bar t} (equals \code{mean(y)})
#'   \item \code{se} – \eqn{\mathrm{SE}(\hat\beta_1)}
#'   \item \code{z}, \code{p} – Z statistic and two-sided p-value
#'   \item \code{ci} – numeric length-2 vector for slope at \code{level}
#'   \item \code{sigma2} – \eqn{\mathrm{sd}^2 (1-r)}
#'   \item \code{t}, \code{t_centered}, \code{y}, \code{Sxx}, \code{n}, \code{sd}, \code{r}, \code{level}
#' }
#'
#' @examples
#' y <- c(12, 11, 13, 16, 17, 19)
#' fit <- rti(y, sd = 8, r = 0.85)
#' fit$estimate
#' fit$p
#' plot(fit)  # see plot.reliableTrend
#'
#' @export
rti <- function(y, sd, r, t = NULL, na.rm = FALSE, level = 0.95) {
  cl <- match.call()
  
  if (!is.numeric(y) || length(y) < 2L) stop("`y` must be numeric with length >= 2.", call. = FALSE)
  if (!is.numeric(sd) || length(sd) != 1L || !is.finite(sd) || sd <= 0) {
    stop("`sd` must be a single positive, finite number.", call. = FALSE)
  }
  if (!is.numeric(r) || length(r) != 1L || !is.finite(r) || r < 0 || r > 1) {
    stop("`r` must be a single number in [0, 1].", call. = FALSE)
  }
  if (!is.null(t) && (!is.numeric(t) || length(t) != length(y))) {
    stop("`t` must be numeric and the same length as `y`.", call. = FALSE)
  }
  
  # Handle missingness
  if (is.null(t)) t <- seq_along(y)
  keep <- is.finite(y) & is.finite(t)
  if (!all(keep)) {
    if (!na.rm) stop("Missing or non-finite values in `y`/`t`. Set `na.rm = TRUE` to drop.", call. = FALSE)
    y <- y[keep]
    t <- t[keep]
    warning("Dropped ", sum(!keep), " non-finite observations.", call. = FALSE)
  }
  
  n <- length(y)
  if (n < 2L) stop("Need at least 2 finite observations after dropping.", call. = FALSE)
  
  # Center time; this makes intercept = mean(y) and Cov(β0,β1) = 0
  tbar <- mean(t)
  tc <- t - tbar
  Sxx <- sum(tc^2)
  if (Sxx <= 0) stop("Degenerate time vector: Sxx = 0. Time points must vary.", call. = FALSE)
  
  # OLS slope and intercept with centered time
  beta1 <- sum(tc * y) / Sxx
  beta0 <- mean(y)
  
  # Reliability-based SE for slope
  # Use closed form if equal spacing, otherwise compute from tc
  equal_spacing <- .is_equal_spacing(t)
  se_beta1 <- if (equal_spacing) {
    se_slope_reliability(n = n, sd = sd, r = r, spacing = "equal")
  } else {
    se_slope_reliability(n = n, sd = sd, r = r, spacing = "custom", t = t)
  }
  
  # Z-test (external variance treated as known)
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
    sigma2 = sd^2 * (1 - r),
    t = as.numeric(t),
    t_centered = as.numeric(tc),
    y = as.numeric(y),
    Sxx = Sxx,
    n = n,
    sd = sd,
    r = r,
    level = level,
    call = cl
  )
  class(out) <- "reliableTrend"
  out
}

# Internal: check if t is equally spaced up to numeric tolerance
.is_equal_spacing <- function(t, tol = 1e-10) {
  dt <- diff(as.numeric(t))
  if (length(dt) == 0L) return(TRUE)
  max(dt) - min(dt) < tol
}

#' @export
print.reliableTrend <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  cat("Reliable Trend Index (reliableTrend)\n")
  cat("n =", x$n, " | sd =", formatC(x$sd, digits = digits, format = "fg"),
      " | r =", formatC(x$r, digits = digits, format = "fg"), "\n")
  cat("Slope (beta1):", formatC(x$estimate, digits = digits, format = "fg"),
      "SE:", formatC(x$se, digits = digits, format = "fg"),
      "Z:", formatC(x$z, digits = digits, format = "fg"),
      "p:", formatC(x$p, digits = digits, format = "fg"), "\n")
  invisible(x)
}
