#' Standard error for a within-person slope using reliability
#'
#' Compute the standard error of the OLS slope \eqn{\hat\beta_1} when the only
#' source of uncertainty is measurement error implied by a single-occasion
#' standard deviation (\code{sd}) and reliability (\code{r}).
#'
#' Under the working model \eqn{y_t = \beta_0 + \beta_1 t + \epsilon_t} with
#' \eqn{\epsilon_t \sim (0, \sigma^2)} and \eqn{\sigma^2 = \mathrm{sd}^2 (1-r)},
#' the variance of the slope is \eqn{\mathrm{Var}(\hat\beta_1) = \sigma^2 / S_{xx}},
#' where \eqn{S_{xx} = \sum (t - \bar t)^2}. For equally spaced time
#' points \eqn{t=1,\dots,n}, \eqn{S_{xx} = n(n^2 - 1)/12}.
#'
#' @param n Integer (>= 2). Number of observations.
#' @param sd Positive numeric. Single-occasion standard deviation (same scale as \code{y}).
#' @param r Numeric in \eqn{\eqn{\eqn{[0, 1]}}}. Reliability of the single-occasion measure.
#' @param spacing Character. Either \code{"equal"} (default; uses closed-form \eqn{S_{xx}})
#'   or \code{"custom"} (uses \code{t} to compute \eqn{S_{xx}} directly).
#' @param t Optional numeric vector of time points (length \code{n}) used when
#'   \code{spacing = "custom"}. If provided with \code{spacing="equal"}, it is ignored.
#'
#' @return A single positive numeric: \code{SE_hat_beta1}.
#' @examples
#' se_slope_reliability(n = 5, sd = 10, r = 0.8)           # equal spacing
#' se_slope_reliability(n = 5, sd = 10, r = 0.8, spacing = "custom",
#'                      t = c(0, 1, 2, 4, 7))
#' @export
se_slope_reliability <- function(n, sd, r, spacing = c("equal", "custom"), t = NULL) {
  spacing <- match.arg(spacing)
  if (!is.numeric(n) || length(n) != 1L || n < 2L || !is.finite(n)) {
    stop("`n` must be a finite integer >= 2.", call. = FALSE)
  }
  if (!is.numeric(sd) || length(sd) != 1L || !is.finite(sd) || sd <= 0) {
    stop("`sd` must be a single positive, finite number.", call. = FALSE)
  }
  if (!is.numeric(r) || length(r) != 1L || !is.finite(r) || r < 0 || r > 1) {
    stop("`r` must be a single number in [0, 1].", call. = FALSE)
  }
  
  sigma2 <- sd^2 * (1 - r)
  
  if (identical(spacing, "equal") && is.null(t)) {
    # Closed-form Sxx for t = 1,...,n
    Sxx <- n * (n^2 - 1) / 12
  } else {
    if (is.null(t)) stop("Provide `t` when spacing = 'custom'.", call. = FALSE)
    if (!is.numeric(t) || length(t) != n) stop("`t` must be numeric of length `n`.", call. = FALSE)
    if (!all(is.finite(t))) stop("`t` must be finite.", call. = FALSE)
    tc <- t - mean(t)
    Sxx <- sum(tc^2)
    if (Sxx <= 0) stop("Degenerate time vector: Sxx = 0. Time points must vary.", call. = FALSE)
  }
  
  sqrt(sigma2 / Sxx)
}
