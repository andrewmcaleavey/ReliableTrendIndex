#' Within-person slope SE using reliability (RTI components for one series)
#'
#' @title Reliable Trend components for a single time series
#'
#' @description
#' Computes the pieces needed for a reliability-adjusted test of a linear trend
#' for a single person’s time series. Given a vector of observations \code{y},
#' a reliability coefficient \code{r}, and (optionally) a time vector, this
#' function returns the OLS slope, the reliability-based standard error of that
#' slope, and the corresponding t-like statistic (the Reliable Trend Index; RTI).
#'
#' @details
#' For one individual with observations \eqn{y_t} at times \eqn{t}, consider
#' the model \eqn{y_t = \beta_0 + \beta_1 t + \varepsilon_t}. The OLS slope is
#' \deqn{\hat\beta_1 = \frac{\sum (t-\bar t)(y_t-\bar y)}{\sum (t-\bar t)^2},}
#' and its standard error can be written as
#' \deqn{SE(\hat\beta_1) = \frac{\sigma_e}{\sqrt{\sum (t-\bar t)^2}},}
#' where \eqn{\sigma_e} is the residual standard deviation.
#'
#' If we approximate residual variation by measurement error and plug in the
#' instrument's reliability \eqn{r} and single-occasion SD \eqn{SD}, then
#' \deqn{\sigma_e \approx SD \sqrt{1-r}.}
#' Hence the reliability-based SE of the slope is
#' \deqn{SE_{\mathrm{rel}}(\hat\beta_1) = \frac{SD \sqrt{1-r}}{\sqrt{S_{xx}}}, \quad
#'  S_{xx} = \sum (t-\bar t)^2.}
#'
#' The corresponding Reliable Trend Index (RTI) is
#' \deqn{\mathrm{RTI} = \frac{\hat\beta_1}{SE_{\mathrm{rel}}(\hat\beta_1)}.}
#'
#' When times are equally spaced \eqn{t = 1, \dots, n}, \eqn{S_{xx}} has the
#' closed form \eqn{S_{xx} = n(n^2-1)/12}. For \eqn{n=2}, the slope reduces to
#' \eqn{\hat\beta_1 = y_2 - y_1} and
#' \deqn{SE_{\mathrm{rel}}(\hat\beta_1) = SD \sqrt{2(1-r)},}
#' making \eqn{\mathrm{RTI}} identical to the classical Reliable Change Index
#' (RCI; Jacobson & Truax, 1991).
#'
#' @param y Numeric vector of observations for one person (length \eqn{n \ge 2}).
#' @param r Reliability coefficient in \code{[0, 1]} (treated as known).
#' @param time Optional numeric vector of the same length as \code{y}. If
#'   \code{NULL}, time is set to \code{1:length(y)} (equally spaced).
#' @param sd_single Optional single-occasion SD for the instrument (e.g., known
#'   baseline or population SD). If \code{NULL}, uses \code{sd(y)} as a proxy.
#'
#' @return
#' An object of class \code{"slopeSErel"}: a named \code{list} with elements
#' \describe{
#'   \item{\code{n}}{Number of observations.}
#'   \item{\code{reliability_r}}{The supplied reliability \code{r}.}
#'   \item{\code{time_equal_spacing}}{Logical; \code{TRUE} if \code{time == 1:n}.}
#'   \item{\code{S_xx}}{\eqn{S_{xx} = \sum (t-\bar t)^2}.}
#'   \item{\code{S_xx_closed_form}}{If equally spaced, \eqn{n(n^2-1)/12}; else \code{NA}.}
#'   \item{\code{sqrt_S_xx}}{\eqn{\sqrt{S_{xx}}}.}
#'   \item{\code{SE_factor}}{\eqn{1/\sqrt{S_{xx}}}.}
#'   \item{\code{sd_single}}{Single-occasion SD used.}
#'   \item{\code{sd_source}}{Character note on how \code{sd_single} was obtained.}
#'   \item{\code{sigma_e}}{\eqn{SD \sqrt{1-r}} (measurement-error SD).}
#'   \item{\code{SE_reliability}}{\eqn{SD\sqrt{1-r} / \sqrt{S_{xx}}}.}
#'   \item{\code{slope_hat}}{OLS slope estimate \eqn{\hat\beta_1}.}
#'   \item{\code{t_reliability}}{RTI: \eqn{\hat\beta_1 / SE_{\mathrm{rel}}(\hat\beta_1)}.}
#'   \item{\code{call}}{Matched call.}
#' }
#'
#' @references
#' Jacobson, N. S., & Truax, P. (1991). Clinical significance: A statistical
#' approach to defining meaningful change in psychotherapy research.
#' \emph{Journal of Consulting and Clinical Psychology}, 59(1), 12–19.
#'
#' @seealso \code{\link[stats]{lm}}, \code{\link[stats]{sd}}, \code{\link[stats]{coef}}
#'
#' @examples
#' # Five equally spaced observations with r = 0.90
#' y <- c(20, 18, 17, 16, 15)
#' res <- slope_se_reliability(y, r = 0.90)
#' res
#'
#' # Provide a known single-occasion SD (e.g., instrument manual)
#' slope_se_reliability(y, r = 0.90, sd_single = 8)
#'
#' # Uneven time spacing
#' t_custom <- c(0, 1, 3, 7, 10)
#' y2 <- c(12, 11, 10, 9, 8)
#' slope_se_reliability(y2, r = 0.85, time = t_custom)
#'
#' @importFrom stats lm coef sd
#' @export
slope_se_reliability <- function(y, r, time = NULL, sd_single = NULL) {
  stopifnot(is.numeric(y), length(y) >= 2)
  if (is.null(time)) time <- seq_along(y)
  stopifnot(length(time) == length(y))
  if (anyNA(y) || anyNA(time)) {
    ok <- !is.na(y) & !is.na(time)
    y <- y[ok]; time <- time[ok]
    if (length(y) < 2) stop("Not enough non-missing observations after removing NAs.")
  }
  if (!is.numeric(r) || length(r) != 1 || r < 0 || r > 1) {
    stop("r must be a single number in [0, 1].")
  }
  
  n <- length(y)
  tbar <- mean(time)
  S_xx <- sum((time - tbar)^2)
  sqrt_S_xx <- sqrt(S_xx)
  
  # Single-occasion SD for scaling measurement error.
  if (is.null(sd_single)) {
    sd_single <- stats::sd(y)
    sd_source <- "sd(y)"
  } else {
    sd_source <- "sd_single (provided)"
  }
  
  # Measurement-error SD per occasion (in the same units as y)
  sigma_e <- sd_single * sqrt(1 - r)
  
  # SE of slope using reliability plug-in: SE(beta1) = sigma_e / sqrt(S_xx)
  SE_factor <- 1 / sqrt_S_xx
  SE_reliability <- sigma_e * SE_factor
  
  # OLS slope for reference (using standard lm)
  fit <- stats::lm(y ~ time)
  slope_hat <- unname(stats::coef(fit)[["time"]])
  
  # t-statistic (RTI) for H0: true trend = 0, using reliability-based SE
  t_reliability <- slope_hat / SE_reliability
  
  # Closed-form S_xx for equally spaced time = 1:n
  S_xx_closed_form <- if (all(time == seq_len(n))) n * (n^2 - 1) / 12 else NA_real_
  
  out <- list(
    n = n,
    reliability_r = r,
    time_equal_spacing = all(time == seq_len(n)),
    S_xx = S_xx,
    S_xx_closed_form = S_xx_closed_form,
    sqrt_S_xx = sqrt_S_xx,
    SE_factor = SE_factor,                     # equals 1 / sqrt(S_xx)
    sd_single = sd_single,
    sd_source = sd_source,
    sigma_e = sigma_e,                         # SD * sqrt(1 - r)
    SE_reliability = SE_reliability,           # SD*sqrt(1-r) / sqrt(S_xx)
    slope_hat = slope_hat,                     # OLS slope estimate
    t_reliability = t_reliability,             # slope / SE_reliability
    call = match.call()
  )
  class(out) <- "slopeSErel"
  out
}

#' @title Print method for slopeSErel objects
#' @description Pretty-prints the components of a \code{slopeSErel} object.
#'
#' @param x An object of class \code{"slopeSErel"} returned by
#'   \code{\link{slope_se_reliability}}.
#' @param digits Number of digits to display.
#' @param ... Further arguments (unused).
#'
#' @return Invisibly returns \code{x}.
#' @export
#' @method print slopeSErel
print.slopeSErel <- function(x, digits = 4, ...) {
  cat("Within-person slope SE (reliability-based)\n")
  cat(sprintf("n = %d | r = %.3f | equal spacing: %s\n",
              x$n, x$reliability_r, x$time_equal_spacing))
  if (!is.na(x$S_xx_closed_form)) {
    cat(sprintf("S_xx = %.*f  (closed form n(n^2-1)/12 = %.*f)\n",
                digits, x$S_xx, digits, x$S_xx_closed_form))
  } else {
    cat(sprintf("S_xx = %.*f\n", digits, x$S_xx))
  }
  cat(sprintf("sqrt(S_xx) = %.*f | SE factor = 1/sqrt(S_xx) = %.*f\n",
              digits, x$sqrt_S_xx, digits, x$SE_factor))
  cat(sprintf("sd_single = %.*f (%s) | sigma_e = sd_single*sqrt(1-r) = %.*f\n",
              digits, x$sd_single, x$sd_source, digits, x$sigma_e))
  cat(sprintf("SE_reliability = sigma_e / sqrt(S_xx) = %.*f\n",
              digits, x$SE_reliability))
  cat(sprintf("slope_hat (OLS) = %.*f | t_reliability = slope_hat / SE_reliability = %.*f\n",
              digits, x$slope_hat, digits, x$t_reliability))
  invisible(x)
}
