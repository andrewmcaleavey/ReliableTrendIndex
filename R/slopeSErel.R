# with assistance from ChatGPT 5, 2025-09-01

#' Within-person slope SE using reliability (RTI components for one series)
#'
#' @title Reliable Trend components for a single time series
#'
#' @description
#' Computes the pieces needed for a reliability-adjusted test of a linear trend
#' for a single person’s time series. Given a vector of observations \code{y},
#' a reliability coefficient \code{r}, and (optionally) a time vector, this
#' function returns the OLS slope, the reliability-based standard error of that
#' slope, and the corresponding z-like statistic (the Reliable Trend Index; RTI).
#' It also provides a two-sided \code{z} critical value, a p-value, and a simple
#' categorical interpretation using the \code{z}-criterion.
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
#' Because the denominator is a plug-in standard error derived from external
#' reliability and a single-occasion SD (not estimated from the same series),
#' inference uses the \emph{standard normal} (\eqn{z}) reference, not \eqn{t}.
#'
#' When times are equally spaced \eqn{t = 1, \dots, n}, \eqn{S_{xx}} has the
#' closed form \eqn{S_{xx} = n(n^2-1)/12}. For \eqn{n=2}, the slope reduces to
#' \eqn{\hat\beta_1 = y_2 - y_1} and
#' \deqn{SE_{\mathrm{rel}}(\hat\beta_1) = SD \sqrt{2(1-r)},}
#' making \eqn{\mathrm{RTI}} identical to the classical Reliable Change Index
#' (RCI; Jacobson & Truax, 1991).
#'
#' \strong{Inference and categorization.}
#' Given a two-sided significance level \code{p} (default \code{0.05}),
#' the critical value is \eqn{z_{1-p/2}} (e.g., \eqn{\pm 1.96} at \code{p=0.05}).
#' The function returns the p-value \eqn{2(1-\Phi(|\mathrm{RTI}|))} and a label:
#' \itemize{
#'   \item \code{"Reliable increase"} if \eqn{\mathrm{RTI} \ge z_{1-p/2}},
#'   \item \code{"Reliable decrease"} if \eqn{\mathrm{RTI} \le -z_{1-p/2}},
#'   \item \code{"No reliable trend"} otherwise.
#' }
#'
#' @param y Numeric vector of observations for one person (length \eqn{n \ge 2}).
#' @param r Reliability coefficient in \code{[0, 1]} (treated as known).
#' @param time Optional numeric vector of the same length as \code{y}. If
#'   \code{NULL}, time is set to \code{1:length(y)} (equally spaced).
#' @param sd_single Optional single-occasion SD for the instrument (e.g., known
#'   baseline or population SD). If \code{NULL}, uses \code{sd(y)} as a proxy.
#' @param p Two-sided significance level for z-based inference (default \code{0.05}).
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
#'   \item{\code{RTI}}{\eqn{\hat\beta_1 / SE_{\mathrm{rel}}(\hat\beta_1)} (z-like index).}
#'   \item{\code{crit}}{Two-sided z critical value \eqn{z_{1-p/2}}.}
#'   \item{\code{p_value}}{Two-sided p-value based on \code{RTI} under \eqn{z}.}
#'   \item{\code{RTI_cat}}{"Reliable increase", "Reliable decrease", or "No reliable trend".}
#'   \item{\code{t_reliability}}{Deprecated alias for \code{RTI} (for backward compatibility).}
#'   \item{\code{call}}{Matched call.}
#' }
#'
#' @references
#' Jacobson, N. S., & Truax, P. (1991). Clinical significance: A statistical
#' approach to defining meaningful change in psychotherapy research.
#' \emph{Journal of Consulting and Clinical Psychology}, 59(1), 12–19.
#'
#' McAleavey, A. A. (2024). When (not) to rely on the reliable change index:
#' A critical appraisal and alternatives to consider in clinical psychology.
#' \emph{Clinical Psychology: Science and Practice}, 31(3), 351–366.
#' \doi{10.1037/cps0000203}
#'
#' @seealso \code{\link[stats]{lm}}, \code{\link[stats]{sd}}, \code{\link[stats]{coef}}, \code{\link[stats]{qnorm}}, \code{\link[stats]{pnorm}}
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
#' # Three-point linear example equals endpoint RCI in magnitude
#' y3 <- c(47.5, 40, 32.5); t3 <- 1:3
#' slope_se_reliability(y3, r = 0.80, time = t3, sd_single = 7.5, p = 0.05)
#'
#' @importFrom stats lm coef sd qnorm pnorm
#' @export
slope_se_reliability <- function(y, r, time = NULL, sd_single = NULL, p = 0.05) {
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
  if (!is.numeric(p) || length(p) != 1L || p <= 0 || p >= 1) {
    stop("`p` must be a single numeric in (0, 1).")
  }
  
  n <- length(y)
  tbar <- mean(time)
  S_xx <- sum((time - tbar)^2)
  if (!is.finite(S_xx) || S_xx <= 0) stop("S_xx is not positive; check `time` variability.")
  sqrt_S_xx <- sqrt(S_xx)
  
  # Single-occasion SD for scaling measurement error.
  if (is.null(sd_single)) {
    sd_single <- stats::sd(y)
    if (!is.finite(sd_single) || sd_single <= 0) stop("sd(y) is zero/non-finite; provide `sd_single`.")
    sd_source <- "sd(y)"
  } else {
    if (!is.numeric(sd_single) || length(sd_single) != 1L || sd_single <= 0) {
      stop("`sd_single` must be a positive numeric scalar if supplied.")
    }
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
  
  # RTI (z-like index) and z inference
  RTI <- slope_hat / SE_reliability
  crit <- stats::qnorm(1 - p/2)
  p_value <- 2 * (1 - stats::pnorm(abs(RTI)))
  
  RTI_cat <- if (!is.finite(RTI)) {
    NA_character_
  } else if (RTI >=  crit) {
    "Reliable increase"
  } else if (RTI <= -crit) {
    "Reliable decrease"
  } else {
    "No reliable trend"
  }
  
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
    RTI = RTI,                                 # slope / SE_reliability (z-like)
    crit = crit,                               # z critical value
    p_value = p_value,                         # two-sided p-value under z
    RTI_cat = RTI_cat,                         # categorical interpretation
    t_reliability = RTI,                       # deprecated alias for backward compatibility
    # store raw series and intercept for plotting
    y = as.numeric(y),
    time_vec = as.numeric(time),
    intercept_hat = unname(stats::coef(fit)[1L]),
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
  cat(sprintf("slope_hat (OLS) = %.*f\n", digits, x$slope_hat))
  cat(sprintf("RTI = slope_hat / SE_reliability = %.*f | z_crit = %.*f | p = %.*g | %s\n",
              digits, x$RTI, digits, x$crit, digits, x$p_value, x$RTI_cat))
  invisible(x)
}


#' @title Plot method for slopeSErel (returns a ggplot)
#' @description
#' Creates a ggplot showing the observed series and the fitted line from
#' \code{slope_se_reliability()}. Optionally adds a confidence band
#' for the linear trend based on the reliability-derived slope SE
#' (slope-only uncertainty: half-width = z_crit * |t - tbar| * SE_slope).
#'
#' The returned object is a standard \code{ggplot} that you can modify
#' (e.g., add themes, scales, and layers).
#'
#' @param x A \code{slopeSErel} object from \code{\link{slope_se_reliability}}.
#' @param annotate Logical; if \code{TRUE}, adds a label with RTI, z-crit, and p.
#' @param digits Integer; rounding for numeric labels.
#' @param point_args Named list of args for \code{ggplot2::geom_point()} (e.g., \code{list(size=2)}).
#' @param line_args  Named list of args for \code{ggplot2::geom_line()} for the fitted line
#'   (e.g., \code{list(linewidth=0.9)}).
#' @param band Logical; if \code{TRUE}, adds a \code{geom_ribbon()} confidence band
#'   for the linear trend using the reliability-based slope SE.
#' @param band_args Named list of args for \code{ggplot2::geom_ribbon()}
#'   (e.g., \code{list(alpha=0.15)}). Defaults to a light, semi-transparent band.
#' @param grid_n Integer; number of points in the time grid used to draw the line/band.
#' @param ... Unused; included for S3 compatibility.
#'
#' @return A \code{ggplot} object.
#'
#' @details
#' **What the band represents.** The band reflects uncertainty in the **slope**
#' only, using \code{SE(beta1) = SD*sqrt(1-r)/sqrt(S_xx)} and your z critical value.
#' The half-width at time \eqn{t} is \eqn{z_{1-p/2}\,|t-\bar t|\,SE(\hat\beta_1)}.
#' It does **not** include uncertainty in the intercept or residual scatter, so it is
#' a “trend-rate” band rather than a mean-curve CI or prediction interval.
#'
#' Requires the \strong{ggplot2} package. This method expects the \code{slopeSErel}
#' object to include \code{y}, \code{time_vec}, and \code{intercept_hat}; if missing,
#' recompute with the updated \code{slope_se_reliability()}.
#'
#' @examples
#' y <- c(20, 18, 17, 16, 15)
#' fit <- slope_se_reliability(y, r = 0.90)     # stores y/time/intercept
#' p <- plot(fit, band = TRUE)                  # returns a ggplot with ribbon
#' p + ggplot2::theme_bw()
#'
#' # Customize aesthetics
#' plot(fit,
#'      point_args = list(size = 2),
#'      line_args  = list(linewidth = 1),
#'      band = TRUE,
#'      band_args = list(alpha = 0.2)) +
#'   ggplot2::labs(title = "Customized RTI plot")
#'
#' @export
#' @method plot slopeSErel
plot.slopeSErel <- function(x,
                            annotate = TRUE,
                            digits = 2,
                            point_args = list(),
                            line_args  = list(),
                            band = FALSE,
                            band_args = list(alpha = 0.15),
                            grid_n = 200,
                            ...) {
  # Validate presence of raw vectors/intercept for plotting
  if (is.null(x$time_vec) || is.null(x$y) || is.null(x$intercept_hat)) {
    stop("This slopeSErel object lacks `time_vec`, `y`, and/or `intercept_hat`.\n",
         "Recompute with the updated slope_se_reliability() that stores these fields.")
  }
  
  # Prepare data
  df <- data.frame(time = x$time_vec, y = x$y)
  tbar <- mean(df$time, na.rm = TRUE)
  
  # Dense grid for smooth line/band
  time_grid <- seq(min(df$time, na.rm = TRUE),
                   max(df$time, na.rm = TRUE),
                   length.out = max(2L, grid_n))
  df_fit <- data.frame(time = time_grid)
  df_fit$y_hat <- x$intercept_hat + x$slope_hat * df_fit$time
  
  # Base plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = time, y = y))
  
  # Points
  p <- p + do.call(ggplot2::geom_point, point_args)
  
  # Optional band (slope-only uncertainty)
  if (isTRUE(band)) {
    if (!is.finite(x$SE_reliability) || !is.finite(x$crit)) {
      warning("Band not added: SE_reliability or crit is not finite.")
    } else {
      halfwidth <- x$crit * abs(df_fit$time - tbar) * x$SE_reliability
      df_band <- transform(df_fit,
                           y_lower = y_hat - halfwidth,
                           y_upper = y_hat + halfwidth)
      default_ribbon <- list(data = df_band,
                             mapping = ggplot2::aes(x = time, ymin = y_lower, ymax = y_upper))
      p <- p + do.call(ggplot2::geom_ribbon,
                       utils::modifyList(default_ribbon, band_args))
    }
  }
  
  # Fitted line
  default_line <- list(data = df_fit,
                       mapping = ggplot2::aes(x = time, y = y_hat))
  p <- p + do.call(ggplot2::geom_line, utils::modifyList(default_line, line_args))
  
  # Labels / theme
  p <- p +
    ggplot2::labs(
      x = "Time",
      y = "Score",
      title = "Reliable Trend (reliability-based slope)",
      subtitle = sprintf("Slope = %.*f, RTI = %.*f (z_crit = %.*f, p = %.*g)",
                         digits, x$slope_hat, digits, x$RTI, digits, x$crit, digits, x$p_value),
      caption = sprintf("r = %.2f, SD = %.2f; SE = SD*sqrt(1-r)/sqrt(S_xx); S_xx = %.*f",
                        x$reliability_r, x$sd_single, digits, x$S_xx)
    ) +
    ggplot2::theme_minimal(base_size = 12)
  
  # Optional on-plot annotation label
  if (isTRUE(annotate)) {
    lab <- sprintf("RTI = %.*f\nz_crit = %.*f\np = %.*g",
                   digits, x$RTI, digits, x$crit, digits, x$p_value)
    p <- p + ggplot2::annotate(
      "label",
      x = max(df$time, na.rm = TRUE),
      y = max(df$y,   na.rm = TRUE),
      label = lab, hjust = 1, vjust = 1
    )
  }
  
  p
}

