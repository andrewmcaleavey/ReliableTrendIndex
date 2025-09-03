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
#' @param r Reliability coefficient in \eqn{\eqn{\eqn{[0, 1]}}} (treated as known).
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
#' @seealso \code{\link[stats]{lm}}, \code{\link[stats]{sd}}, \code{\link[stats]{coef}}, \code{\link[stats]{qnorm}}, \code{\link[stats]{pnorm}}, \code{\link[ReliableTrendIndex]{plot.slopeSErel}}, \code{\link[ReliableTrendIndex]{print.slopeSErel}}
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
#' # Plotting  
#' 
#' y <- c(20, 18, 17, 16, 15)
#' fit <- slope_se_reliability(y, r = 0.90)  # stores y/time/intercept etc.
#' 
#' # Slope-only band (as before)
#' plot(fit, band = TRUE, band_type = "slope")
#' 
#' # Mean-curve band (includes intercept uncertainty)
#' plot(fit, band = TRUE, band_type = "mean")
#' 
#' # Prediction band (mean band + residual)
#' plot(fit, band = TRUE, band_type = "prediction", band_args = list(alpha = 0.12))
#' 
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
#' @exportS3Method print slopeSErel
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
#' \code{slope_se_reliability()}. Optionally adds a confidence band using
#' the reliability-based error model (\eqn{\sigma_e = SD\sqrt{1-r}}).
#'
#' \strong{Band types}
#' \itemize{
#'   \item \code{"slope"}: slope-only band (half-width = \eqn{z_{1-p/2}\,|t-\bar t|\,SE(\hat\beta_1)})
#'   \item \code{"mean"}: mean curve band (includes intercept variance and slope–intercept covariance)\\
#'         half-width = \eqn{z_{1-p/2}\,\sigma_e\,\sqrt{1/n + (t-\bar t)^2/S_{xx}}}
#'   \item \code{"prediction"}: prediction band (mean band plus residual)\\
#'         half-width = \eqn{z_{1-p/2}\,\sigma_e\,\sqrt{1 + 1/n + (t-\bar t)^2/S_{xx}}}
#' }
#' All bands use the z reference because the SEs are based on a plug-in \eqn{\sigma_e}
#' from reliability (see McAleavey, 2024).
#'
#' @param x A \code{slopeSErel} object from \code{\link{slope_se_reliability}}.
#' @param y Ignored (kept for S3 signature compatibility with \code{plot()}).
#' @param annotate Logical; if \code{TRUE}, adds a label with RTI, z-crit, and p.
#' @param digits Integer; rounding for numeric labels.
#' @param point_args Named list of args for \code{ggplot2::geom_point()} (e.g., \code{list(size=2)}).
#' @param line_args  Named list of args for \code{ggplot2::geom_line()} (e.g., \code{list(linewidth=0.9)}).
#' @param band Logical; if \code{TRUE}, adds a \code{geom_ribbon()} band.
#' @param band_type One of \code{c("slope","mean","prediction")}; default \code{"slope"}.
#' @param band_args Named list of args for \code{ggplot2::geom_ribbon()} (e.g., \code{list(alpha=0.15)}).
#' @param grid_n Integer; number of time points in the grid for line/band.
#' @param ... Unused; included for S3 compatibility.
#'
#' @return A \code{ggplot} object.
#'
#' @references
#' Jacobson & Truax (1991). \emph{JCCP}, 59, 12–19.
#' McAleavey (2024). \emph{Clinical Psychology: Science and Practice}, 31(3), 351–366. doi:10.1037/cps0000203
#'
#' @export
#' @method plot slopeSErel
plot.slopeSErel <- function(x, y = NULL,
                            annotate = TRUE,
                            digits = 2,
                            point_args = list(),
                            line_args  = list(),
                            band = FALSE,
                            band_type = c("slope","mean","prediction"),
                            band_args = list(alpha = 0.15),
                            grid_n = 200,
                            ...) {
  
  band_type <- match.arg(band_type)
  
  # Ensure object contains what we need (slope_se_reliability stores these)
  if (is.null(x$time_vec) || is.null(x$y) || is.null(x$intercept_hat)) {
    stop("This slopeSErel object lacks `time_vec`, `y`, and/or `intercept_hat`.\n",
         "Recompute with the updated slope_se_reliability() that stores these fields.")
  }
  if (!is.finite(x$S_xx) || x$S_xx <= 0) {
    stop("S_xx must be positive to plot a line/band.")
  }
  
  # Data
  df_pts <- data.frame(time = as.numeric(x$time_vec),
                       score = as.numeric(x$y))
  n    <- length(df_pts$time)
  tbar <- mean(df_pts$time, na.rm = TRUE)
  
  # Dense grid for smooth line/band
  time_grid <- seq(min(df_pts$time, na.rm = TRUE),
                   max(df_pts$time, na.rm = TRUE),
                   length.out = max(2L, grid_n))
  df_fit <- data.frame(time = time_grid)
  df_fit$y_hat <- x$intercept_hat + x$slope_hat * df_fit$time
  
  # Compute half-width if band requested
  df_band <- NULL
  if (isTRUE(band)) {
    if (!is.finite(x$sigma_e) || !is.finite(x$crit)) {
      warning("Band not added: `sigma_e` or `crit` is not finite.")
    } else {
      if (band_type == "slope") {
        # slope-only band: z * |t - tbar| * SE(beta1); SE(beta1) = sigma_e / sqrt(S_xx)
        halfwidth <- x$crit * abs(df_fit$time - tbar) * (x$sigma_e / sqrt(x$S_xx))
      } else if (band_type == "mean") {
        # mean band: z * sigma_e * sqrt(1/n + (t - tbar)^2 / S_xx)
        halfwidth <- x$crit * x$sigma_e * sqrt(1/n + (df_fit$time - tbar)^2 / x$S_xx)
      } else { # prediction
        # prediction band: z * sigma_e * sqrt(1 + 1/n + (t - tbar)^2 / S_xx)
        halfwidth <- x$crit * x$sigma_e * sqrt(1 + 1/n + (df_fit$time - tbar)^2 / x$S_xx)
      }
      df_band <- data.frame(
        time   = df_fit$time,
        ymin   = df_fit$y_hat - halfwidth,
        ymax   = df_fit$y_hat + halfwidth
      )
    }
  }
  
  # Build plot with no global aes; each layer sets its own mapping; never inherit
  p <- ggplot2::ggplot()
  
  # Points
  args_points <- utils::modifyList(
    list(
      data        = df_pts,
      mapping     = ggplot2::aes(x = time, y = score),
      inherit.aes = FALSE
    ),
    point_args
  )
  p <- p + do.call(ggplot2::geom_point, args_points)
  
  # Ribbon (optional)
  if (!is.null(df_band)) {
    args_ribbon <- utils::modifyList(
      list(
        data        = df_band,
        mapping     = ggplot2::aes(x = time, ymin = ymin, ymax = ymax),
        inherit.aes = FALSE
      ),
      band_args
    )
    p <- p + do.call(ggplot2::geom_ribbon, args_ribbon)
  }
  
  # Fitted line
  args_line <- utils::modifyList(
    list(
      data        = df_fit,
      mapping     = ggplot2::aes(x = time, y = y_hat),
      inherit.aes = FALSE
    ),
    line_args
  )
  p <- p + do.call(ggplot2::geom_line, args_line)
  
  # Labels / theme
  p <- p +
    ggplot2::labs(
      x = "Time",
      y = "Score",
      title    = "Reliable Trend (reliability-based slope)",
      subtitle = sprintf("Slope = %.*f, RTI = %.*f (z_crit = %.*f, p = %.*g)",
                         digits, x$slope_hat, digits, x$RTI, digits, x$crit, digits, x$p_value),
      caption  = sprintf("r = %.2f, SD = %.2f; SE = SD*sqrt(1-r)/sqrt(S_xx); S_xx = %.*f",
                         x$reliability_r, x$sd_single, digits, x$S_xx)
    ) +
    ggplot2::theme_minimal(base_size = 12)
  
  # Optional annotation
  if (isTRUE(annotate)) {
    lab <- sprintf("RTI = %.*f\nz_crit = %.*f\np = %.*g",
                   digits, x$RTI, digits, x$crit, digits, x$p_value)
    p <- p + ggplot2::annotate(
      "label",
      x = max(df_pts$time, na.rm = TRUE),
      y = max(df_pts$score, na.rm = TRUE),
      label = lab, hjust = 1, vjust = 1
    )
  }
  
  p
}
