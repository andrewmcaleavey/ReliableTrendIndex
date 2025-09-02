#' Plot a reliableTrend object
#'
#' Produces a ggplot with the fitted line and an uncertainty band. Choose among:
#' \describe{
#'   \item{\code{"slope"}}{Band reflects uncertainty in the \emph{slope} only:
#'     \eqn{\pm z \cdot \mathrm{SE}(\hat\beta_1) \cdot |t_c|}. Intercept treated as fixed.}
#'   \item{\code{"mean"}}{Confidence band for the fitted mean:
#'     \eqn{\mathrm{SE}(\hat y_t) = \sqrt{\sigma^2 (1/n + t_c^2/S_{xx})}} using
#'     \eqn{\sigma^2 = \mathrm{sd}^2 (1-r)}. If \code{include_intercept_uncertainty = FALSE},
#'     drops the \eqn{1/n} term, reducing to slope-only variance.}
#'   \item{\code{"prediction"}}{Prediction band for a new observation:
#'     \eqn{\mathrm{SE}_{\mathrm{pred}}(t) = \sqrt{\sigma^2 (1 + 1/n + t_c^2/S_{xx})}}.}
#' }
#'
#' Time is internally centered (\eqn{t_c = t - \bar t}) so that the slope-only band
#' pivots around the mean time. The plotted intercept equals \code{mean(y)}.
#'
#' @param x A \code{reliableTrend} object from \code{\link{rti}}.
#' @param band One of \code{c("slope","mean","prediction")}. Default \code{"slope"}.
#' @param include_intercept_uncertainty Logical. Applies to \code{band = "mean"}:
#' include the \eqn{1/n} term (default \code{TRUE}). Ignored otherwise.
#' @param level Confidence level (default taken from object if not supplied).
#' @param show_points,show_line Logical. Toggle observed points and fitted line.
#' @param ... Passed to \code{ggplot2::geom_line()} for the fitted line (e.g., linewidth).
#'
#' @return A \code{ggplot} object.
#' @examples
#' \dontrun{
#' y <- c(12, 11, 13, 16, 17, 19)
#' fit <- rti(y, sd = 8, r = 0.85)
#' plot(fit, band = "mean", include_intercept_uncertainty = TRUE, level = 0.95)
#' }
#' @export
#' @method plot reliableTrend
plot.reliableTrend <- function(x,
                               band = c("slope", "mean", "prediction"),
                               include_intercept_uncertainty = TRUE,
                               level = x$level %||% 0.95,
                               show_points = TRUE,
                               show_line = TRUE,
                               ...) {
  band <- match.arg(band)
  df <- data.frame(t = x$t, y = x$y)
  preds <- predict(x,
                   newdata = x$t,
                   interval = switch(band,
                                     slope = "slope",
                                     mean = "mean",
                                     prediction = "prediction"),
                   level = level,
                   include_intercept_uncertainty = include_intercept_uncertainty)
  df$yhat <- preds$fit
  df$ymin <- preds$lwr
  df$ymax <- preds$upr
  
  p <- ggplot2::ggplot(df, ggplot2::aes(x = t, y = y)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = ymin, ymax = ymax), alpha = 0.15) +
    (if (show_line) ggplot2::geom_line(ggplot2::aes(y = yhat), ...) else NULL) +
    (if (show_points) ggplot2::geom_point() else NULL) +
    ggplot2::labs(
      x = "Time", y = "Outcome", title = "Reliable Trend (RTI)",
      subtitle = paste0(
        "Slope = ", signif(x$estimate, 4L),
        " | Z = ", signif(x$z, 4L),
        " | p = ", signif(x$p, 4L),
        " | Band: ", band,
        if (band == "mean")
          paste0(" (intercept ", if (include_intercept_uncertainty) "included" else "fixed", ")")
        else ""
      )
    ) +
    ggplot2::theme_minimal() +
    ggplot2::guides(fill = "none", color = "none")
  
  p
}

# small infix helper (avoids importing rlang)
`%||%` <- function(x, y) if (is.null(x)) y else x
