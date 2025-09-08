#' Predict from a reliableTrend object
#'
#' Returns fitted values for given time points, with optional standard errors
#' and intervals that mirror the band options used in \code{plot.reliableTrend()}.
#'
#' @param object A \code{reliableTrend} from \code{\link{rti}}.
#' @param newdata Optional numeric vector of time points at which to predict.
#'   Defaults to the original times stored in \code{object$t}.
#' @param se.fit Logical; return the standard error per point?
#' @param interval One of \code{"none"}, \code{"mean"}, \code{"prediction"},
#'   or \code{"slope"}. See Details.
#' @param level Confidence level for intervals (defaults to \code{object$level}).
#' @param include_intercept_uncertainty Logical; applies to \code{interval = "mean"}.
#'   If \code{TRUE}, includes the \eqn{1/n} term; if \code{FALSE}, drops it.
#' @param ... Ignored.
#'
#' @details
#' Let \eqn{t_c = t - \bar t} where \eqn{\bar t} is the mean of the original times.
#' Fitted values are \eqn{\hat y(t) = \hat\beta_0 + \hat\beta_1 t_c}.
#'
#' Standard errors used:
#' \itemize{
#'   \item \code{interval = "mean"}:
#'     \eqn{\mathrm{SE} = \sqrt{\sigma^2 (1/n + t_c^2 / S_{xx})}}
#'     (drop \eqn{1/n} if \code{include_intercept_uncertainty = FALSE}).
#'   \item \code{interval = "prediction"}:
#'     \eqn{\mathrm{SE} = \sqrt{\sigma^2 (1 + 1/n + t_c^2 / S_{xx})}}.
#'   \item \code{interval = "slope"}:
#'     \eqn{\mathrm{SE} = \mathrm{SE}(\hat\beta_1) \cdot |t_c|}.
#' }
#'
#' @return If \code{interval = "none"} and \code{se.fit = FALSE}, a numeric vector
#'   of fitted \eqn{\hat y}. Otherwise a \code{data.frame} with columns:
#'   \code{fit}, optionally \code{se.fit}, and (when intervals requested)
#'   \code{lwr}, \code{upr}.
#'
#' @export
predict.reliableTrend <- function(object,
                                  newdata = NULL,
                                  se.fit = FALSE,
                                  interval = c("none", "mean", "prediction", "slope"),
                                  level = object$level %||% 0.95,
                                  include_intercept_uncertainty = TRUE,
                                  ...) {
  if (!inherits(object, "reliableTrend")) stop("`object` must be a reliableTrend.", call. = FALSE)
  interval <- match.arg(interval)
  if (is.null(newdata)) newdata <- object$t
  if (!is.numeric(newdata) || !all(is.finite(newdata))) stop("`newdata` must be finite numeric.", call. = FALSE)
  
  tbar <- mean(object$t)
  tc   <- as.numeric(newdata) - tbar
  fit  <- object$intercept + object$estimate * tc
  
  sigma2 <- object$sigma2
  n  <- object$n
  Sxx <- object$Sxx
  
  se_line <- switch(
    interval,
    none = rep_len(NA_real_, length(tc)),
    mean = {
      base <- sqrt(sigma2 * (tc^2 / Sxx))
      if (isTRUE(include_intercept_uncertainty)) {
        sqrt(sigma2 * (1 / n + tc^2 / Sxx))
      } else {
        base
      }
    },
    prediction = sqrt(sigma2 * (1 + 1 / n + tc^2 / Sxx)),
    slope = abs(tc) * object$se
  )
  
  if (interval == "none" && !se.fit) {
    return(fit)
  }
  
  zcrit <- stats::qnorm(1 - (1 - level) / 2)
  out <- data.frame(fit = fit)
  if (interval != "none") {
    out$lwr <- fit - zcrit * se_line
    out$upr <- fit + zcrit * se_line
  }
  if (isTRUE(se.fit)) out$se.fit <- se_line
  out
}

# small infix (shared with other files if not already present)
`%||%` <- function(x, y) if (is.null(x)) y else x
