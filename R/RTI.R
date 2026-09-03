#' Reliable Trend Index (RTI)
#'
#' Compute a reliability-based test of within-person linear trend.
#' This is the supported single-series RTI interface. New code should use
#' \code{y}, \code{t}, and either \code{sd}/\code{r}, \code{sem}, or
#' \code{sdiff}; the older \code{values} and \code{time} aliases remain for
#' compatibility.
#' Backward compatible with older code that passed \code{values}/\code{time}
#' and/or \code{sem} instead of \code{sd}/\code{r}. You can also pass
#' \code{sdiff}, the standard error of the difference (RCI SE); if provided,
#' it takes precedence over \code{sem} and \code{sd}/\code{r}.
#'
#' @param y,values Numeric vector of within-person observations (length \eqn{n \ge 2}).
#'   \code{values} is a legacy alias for \code{y}; if both are supplied, \code{y} is used.
#' @param sd Positive numeric. Single-occasion standard deviation (external).
#' @param r Numeric in \eqn{[0, 1]}. Reliability (external).
#' @param sem Optional positive numeric. Standard error of measurement (external).
#'   If provided (and \code{sdiff} is not), it takes precedence and sets
#'   \eqn{\sigma^2 = \mathrm{sem}^2}.
#' @param sdiff Optional positive numeric. \emph{Standard error of the difference}
#'   used in the RCI: \eqn{\mathrm{sdiff} = SD\sqrt{2(1-r)} = \sqrt{2}\,\mathrm{sem}}.
#'   If provided, it takes precedence and the slope SE is \eqn{\mathrm{sdiff}/\sqrt{2 S_{xx}}}.
#' @param t,time Optional numeric vector of time indices (same length as \code{y}).
#'   \code{time} is a legacy alias for \code{t}. If both are missing, uses \code{t = 1:n}.
#' @param na.rm Logical. Drop incomplete \code{(y, t)} pairs? Default \code{FALSE}.
#' @param level Confidence level for slope intervals (default \code{0.95}).
#'
#' @return An object of class \code{"reliableTrend"} with elements:
#' \itemize{
#'   \item \code{estimate}, \code{intercept}, \code{se}, \code{z}, \code{p}, \code{ci}
#'   \item \code{sigma2}, \code{t}, \code{t_centered}, \code{y}, \code{Sxx}, \code{n}
#'   \item \code{sd}, \code{r}, \code{sem}, \code{sdiff}, \code{level}, \code{call}
#' }
#' @examples
#' # New style (sd & r)
#' rti(y = c(12, 11, 13, 16), sd = 8, r = 0.85)
#'
#' # Legacy style (values & sem)
#' rti(values = c(12, 11, 13, 16), sem = 3)
#'
#' # Provide sdiff directly (takes precedence over sem and sd/r)
#' rti(y = c(12, 11, 13, 16), sdiff = 8 * sqrt(2 * (1 - 0.85)))
#' @export
rti <- function(y = NULL, sd = NULL, r = NULL,
                t = NULL, na.rm = FALSE, level = 0.95,
                values = NULL, time = NULL, sem = NULL, sdiff = NULL) {
  cl <- match.call()
  used_values_alias <- !is.null(values) && is.null(y)
  if (used_values_alias) y <- values
  if (!is.null(time)   && is.null(t)) t <- time
  # Older workflows interpreted a scalar values argument as a change from a
  # zero baseline. Keep that behavior only for the legacy alias.
  if (used_values_alias && is.numeric(y) && length(y) == 1L) {
    y <- c(0, y)
    cl$values <- as.call(list(as.name("c"), 0, y[2L]))
  }
  .rti_compute(y = y, sd = sd, r = r, t = t, na.rm = na.rm, level = level,
               sem = sem, sdiff = sdiff, call = cl)
}
