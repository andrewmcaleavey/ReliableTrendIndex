#' ReliableTrendIndex: reliability-based change and trend analyses
#'
#' The supported analysis interface is deliberately small:
#' \itemize{
#'   \item \code{rti()} computes a single-series Reliable Trend Index.
#'   \item \code{rci()} computes a Reliable Change Index.
#'   \item \code{rti_by()} applies \code{rti()} to long-format grouped data.
#' }
#'
#' All three functions use externally supplied measurement-error information.
#' For \code{rti()} and \code{rti_by()}, the primary parameterization is a
#' single-occasion standard deviation (\code{sd}) and reliability (\code{r}).
#' \code{rti()} also accepts \code{sem} or \code{sdiff} when those quantities
#' are the directly available external inputs.
#'
#' @section API lifecycle:
#' These functions are the supported API. Historical helpers remain available
#' for compatibility during the transition, but new analyses should use
#' \code{rti()}, \code{rci()}, and \code{rti_by()}.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom metafor rma
#' @importFrom stats predict
## usethis namespace: end
NULL
