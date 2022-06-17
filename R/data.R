# data

#' Synthetic change data.
#'
#' A long-format (tidy) dataset of simulated generic scores from an imaginary scale score
#'
#' @format A data frame with 1081 rows and 10 variables:
#' \describe{
#'   \item{id}{Identifier}
#'   \item{num_obs}{Number of observations of this individual in the data}
#'   \item{BL_true}{True score at baseline}
#'   \item{Final_true}{True score at end of observation}
#'   \item{diff_true}{True difference score}
#'   \item{index}{Observation number, starts at 0}
#'   \item{increment}{Linear change per observation for this individual}
#'   \item{true_score}{True score at every observation}
#'   \item{true_change}{True change category}
#'   \item{obs_score}{Observed score at every observation}
#' }
#' @source \code{data-raw/simulated_data.R}
"simulated_data"
