# data

#' Synthetic change data.
#'
#' A long-format (tidy) dataset of simulated generic scores from an imaginary scale score
#'
#' @format A data frame with 5534 rows and 10 variables:
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

#' Jacobson & Truax (1991) data for a single case example
#' 
#' A `data.frame` containing the sample data for one patient provided in Jacobson & Truax (1991). 
#' 
#' @format A data.frame with 2 rows and 2 variables:  
#' \describe{
#'   \item{obs}{Observed score.}
#'   \item{time}{Indicator for time.}
#' }
#' @source Jacobson, N. S. and Truax, P. (1991). Clinical significance: A statistical 
#' approach to defining meaningful change in psychotherapy research. Journal of Consulting 
#' and Clinical Psychology, 59, p. 12 - 19. 
"jt_example_data_1"

#' Mac height data
#' 
#' Example data for a single case
#' 
#' A `data.frame` containing the sample data for one individual measured at six times. 
#' 
#' @format A data.frame with 2 rows and 2 variables:  
#' \describe{
#'   \item{obs}{Observed score.}
#'   \item{time}{Indicator for time.}
#' }
#' @source McAleavey 2022 
"mac_height"

#' Liu Data
#' 
#' Data from a single case using the PHQ-9 Modified for Adolescents. 
#' Presented in Liu, F. F., & Adrian, M. C. (2019). Is treatment working? Detecting real 
#' change in the treatment of child and adolescent depression. Journal of the American 
#' Academy of Child & Adolescent Psychiatry, 58(12), 1157-1164.
#' 
#' A `data.frame` containing the sample data for one individual measured at 12 times. 
#' 
#' @format A data.frame with 12 rows and 2 variables:  
#' \describe{
#'   \item{obs}{Observed score.}
#'   \item{time}{Indicator for time.}
#' }
#' @source Liu, F. F., & Adrian, M. C. (2019). Is treatment working? Detecting real 
#' change in the treatment of child and adolescent depression. Journal of the American 
#' Academy of Child & Adolescent Psychiatry, 58(12), 1157-1164. 
"liu_data"
