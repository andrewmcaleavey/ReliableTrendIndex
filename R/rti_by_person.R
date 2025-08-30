# with assistance from ChatGPT 5, 2025-08-28

#' Reliable Trend Index (RTI) by person
#'
#' @title Compute the Reliable Trend Index (RTI) for each person
#'
#' @description
#' Computes the **Reliable Trend Index (RTI)** for each person in a long-format
#' data frame containing a person identifier, an outcome/score, and (optionally)
#' a time variable. The RTI is a reliability-adjusted t-like statistic for the
#' within-person linear trend (slope) of \code{y} on \code{time}.
#'
#' @details
#' \strong{Concept.} For one individual observed over time, fit the simple regression
#' \eqn{y_t = \beta_0 + \beta_1 t + \varepsilon_t}. The OLS slope estimate is
#' \deqn{\hat\beta_1 = \frac{\sum (t-\bar t)(y_t-\bar y)}{\sum (t-\bar t)^2}.}
#' Its standard error can be written as
#' \deqn{SE(\hat\beta_1) = \frac{\sigma_e}{\sqrt{\sum (t-\bar t)^2}},}
#' where \eqn{\sigma_e} is the residual standard deviation. If one treats
#' measurement error as the dominant source of residual variation and plugs in
#' the instrument's reliability \eqn{r} and single-occasion standard deviation \eqn{SD},
#' we use \eqn{\sigma_e \approx SD \sqrt{1-r}}.
#'
#' For equally spaced times \eqn{t = 1,\dots,n}, \eqn{\sum (t-\bar t)^2 = n(n^2-1)/12}.
#' The resulting \emph{Reliable Trend Index} is
#' \deqn{\mathrm{RTI} = \frac{\hat\beta_1}{SD \sqrt{1-r}\ /\ \sqrt{\sum (t-\bar t)^2}}
#' \;=\; \hat\beta_1 \cdot \frac{\sqrt{\sum (t-\bar t)^2}}{SD \sqrt{1-r}}.}
#'
#' \strong{Relationship to RCI.} When \eqn{n=2} and times are \eqn{t=1,2},
#' the slope reduces to \eqn{\hat\beta_1 = y_2 - y_1}, and
#' \eqn{\sum (t-\bar t)^2 = 1/2}. Then
#' \deqn{SE(\hat\beta_1) = SD \sqrt{2(1-r)}, \quad
#' \mathrm{RTI} = \frac{y_2 - y_1}{SD \sqrt{2(1-r)}},}
#' which is exactly the \emph{Reliable Change Index} (RCI; Jacobson & Truax, 1991).
#' Thus, the RCI is the \eqn{n=2} special case of the RTI.
#'
#' \strong{What this function does.}
#' For each person (identified by \code{id}), the function:
#' \enumerate{
#'   \item sorts by \code{time} if supplied; otherwise uses the observed order and
#'         defines time as \eqn{1{:}n_i},
#'   \item fits \code{lm(y ~ time)} to obtain \eqn{\hat\beta_1},
#'   \item computes \eqn{S_{xx} = \sum (t-\bar t)^2},
#'   \item constructs \eqn{SE_{\text{reliability}} = SD \sqrt{1-r} / \sqrt{S_{xx}}},
#'   \item returns \eqn{\mathrm{RTI} = \hat\beta_1 / SE_{\text{reliability}}} and components.
#' }
#'
#' \strong{Supplying the single-occasion SD.}
#' You can provide \code{sd_single} as a scalar for all persons (e.g., from a manual).
#' If \code{sd_single = NULL}, choose \code{sd_method}:
#' \itemize{
#'   \item \code{"global"}: uses the SD of all available \code{y} across persons.
#'   \item \code{"per_person"}: uses each person’s SD of \code{y}.
#'   \item \code{"baseline"}: uses the SD of \code{y} across persons at a specified
#'         \code{baseline_time} (requires a \code{time} column).
#' }
#'
#' \strong{Assumptions & cautions.}
#' \itemize{
#'   \item The reliability plug-in \eqn{\sigma_e \approx SD \sqrt{1-r}} assumes measurement
#'         error dominates residual variation around a linear trend. Additional true
#'         within-person scatter will make the RTI optimistic (too large).
#'   \item If \eqn{S_{xx} = 0} (e.g., constant time), the RTI is undefined.
#'   \item If the chosen \code{SD} is zero/non-finite for a person, the RTI is undefined.
#'   \item If \eqn{r=1}, then \eqn{\sigma_e=0} and the RTI diverges; in practice,
#'         use \eqn{r<1}.
#' }
#'
#' @param df A \code{data.frame} in long format containing at least an identifier
#'   column, an outcome column, and optionally a time column.
#' @param id Unquoted column name in \code{df} for the person identifier.
#' @param y Unquoted column name in \code{df} for the outcome/score.
#' @param time Unquoted column name in \code{df} for time (numeric). If omitted,
#'   each person’s time is set to \eqn{1{:}n_i} in their observed order.
#' @param r A scalar reliability coefficient in \code{[0, 1]}.
#' @param sd_single Optional scalar single-occasion SD used for all persons. If
#'   \code{NULL}, \code{sd_method} determines how the SD is obtained.
#' @param sd_method Character; one of \code{"global"}, \code{"per_person"}, or
#'   \code{"baseline"}. Only used when \code{sd_single = NULL}. See Details.
#' @param baseline_time A single value of \code{time} indicating the baseline
#'   occasion when \code{sd_method = "baseline"}. Ignored otherwise.
#' @param na_rm Logical; if \code{TRUE}, drop rows with missing \code{id}, \code{y},
#'   or \code{time} (if supplied). If \code{FALSE}, \code{NA}s will trigger an error.
#'
#' @return A \code{data.frame} with one row per person containing:
#' \describe{
#'   \item{\code{id}}{Person identifier.}
#'   \item{\code{n}}{Number of observations used for that person.}
#'   \item{\code{S_xx}}{\eqn{\sum (t-\bar t)^2} for that person.}
#'   \item{\code{slope_hat}}{OLS slope \eqn{\hat\beta_1} from \code{lm(y ~ time)}.}
#'   \item{\code{sd_single_used}}{Single-occasion SD used for that person.}
#'   \item{\code{sigma_e}}{\eqn{SD \sqrt{1-r}}.}
#'   \item{\code{SE_reliability}}{\eqn{SD \sqrt{1-r} / \sqrt{S_{xx}}}.}
#'   \item{\code{RTI}}{\eqn{\hat\beta_1 / SE_{\text{reliability}}}.}
#' }
#'
#' @references
#' Jacobson, N. S., & Truax, P. (1991). Clinical significance: A statistical
#' approach to defining meaningful change in psychotherapy research.
#' \emph{Journal of Consulting and Clinical Psychology}, 59(1), 12–19.
#' \doi{10.1037/0022-006X.59.1.12}
#' 
#' McAleavey, A. A. (2024). When (not) to rely on the reliable change index: A 
#' critical appraisal and alternatives to consider in clinical psychology. 
#' \emph{Clinical Psychology: Science and Practice}, 31(3), 351–366. 
#' \doi{10.1037/cps0000203}
#'
#' @seealso \code{\link[stats]{lm}}, \code{\link[stats]{sd}}
#'
#' @examples
#' # Toy data: two persons, uneven time spacing for id B
#' set.seed(1)
#' df <- data.frame(
#'   id   = rep(c("A", "B"), each = 5),
#'   time = c(1,2,3,4,5,  0,1,3,4,7),
#'   y    = c(20,18,17,16,15,  12,11,10,10,9)
#' )
#'
#' # Reliability r and global SD (computed automatically)
#' rti_by_person(df, id, y, time, r = 0.90, sd_single = NULL, sd_method = "global")
#'
#' # Provide a manual SD for the instrument (same for everyone)
#' rti_by_person(df, id, y, time, r = 0.90, sd_single = 8)
#'
#' # Per-person SDs
#' rti_by_person(df, id, y, time, r = 0.85, sd_method = "per_person")
#'
#' # Baseline SD across persons at time == 1
#' rti_by_person(df, id, y, time, r = 0.85, sd_method = "baseline", baseline_time = 1)
#'
#' # Without an explicit time column: uses 1:n_i in observed order
#' df2 <- df[, c("id","y")]
#' rti_by_person(df2, id, y, r = 0.9, sd_method = "global")
#'
#' @importFrom stats lm coef sd
#' @export
rti_by_person <- function(df, id, y, time = NULL, r,
                          sd_single = NULL,
                          sd_method = c("global", "per_person", "baseline"),
                          baseline_time = NULL,
                          na_rm = TRUE) {
  stopifnot(is.data.frame(df))
  # NSE capture
  id_sym   <- substitute(id)
  y_sym    <- substitute(y)
  time_sym <- substitute(time)
  
  id_name <- deparse(id_sym)
  y_name  <- deparse(y_sym)
  
  if (!id_name %in% names(df)) stop("`id` column not found in `df`.")
  if (!y_name %in% names(df))  stop("`y` column not found in `df`.")
  
  has_time <- !is.symbol(time_sym)
  if (has_time) {
    time_name <- deparse(time_sym)
    if (!time_name %in% names(df)) stop("`time` column not found in `df`.")
  } else {
    time_name <- NULL
  }
  
  if (!is.numeric(r) || length(r) != 1L || r < 0 || r > 1) {
    stop("`r` must be a single numeric in [0, 1].")
  }
  
  # Build clean working data.frame
  dat <- data.frame(
    id   = df[[id_name]],
    y    = df[[y_name]],
    time = if (has_time) df[[time_name]] else NA_real_
  )
  
  # Handle missingness
  if (na_rm) {
    if (has_time) {
      keep <- !(is.na(dat$id) | is.na(dat$y) | is.na(dat$time))
    } else {
      keep <- !(is.na(dat$id) | is.na(dat$y))
    }
    dat <- dat[keep, , drop = FALSE]
  } else {
    if (any(is.na(dat$id)) || any(is.na(dat$y)) || (has_time && any(is.na(dat$time)))) {
      stop("Missing values present. Set `na_rm = TRUE` to drop incomplete rows.")
    }
  }
  if (nrow(dat) == 0L) stop("No complete cases to analyze.")
  
  # Decide SD source
  if (is.null(sd_single)) {
    sd_method <- match.arg(sd_method)
    if (sd_method == "global") {
      sd_global <- stats::sd(dat$y)
      if (!is.finite(sd_global) || sd_global <= 0) {
        stop("Global SD is zero or not finite; cannot compute RTI.")
      }
      sd_get <- function(id_val, y_vec, t_vec) sd_global
    } else if (sd_method == "per_person") {
      sd_get <- function(id_val, y_vec, t_vec) {
        s <- stats::sd(y_vec)
        if (!is.finite(s) || s <= 0) NA_real_ else s
      }
    } else { # baseline
      if (!has_time) stop("`sd_method = \"baseline\"` requires a `time` column.")
      if (is.null(baseline_time)) stop("Provide `baseline_time` for `sd_method = \"baseline\"`.")
      y_base <- dat$y[dat$time == baseline_time]
      if (length(y_base) < 2L) stop("Not enough observations at `baseline_time` to compute SD.")
      sd_base <- stats::sd(y_base)
      if (!is.finite(sd_base) || sd_base <= 0) {
        stop("Baseline SD is zero or not finite; cannot compute RTI.")
      }
      sd_get <- function(id_val, y_vec, t_vec) sd_base
    }
  } else {
    if (!is.numeric(sd_single) || length(sd_single) != 1L || sd_single <= 0) {
      stop("`sd_single` must be a positive numeric scalar if supplied.")
    }
    sd_get <- function(id_val, y_vec, t_vec) sd_single
  }
  
  # Split by person and compute
  split_idx <- split(seq_len(nrow(dat)), dat$id)
  out_list <- lapply(names(split_idx), function(id_val) {
    rows <- split_idx[[id_val]]
    y_i  <- dat$y[rows]
    
    if (has_time) {
      t_i <- dat$time[rows]
      # sort by time (stable)
      ord <- order(t_i, method = "auto")
      t_i <- t_i[ord]
      y_i <- y_i[ord]
    } else {
      # use observed order: define time = 1:n_i
      t_i <- seq_along(y_i)
    }
    
    n_i <- length(y_i)
    if (n_i < 2L) {
      return(data.frame(
        id = id_val, n = n_i, S_xx = NA_real_,
        slope_hat = NA_real_, sd_single_used = NA_real_,
        sigma_e = NA_real_, SE_reliability = NA_real_, RTI = NA_real_
      ))
    }
    
    tbar <- mean(t_i)
    S_xx <- sum((t_i - tbar)^2)
    
    if (!is.finite(S_xx) || S_xx <= 0) {
      return(data.frame(
        id = id_val, n = n_i, S_xx = S_xx,
        slope_hat = NA_real_, sd_single_used = NA_real_,
        sigma_e = NA_real_, SE_reliability = NA_real_, RTI = NA_real_
      ))
    }
    
    # OLS slope
    fit <- stats::lm(y_i ~ t_i)
    slope_hat <- unname(stats::coef(fit)[2L])
    
    # Single-occasion SD and reliability plug-in
    sd_use <- sd_get(id_val, y_i, t_i)
    if (!is.finite(sd_use) || sd_use <= 0) {
      return(data.frame(
        id = id_val, n = n_i, S_xx = S_xx,
        slope_hat = slope_hat, sd_single_used = sd_use,
        sigma_e = NA_real_, SE_reliability = NA_real_, RTI = NA_real_
      ))
    }
    
    sigma_e <- sd_use * sqrt(1 - r)
    if (!is.finite(sigma_e) || sigma_e < 0) sigma_e <- NA_real_
    
    SE_rel <- if (is.na(sigma_e)) NA_real_ else sigma_e / sqrt(S_xx)
    RTI    <- if (is.na(SE_rel) || SE_rel == 0) NA_real_ else slope_hat / SE_rel
    
    data.frame(
      id = id_val,
      n = n_i,
      S_xx = S_xx,
      slope_hat = slope_hat,
      sd_single_used = sd_use,
      sigma_e = sigma_e,
      SE_reliability = SE_rel,
      RTI = RTI
    )
  })
  
  do.call(rbind, out_list)
}
