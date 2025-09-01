# with assistance from ChatGPT 5, 2025-08-28; updated 2025-09-01

#' Reliable Trend Index (RTI) by person
#'
#' @title Compute the Reliable Trend Index (RTI) for each person
#'
#' @description
#' Computes the **Reliable Trend Index (RTI)** for each person in a long-format
#' data frame containing a person identifier, an outcome/score, and (optionally)
#' a time variable. The RTI is a reliability-adjusted z-like statistic for the
#' within-person linear trend (slope) of \code{y} on \code{time}. Adds a
#' classification of the trend's reliability given a user-set two-sided
#' significance level \code{p} (default 0.05).
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
#' Thus, the RCI is the \eqn{n=2} special case of the RTI, and—given the same
#' \eqn{SD} and \eqn{r}—the magnitude of RTI can coincide with RCI when the
#' three-point series is perfectly linear (e.g., first and last points define the line).
#'
#' \strong{Threshold and classification.}
#' To interpret the magnitude of \code{RTI}, this function compares it to a
#' two-sided **standard normal** critical value \eqn{z_{1-p/2}} (default \code{p = 0.05}
#' gives \eqn{\pm 1.96}). The categorical variable \code{RTI_cat} is:
#' \itemize{
#'   \item \code{"Reliable increase"} if \eqn{RTI \ge z_{1-p/2}},
#'   \item \code{"Reliable decrease"} if \eqn{RTI \le -z_{1-p/2}},
#'   \item \code{"No reliable trend"} otherwise.
#' }
#' The z reference is used because the denominator is a plug-in standard error
#' derived from external reliability and a single-occasion SD, rather than an
#' SD estimated from the same series; a t reference would be unjustifiably conservative.
#'
#' \strong{Supplying the single-occasion SD.}
#' You can provide \code{sd_single} as a scalar for all persons (e.g., from a manual).
#' If \code{sd_single = NULL}, choose \code{sd_method}:
#' \itemize{
#'   \item \code{"global"}: uses the SD of all available \code{y} across persons.
#'   \item \code{"per_person"}: uses each person’s SD of \code{y}.
#'   \item \code{"baseline"}: uses the SD of one \emph{baseline} observation per person.  
#'         If a \code{time} column is supplied and \code{baseline_time} is set, the
#'         baseline is \code{time == baseline_time}. If \code{time} is supplied and
#'         \code{baseline_time} is missing, the baseline is each person’s earliest
#'         time. If \code{time} is not supplied, the baseline is the \emph{first
#'         observed} measurement per person.
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
#'   occasion when \code{sd_method = "baseline"}. Ignored when \code{time} is omitted.
#' @param p Two-sided significance level for the RTI threshold (default \code{0.05}).
#'   Internally uses \eqn{z_{1-p/2}} for all persons.
#' @param verbose Logical; if \code{FALSE} (default) return a compact table with
#'   \code{id}, \code{n}, \code{slope_hat}, \code{RTI}, \code{crit}, and \code{RTI_cat}. If
#'   \code{TRUE}, return all components as well.
#' @param na_rm Logical; if \code{TRUE}, drop rows with missing \code{id}, \code{y},
#'   or \code{time} (if supplied). If \code{FALSE}, \code{NA}s will trigger an error.
#'
#' @return If \code{verbose = FALSE}, a \code{data.frame} with per-person:
#' \describe{
#'   \item{\code{id}}{Person identifier.}
#'   \item{\code{n}}{Number of observations used for that person.}
#'   \item{\code{slope_hat}}{OLS slope \eqn{\hat\beta_1}.}
#'   \item{\code{RTI}}{\eqn{\hat\beta_1 / SE_{\text{reliability}}}.}
#'   \item{\code{crit}}{Critical value \eqn{z_{1-p/2}} used for classification.}
#'   \item{\code{RTI_cat}}{"Reliable increase", "Reliable decrease", or "No reliable trend".}
#' }
#' If \code{verbose = TRUE}, also includes:
#' \describe{
#'   \item{\code{S_xx}}{\eqn{\sum (t-\bar t)^2}.}
#'   \item{\code{sd_single_used}}{Single-occasion SD used.}
#'   \item{\code{sigma_e}}{\eqn{SD \sqrt{1-r}}.}
#'   \item{\code{SE_reliability}}{\eqn{SD \sqrt{1-r} / \sqrt{S_{xx}}}.}
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
#' @seealso \code{\link[stats]{lm}}, \code{\link[stats]{sd}}, \code{\link[stats]{qnorm}}
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
#' # Compact output with classification at p = 0.05 (z = 1.96)
#' rti_by_person(df, id, y, time, r = 0.90, sd_single = NULL, sd_method = "global")
#'
#' # Provide a manual SD for the instrument (same for everyone)
#' rti_by_person(df, id, y, time, r = 0.90, sd_single = 8)
#'
#' # Per-person SDs, verbose output to see components
#' rti_by_person(df, id, y, time, r = 0.85, sd_method = "per_person", verbose = TRUE)
#'
#' # Baseline SD across persons at time == 1
#' rti_by_person(df, id, y, time, r = 0.85, sd_method = "baseline", baseline_time = 1)
#'
#' # Without an explicit time column: uses 1:n_i in observed order
#' df2 <- df[, c("id","y")]
#' rti_by_person(df2, id, y, r = 0.9, sd_method = "global")
#'
#' # Baseline SD without a time column: first observed measurement per person
#' rti_by_person(df2, id, y, r = 0.9, sd_method = "baseline")
#'
#' # Three-point linear example; RTI equals the two-point RCI in magnitude
#' df3 <- data.frame(id = "A", time = 1:3, y = c(47.5, 40, 32.5))
#' rti_by_person(df3, id, y, time, r = 0.80, sd_single = 7.5, p = 0.05)
#' # This is due to lack of leverage for the middle point. Altering time changes this: 
#' # Here we alter the slope with the same y values
#' df3a <- data.frame(id = "A", time = c(1, 5, 6), y = c(47.5, 40, 32.5))
#' rti_by_person(df3a, id, y, time, r = 0.80, sd_single = 7.5, p = 0.05)
#' # Here we keep the same slope but space observations out
#' df3b <- data.frame(id = "A", time = c(1, 5, 6), y = c(47.5, 17.5, 10))
#' rti_by_person(df3b, id, y, time, r = 0.80, sd_single = 7.5, p = 0.05)
#' # and here we do the same within the same y-value range:
#' df3c <- data.frame(id = "A", time = c(1, 1.25, 3), y = c(47.5, 45.625, 32.5))
#' rti_by_person(df3c, id, y, time, r = 0.80, sd_single = 7.5, p = 0.05)
#' 
#' # Two-point RCI example from Jacobson & Truax (1991)
#' rti_by_person(df3[c(1,3),], id, y, time, r = 0.80, sd_single = 7.5, p = 0.05)
#' # See the same value in their paper
#' 
#' # Four points
#' df4 <- data.frame(id = "A", time = 1:4, y = c(47.5, 40, 32.5, 25))
#' rti_by_person(df4, id, y, time, r = 0.80, sd_single = 7.5, p = 0.05)
#'
#' # Stricter threshold (e.g., p = 0.01 -> z = 2.576)
#' rti_by_person(df, id, y, time, r = 0.90, sd_method = "global", p = 0.01)
#'
#' @importFrom stats lm coef sd qnorm
#' @export
rti_by_person <- function(df, id, y, time = NULL, r,
                          sd_single = NULL,
                          sd_method = c("global", "per_person", "baseline"),
                          baseline_time = NULL,
                          p = 0.05,
                          verbose = FALSE,
                          na_rm = TRUE) {
  stopifnot(is.data.frame(df))
  # NSE capture
  id_sym <- substitute(id)
  y_sym  <- substitute(y)
  
  id_name <- deparse(id_sym)
  y_name  <- deparse(y_sym)
  
  if (!id_name %in% names(df)) stop("`id` column not found in `df`.")
  if (!y_name %in% names(df))  stop("`y` column not found in `df`.")
  
  # Detect whether a usable time column was supplied
  has_time <- !(missing(time) || is.null(time))
  if (has_time) {
    time_sym  <- substitute(time)
    time_name <- deparse(time_sym)
    if (!time_name %in% names(df)) stop("`time` column not found in `df`.")
  } else {
    time_name <- NULL
  }
  
  if (!is.numeric(r) || length(r) != 1L || r < 0 || r > 1) {
    stop("`r` must be a single numeric in [0, 1].")
  }
  if (!is.numeric(p) || length(p) != 1L || p <= 0 || p >= 1) {
    stop("`p` must be a single numeric in (0, 1).")
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
      # build a vector of baseline y across persons
      split_idx_full <- split(seq_len(nrow(dat)), dat$id)
      if (has_time) {
        baseline_vals <- vapply(names(split_idx_full), function(id_val) {
          rows <- split_idx_full[[id_val]]
          t_i  <- dat$time[rows]
          y_i  <- dat$y[rows]
          if (!is.null(baseline_time)) {
            y_b <- y_i[t_i == baseline_time]
            if (length(y_b) < 1L) NA_real_ else y_b[1L]
          } else {
            ord <- order(t_i, method = "auto")
            y_i[ord][1L]
          }
        }, numeric(1))
      } else {
        baseline_vals <- vapply(names(split_idx_full), function(id_val) {
          rows <- split_idx_full[[id_val]]
          y_i  <- dat$y[rows]
          y_i[1L]
        }, numeric(1))
      }
      baseline_vals <- baseline_vals[is.finite(baseline_vals)]
      if (length(baseline_vals) < 2L) {
        stop("Not enough baseline observations across persons to compute SD.")
      }
      sd_base <- stats::sd(baseline_vals)
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
      ord <- order(t_i, method = "auto") # sort by time (stable)
      t_i <- t_i[ord]
      y_i <- y_i[ord]
    } else {
      t_i <- seq_along(y_i)              # observed order -> time = 1:n_i
    }
    
    n_i <- length(y_i)
    if (n_i < 2L) {
      crit <- stats::qnorm(1 - p/2)
      return(data.frame(
        id = id_val, n = n_i, S_xx = NA_real_,
        slope_hat = NA_real_, sd_single_used = NA_real_,
        sigma_e = NA_real_, SE_reliability = NA_real_, RTI = NA_real_,
        crit = crit, RTI_cat = NA_character_
      ))
    }
    
    tbar <- mean(t_i)
    S_xx <- sum((t_i - tbar)^2)
    
    if (!is.finite(S_xx) || S_xx <= 0) {
      crit <- stats::qnorm(1 - p/2)
      return(data.frame(
        id = id_val, n = n_i, S_xx = S_xx,
        slope_hat = NA_real_, sd_single_used = NA_real_,
        sigma_e = NA_real_, SE_reliability = NA_real_, RTI = NA_real_,
        crit = crit, RTI_cat = NA_character_
      ))
    }
    
    # OLS slope
    fit <- stats::lm(y_i ~ t_i)
    slope_hat <- unname(stats::coef(fit)[2L])
    
    # Single-occasion SD and reliability plug-in
    sd_use <- sd_get(id_val, y_i, t_i)
    if (!is.finite(sd_use) || sd_use <= 0) {
      crit <- stats::qnorm(1 - p/2)
      return(data.frame(
        id = id_val, n = n_i, S_xx = S_xx,
        slope_hat = slope_hat, sd_single_used = sd_use,
        sigma_e = NA_real_, SE_reliability = NA_real_, RTI = NA_real_,
        crit = crit, RTI_cat = NA_character_
      ))
    }
    
    sigma_e <- sd_use * sqrt(1 - r)
    if (!is.finite(sigma_e) || sigma_e < 0) sigma_e <- NA_real_
    
    SE_rel <- if (is.na(sigma_e)) NA_real_ else sigma_e / sqrt(S_xx)
    RTI    <- if (is.na(SE_rel) || SE_rel == 0) NA_real_ else slope_hat / SE_rel
    
    # z critical (used for everyone)
    crit <- stats::qnorm(1 - p/2)
    
    # Category
    RTI_cat <- if (is.na(RTI)) {
      NA_character_
    } else if (RTI >=  crit) {
      "Reliable increase"
    } else if (RTI <= -crit) {
      "Reliable decrease"
    } else {
      "No reliable trend"
    }
    
    data.frame(
      id = id_val,
      n = n_i,
      S_xx = S_xx,
      slope_hat = slope_hat,
      sd_single_used = sd_use,
      sigma_e = sigma_e,
      SE_reliability = SE_rel,
      RTI = RTI,
      crit = crit,
      RTI_cat = RTI_cat
    )
  })
  
  res <- do.call(rbind, out_list)
  
  if (isTRUE(verbose)) {
    res
  } else {
    res[, c("id", "n", "slope_hat", "RTI", "crit", "RTI_cat")]
  }
}
