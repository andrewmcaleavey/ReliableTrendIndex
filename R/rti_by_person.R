#' RTI for a single person (deprecated)
#'
#' `rti_by_person()` is **deprecated**; use [rti_by()] instead (e.g., filter to a
#' single person or run `rti_by()` on the whole data and slice that person's row).
#' This remains for backward compatibility and mirrors the legacy logic.
#'
#' @param data A `data.frame` with columns referenced by `id`, `y`, and (optionally) `time`.
#' @param id Bare column name identifying the person.
#' @param y Bare column name for the outcome/score.
#' @param time Optional bare column name for time; if missing, the within-person
#'   order `1..n` is used.
#' @param r Numeric scalar in \eqn{[0, 1]}: reliability used to derive measurement error.
#' @param sd_single Optional positive numeric scalar SD used for all persons. If `NULL`,
#'   `sd_method` determines the SD source.
#' @param sd_method Character scalar, one of `"global"`, `"per_person"`, or `"baseline"`;
#'   ignored if `sd_single` is provided.
#' @param baseline_time If `sd_method = "baseline"`, the time value treated as baseline
#'   per person. If `NULL`, uses the earliest time per person.
#' @param p Numeric scalar in \eqn{(0, 1)}; two-sided alpha (default `0.05`).
#' @param verbose Logical; if `TRUE`, return detailed columns (legacy format).
#' @param na_rm Logical; if `TRUE` (default) drops incomplete rows.
#'
#' @return If `verbose = FALSE` (default), a `data.frame` with columns
#'   `id`, `n`, `slope_hat`, `RTI`, `crit`, `RTI_cat`. If `verbose = TRUE`,
#'   returns additional diagnostics as in prior releases.
#'
#' @details
#' Per person, fits `y ~ time` via OLS and computes an RTI z-statistic using
#' `SE = (sd * sqrt(1 - r)) / sqrt(S_xx)`, where `sd` comes from `sd_single` or
#' `sd_method`, and `S_xx = sum((t - mean(t))^2)`.
#'
#' @seealso [rti_by()]
#' @keywords internal
#' @export
rti_by_person <- function(data, id, y, time = NULL, r,
                          sd_single = NULL,
                          sd_method = c("global", "per_person", "baseline"),
                          baseline_time = NULL,
                          p = 0.05,
                          verbose = FALSE,
                          na_rm = TRUE) {
  .Deprecated("rti_by",
              package = "ReliableTrendIndex",
              msg = "rti_by_person() is deprecated; use rti_by() (filter to one id or run by group).")

  stopifnot(is.data.frame(data))
  # NSE capture
  id_sym <- substitute(id)
  y_sym  <- substitute(y)

  id_name <- deparse(id_sym)
  y_name  <- deparse(y_sym)

  if (!id_name %in% names(data)) stop("`id` column not found in `data`.")
  if (!y_name %in% names(data))  stop("`y` column not found in `data`.")

  # Detect whether a usable time column was supplied
  has_time <- !(missing(time) || is.null(time))
  if (has_time) {
    time_sym  <- substitute(time)
    time_name <- deparse(time_sym)
    if (!time_name %in% names(data)) stop("`time` column not found in `data`.")
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
    id   = data[[id_name]],
    y    = data[[y_name]],
    time = if (has_time) data[[time_name]] else NA_real_
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
      ord <- order(t_i, method = "auto")
      t_i <- t_i[ord]
      y_i <- y_i[ord]
    } else {
      t_i <- seq_along(y_i)
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

    fit <- stats::lm(y_i ~ t_i)
    slope_hat <- unname(stats::coef(fit)[2L])

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

    crit <- stats::qnorm(1 - p/2)
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


# alternate version that uses newer functions: 
# ---- Adapter for older grouped helper names -----------------------------------

#' #' Legacy alias: rti_by_person()
#' #'
#' #' Older materials referred to a grouped helper named \code{rti_by_person()}.
#' #' This adapter calls [rti_by()]. If you supply a scalar \code{sem}, it will be
#' #' used for all ids (equivalent to \eqn{\sigma^2=\mathrm{SEM}^2}).
#' #'
#' #' @param data,id,time,y see [rti_by()]
#' #' @param sd,r per-id error parameters (scalars or columns)
#' #' @param sem optional scalar SEM (ignored if \code{sd}/\code{r} are supplied)
#' #' @param ... ignored
#' #' @export
#' rti_by_person <- function(data, id, time, y, sd = NULL, r = NULL, sem = NULL, ..., na.rm = FALSE, level = 0.95) {
#'   if (!is.null(sem) && (is.null(sd) || is.null(r))) {
#'     id_sym   <- substitute(id)
#'     time_sym <- substitute(time)
#'     y_sym    <- substitute(y)
#'     id_vec   <- .legacy_get_col(data, id_sym)
#'     time_vec <- .legacy_get_col(data, time_sym)
#'     y_vec    <- .legacy_get_col(data, y_sym)
#' 
#'     idx_list <- split(seq_len(nrow(data)), id_vec, drop = TRUE)
#'     rows <- lapply(names(idx_list), function(k) {
#'       idx <- idx_list[[k]]
#'       fit <- reliableTrend(values = y_vec[idx], time = time_vec[idx], sem = sem, na.rm = na.rm, level = level)
#'       df  <- rti_to_df(fit)
#'       df$id <- k
#'       df
#'     })
#'     out <- do.call(rbind, rows)
#'     out$fit <- lapply(seq_len(nrow(out)), function(i) {
#'       reliableTrend(values = y_vec[idx_list[[ out$id[i] ]]], time = time_vec[idx_list[[ out$id[i] ]]], sem = sem, na.rm = na.rm, level = level)
#'     })
#'     out <- out[, c("id","slope.est","slope.lb","slope.ub","z","p","n","Sxx","sigma2","category.RTI","fit")]
#'     rownames(out) <- NULL
#'     return(out)
#'   }
#'   rti_by(data, id = id, time = time, y = y, sd = sd, r = r, na.rm = na.rm, level = level)
#' }

#' Other stuff