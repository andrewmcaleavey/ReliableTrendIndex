#' RTI for grouped (panel) data
#'
#' Compute the Reliable Trend Index within groups (e.g., per person) from a
#' long-format data frame.
#'
#' @param data A data frame in long format.
#' @param id Grouping variable (column) identifying units (e.g., person id).
#' @param time Time/index variable (column). Can be equally or irregularly spaced.
#' @param y Outcome variable (column).
#' @param sd Either a single positive numeric (applied to all groups) or the
#'   name of a column in \code{data} providing per-row \code{sd}. If a column is
#'   supplied, it must be constant within each \code{id}.
#' @param r Either a single numeric in [0,1] (applied to all groups) or the
#'   name of a column in \code{data} providing per-row \code{r}. If a column is
#'   supplied, it must be constant within each \code{id}.
#' @param na.rm Logical. Drop non-finite \code{(y, time)} pairs within groups?
#' @param level Confidence level for slope intervals.
#'
#' @return A data frame with one row per \code{id}, containing summary fields
#'   and a list column \code{fit} holding each group's \code{reliableTrend} object.
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   id = rep(letters[1:3], each = 6),
#'   time = rep(1:6, times = 3),
#'   y = c(5 + 0.8*(1:6) + rnorm(6, 0, 3),
#'         10 - 0.5*(1:6) + rnorm(6, 0, 3),
#'         2 + 0.0*(1:6) + rnorm(6, 0, 3))
#' )
#' out <- rti_by(df, id, time, y, sd = 8, r = 0.85)
#' out
#' @export
rti_by <- function(data, id, time, y, sd, r, na.rm = FALSE, level = 0.95) {
  if (!is.data.frame(data)) stop("`data` must be a data.frame.", call. = FALSE)
  
  id_vec   <- .col_from_data(data, substitute(id),  required = TRUE)
  time_vec <- .col_from_data(data, substitute(time), required = TRUE)
  y_vec    <- .col_from_data(data, substitute(y),    required = TRUE)
  
  sd_input <- .maybe_col_or_scalar(data, substitute(sd))
  r_input  <- .maybe_col_or_scalar(data, substitute(r))
  
  # Split indices by id
  idx_list <- split(seq_len(nrow(data)), f = id_vec, drop = TRUE)
  ids <- names(idx_list)
  
  res_rows <- vector("list", length(idx_list))
  for (i in seq_along(idx_list)) {
    idx <- idx_list[[i]]
    
    y_i <- y_vec[idx]
    t_i <- time_vec[idx]
    
    # Determine sd and r for this group
    sd_i <- if (is.list(sd_input) && identical(sd_input$type, "column")) {
      vals <- sd_input$values[idx]
      u <- unique(vals[is.finite(vals)])
      if (length(u) != 1L) {
        stop("`sd` must be constant within id '", ids[i], "' when provided as a column.", call. = FALSE)
      }
      u[[1L]]
    } else {
      sd_input$value
    }
    
    r_i <- if (is.list(r_input) && identical(r_input$type, "column")) {
      vals <- r_input$values[idx]
      u <- unique(vals[is.finite(vals)])
      if (length(u) != 1L) {
        stop("`r` must be constant within id '", ids[i], "' when provided as a column.", call. = FALSE)
      }
      u[[1L]]
    } else {
      r_input$value
    }
    
    fit_i <- rti(y = y_i, sd = sd_i, r = r_i, t = t_i, na.rm = na.rm, level = level)
    
    res_rows[[i]] <- data.frame(
      id = ids[i],
      n = fit_i$n,
      estimate = fit_i$estimate,
      se = fit_i$se,
      z = fit_i$z,
      p = fit_i$p,
      ci_lower = fit_i$ci[1L],
      ci_upper = fit_i$ci[2L],
      intercept = fit_i$intercept,
      Sxx = fit_i$Sxx,
      sigma2 = fit_i$sigma2,
      level = fit_i$level,
      stringsAsFactors = FALSE
    )
    # attach the fit as a list column
    res_rows[[i]]$fit <- list(fit_i)
  }
  
  # rbind + keep list column
  out <- do.call(rbind, lapply(res_rows, function(x) {
    x$fit <- I(x$fit)
    x
  }))
  rownames(out) <- NULL
  out
}

# ---- internal helpers (no export) ----

# Accept bare column names or single strings; return the column vector.
.col_from_data <- function(data, expr, required = FALSE) {
  if (missing(expr)) {
    if (required) stop("Missing required column argument.", call. = FALSE)
    return(NULL)
  }
  # `expr` arrives already captured (e.g., symbol id -> "id", or "id")
  if (is.symbol(expr)) {
    nm <- as.character(expr)
    if (!nm %in% names(data)) stop("Column `", nm, "` not found in `data`.", call. = FALSE)
    return(data[[nm]])
  }
  if (is.character(expr) && length(expr) == 1L) {
    nm <- expr
    if (!nm %in% names(data)) stop("Column `", nm, "` not found in `data`.", call. = FALSE)
    return(data[[nm]])
  }
  stop("Provide a column name (bare or string).", call. = FALSE)
}

# For sd/r: accept a scalar, a bare symbol bound to a scalar, or a column name.
.maybe_col_or_scalar <- function(data, expr) {
  if (is.symbol(expr)) {
    nm <- as.character(expr)
    # 1) Column path
    if (nm %in% names(data)) {
      return(list(type = "column", values = data[[nm]]))
    }
    # 2) Scalar-from-environment path (do NOT evaluate arbitrary expressions)
    val <- tryCatch(get(nm, envir = parent.frame()), error = function(e) NULL)
    if (is.numeric(val) && length(val) == 1L && is.finite(val)) {
      return(list(type = "scalar", value = as.numeric(val)))
    }
    stop("Column or scalar expected; `", nm, "` not found in `data`.", call. = FALSE)
  }
  
  if (is.character(expr) && length(expr) == 1L) {
    nm <- expr
    if (nm %in% names(data)) return(list(type = "column", values = data[[nm]]))
    stop("Column `", nm, "` not found in `data`.", call. = FALSE)
  }
  
  if (is.numeric(expr) && length(expr) == 1L && is.finite(expr)) {
    return(list(type = "scalar", value = as.numeric(expr)))
  }
  
  stop("`sd`/`r` must be a single number or a column name (bare or string).", call. = FALSE)
}

