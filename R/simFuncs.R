#####
# RCI SIM FUNCTIONS


#' Generate 2 timepoint data for RCI analysis
#'
#' @param n_sims Number of simulations
#' @param n_ppl Number of people to simulate
#' @param sd_tot Standard deviation of the true scores at baseline
#' @param m_bl_tot Mean of the true scores at baseline
#' @param delta Mean change, applied to all individuals uniformly.
#' @param delta_sd SD of change. Each person gets a value from this SD between time 0 and time 1. 
#' @param meas_err Measurement error. SEm. SD of the error component at each time point. 
#'
#' @return A tibble. Side effects: creates variables in the parent environment for RCI and Sdiff. 
#' @export
#'
#' @examples 
#' generate_data()
generate_data <- function(n_sims = 1, 
                          n_ppl = 5000, 
                          sd_tot = 1, 
                          m_bl_tot = 0, 
                          delta = -0.5, 
                          delta_sd = 1, 
                          meas_err = 0.5){
  
  t0_true <- rnorm(n    = n_ppl, 
                   mean = m_bl_tot, 
                   sd   = sd_tot)
  t1_true <- t0_true + rnorm(n    = n_ppl, 
                             mean = delta, 
                             sd   = delta_sd)
  t0_obs <- t0_true + rnorm(n    = n_ppl, 
                            mean = 0, 
                            sd   = meas_err)
  t1_obs <- t1_true + rnorm(n    = n_ppl, 
                            mean = 0, 
                            sd   = meas_err)
  sim_dat1 <- dplyr::bind_cols(id = rep(seq(1:n_ppl), each = 2), 
                        time    = rep(c(0, 1), n_ppl))
  
  t0 <- tibble::tibble(id = 1:n_ppl, 
               time = 0,
               obs = t0_obs, 
               tru = t0_true)
  t1 <- tibble::tibble(id = 1:n_ppl, 
               time = 1, 
               obs = t1_obs, 
               tru = t1_true)
  
  simdat2 <- dplyr::bind_rows(t0, t1) |> 
    dplyr::arrange(id, time) |> 
    dplyr::group_by(id) |> 
    dplyr::mutate(obs_diff = obs - dplyr::first(obs), 
           true_diff = tru - dplyr::first(tru)) |> 
    dplyr::ungroup()
  
  # RCI
  SEm <- sd(simdat2 |> 
              dplyr::filter(time == 0) |> 
              dplyr::pull(obs)) * sqrt(1 - (sd_tot^2 / (sd_tot^2 + meas_err^2)))
  
  Sdiff <<- sqrt(2) * SEm
  
  RCI <<- 1.96 * Sdiff
  
  simdat3 <- simdat2 |> 
    dplyr::group_by(id) |> 
    dplyr::mutate(ReliableChange = ifelse(dplyr::last(obs_diff) > RCI, 
                                   "RelDet", 
                                   ifelse(dplyr::last(obs_diff) < -1*RCI, 
                                          "RelImp", 
                                          "NoRel")), 
           TrueChange = ifelse(dplyr::last(true_diff) > 0, 
                               "TrueDet", 
                               ifelse(dplyr::last(true_diff) < 0, 
                                      "TrueImp", 
                                      "TrueNo")), 
           obsChange = ifelse(dplyr::last(obs_diff) > 0, 
                              "ObsDet", 
                              ifelse(dplyr::last(obs_diff) < 0, 
                                     "ObsImp", 
                                     "ObsNo"))) |> 
    dplyr::ungroup() |> 
    dplyr::mutate(wrongRCI = dplyr::case_when(ReliableChange == "RelDet" & TrueChange == "TrueDet" ~ 0, 
                                ReliableChange == "RelImp" & TrueChange == "TrueImp" ~ 0, 
                                ReliableChange == "NoRel"  & TrueChange == "TrueNo" ~ 0,
                                TRUE ~ 1), 
           trueRCI = dplyr::case_when(true_diff < -RCI ~ "TrueRCIImp", 
                               true_diff > RCI  ~ "TrueRCIDet", 
                               TRUE ~ "TrueNotSure"), 
           borderMisclassified = dplyr::case_when(ReliableChange == "RelDet" & trueRCI == "TrueNotSure" ~ 1, 
                                           ReliableChange == "NoRel" & trueRCI == "TrueRCIDet" ~ 1,
                                           ReliableChange == "NoRel" & trueRCI == "TrueRCIImp" ~ 1,
                                           ReliableChange == "RelImp" & trueRCI == "TrueNotSure" ~ 1, 
                                           TRUE ~ 0), 
           delta_err = obs_diff - true_diff) |> 
    dplyr::mutate(obsCorrect = dplyr::case_when(true_diff > 0 & obs_diff > 0 ~ TRUE, 
                                  true_diff < 0 & obs_diff < 0 ~ TRUE, 
                                  TRUE ~ FALSE), 
           RCICorrect = dplyr::case_when(wrongRCI == 1 ~ FALSE, 
                                  TRUE ~ TRUE)) |> 
    dplyr::mutate(nullTile = pnorm(q = obs_diff,  # observed change score
                            mean = 0,  # assume no change
                            sd = Sdiff), # assume SD is Sdiff, which is the measurement-error related distribution
           delta = delta)  
  return(simdat3)
}


#' Generate X timepoint data for RCI analysis
#'
#' @param n_sims Number of simulations
#' @param n_ppl Number of people to simulate
#' @param sd_tot Standard deviation of the true scores at baseline
#' @param m_bl_tot Mean of the true scores at baseline
#' @param delta Mean change per unit time, applied to all individuals uniformly.
#' @param delta_sd SD of change per unit time. Each person gets a value from this SD between time 0 and time 1. 
#' @param meas_err Measurement error. SEm. SD of the error component at each time point. 
#' @param n_obs Number of observations per person
#'
#' @return A tibble. Side effects: creates variables in the parent environment for RCI and Sdiff. 
#' @export
#'
#' @examples 
#' generate_xt_data(n_obs = 10)
generate_xt_data <- function(n_sims = 1, 
                             n_ppl = 500, 
                             sd_tot = 1, 
                             m_bl_tot = 0, 
                             delta = -0.5, 
                             delta_sd = 1, 
                             meas_err = 0.5, 
                             n_obs){
  
  t0_true <- rnorm(n    = n_ppl, 
                   mean = m_bl_tot, 
                   sd   = sd_tot)
  slope_true <- rnorm(n = n_ppl, 
                      m = delta, 
                      sd = delta_sd)
  t1_true <- t0_true + slope_true
  
  data_1 <- tibble::tibble(id = rep(1:n_ppl, each = n_obs), 
                   time = rep(seq(0, 1, by = 1/(n_obs-1)), n_ppl),
                   true_t0 = rep(t0_true, each = n_obs),
                   true_slope = rep(slope_true, each = n_obs)) |> 
    dplyr::mutate(true_value = true_t0 + (true_slope * time), 
           obs = true_value + rnorm(n_obs * n_ppl, 
                                    mean = 0, 
                                    sd = meas_err)) |> 
    dplyr::group_by(id) |> 
    dplyr::mutate(obs_diff = obs - dplyr::first(obs), 
           true_diff = true_value - dplyr::first(true_t0)) |> 
    dplyr::ungroup()
  
  # RCI
  SEm <- sd(data_1 |> 
              dplyr::filter(time == 0) |> 
              dplyr::pull(obs)) * sqrt(1 - (sd_tot^2 / (sd_tot^2 + meas_err^2)))
  
  Sdiff <<- sqrt(2) * SEm
  
  RCI <<- 1.96 * Sdiff
  
  data_2 <- data_1 |> 
    dplyr::group_by(id) |> 
    dplyr::mutate(ReliableChange = ifelse(dplyr::last(obs_diff) > RCI, 
                                   "RelDet", 
                                   ifelse(dplyr::last(obs_diff) < -1*RCI, 
                                          "RelImp", 
                                          "NoRel")), 
           TrueChange = ifelse(dplyr::last(true_diff) > 0, 
                               "TrueDet", 
                               ifelse(dplyr::last(true_diff) < 0, 
                                      "TrueImp", 
                                      "TrueNo")), 
           obsChange = ifelse(dplyr::last(obs_diff) > 0, 
                              "ObsDet", 
                              ifelse(dplyr::last(obs_diff) < 0, 
                                     "ObsImp", 
                                     "ObsNo"))) |> 
    dplyr::ungroup() |> 
    dplyr::mutate(wrongRCI = dplyr::case_when(ReliableChange == "RelDet" & TrueChange == "TrueDet" ~ 0, 
                                ReliableChange == "RelImp" & TrueChange == "TrueImp" ~ 0, 
                                ReliableChange == "NoRel"  & TrueChange == "TrueNo" ~ 0,
                                TRUE ~ 1), 
           trueRCI = dplyr::case_when(true_diff < -RCI ~ "TrueRCIImp", 
                               true_diff > RCI  ~ "TrueRCIDet", 
                               TRUE ~ "TrueNotSure"), 
           borderMisclassified = dplyr::case_when(ReliableChange == "RelDet" & trueRCI == "TrueNotSure" ~ 1, 
                                           ReliableChange == "NoRel" & trueRCI == "TrueRCIDet" ~ 1,
                                           ReliableChange == "NoRel" & trueRCI == "TrueRCIImp" ~ 1,
                                           ReliableChange == "RelImp" & trueRCI == "TrueNotSure" ~ 1, 
                                           TRUE ~ 0), 
           delta_err = obs_diff - true_diff) |> 
    dplyr::group_by(id) |> 
    dplyr::mutate(obsCorrect = dplyr::case_when(true_slope > 0 & dplyr::last(obs_diff) > 0 ~ TRUE, 
                                  true_slope < 0 & dplyr::last(obs_diff) < 0 ~ TRUE, 
                                  TRUE ~ FALSE), 
           RCICorrect = dplyr::case_when(wrongRCI == 1 ~ FALSE, 
                                  TRUE ~ TRUE)) |> 
    dplyr::mutate(nullTile = pnorm(q = obs_diff,  # observed change score
                            mean = 0,  # assume no change
                            sd = Sdiff), # assume SD is Sdiff, which is the measurement-error related distribution
           delta = delta)  |> 
    dplyr::ungroup()
  return(data_2)
}

RCIfunc <- function(rxx, s1 = 1, cut = 1.96){
  cut * sqrt(2 * (s1 * sqrt(1 - rxx))^2)
}
#' Simple plot
#'
#' @param x data.frame
#' @return ggplot object
#' @export
#' @examples
#' plot_raw(generate_data())
plot_raw <- function(x){
  x |>
    ggplot2::ggplot(ggplot2::aes(x = time, y = obs)) +
    ggplot2::geom_line(ggplot2::aes(group = id), alpha = 0.02) +
    ggplot2::geom_point(size = 3, alpha = 0.02, shape = 21) +
    ggplot2::theme_bw() +
    ggplot2::geom_smooth(method = "lm") +
    ggplot2::labs(
      title = "Simulated data",
      y = "Observed score (higher is worse)",
      caption = "There is some range of starting places and outcomes"
    )
}

#' Simple raw histogram of observed difference scores
#'
#' @param x data containing columns labeled `time` and `obs_diff`.
#' @return ggplot object
#' @export
#' @examples
#' hist_obs_diff_raw(generate_data())
hist_obs_diff_raw <- function(x){
  x |>
    dplyr::filter(time == 1) |>
    ggplot2::ggplot(ggplot2::aes(x = obs_diff)) +
    ggplot2::geom_histogram(binwidth = 0.2) +
    ggplot2::theme_bw() +
    ggplot2::labs(
      title = "Simulated data",
      subtitle = "Observed difference scores"
    )
}

#' Compute reliability from known true and observed scores
#'
#' @param x data
#' @return empirical reliability from a data set for simulation
#' @export
#' @examples
#' rxx_empirical(generate_data())
rxx_empirical <- function(x){
  stats::sd(
    x |>
      dplyr::filter(time == 0) |>
      dplyr::pull(tru)
  )^2 /
    stats::sd(
      x |>
        dplyr::filter(time == 0) |>
        dplyr::pull(obs)
    )^2
}

#' Line plot with RCI groups
#'
#' @param x data from simulation
#' @return ggplot object
#' @export
#' @examples
#' plot_rci_grps(generate_data())
plot_rci_grps <- function(x){
  x |>
    ggplot2::ggplot(
      ggplot2::aes(x = time, y = obs, group = id, color = ReliableChange)
    ) +
    ggplot2::geom_line(ggplot2::aes(color = ReliableChange), alpha = 0.02) +
    ggplot2::geom_point(ggplot2::aes(color = ReliableChange), size = 3, alpha = 0.04) +
    ggplot2::theme_bw() +
    ggplot2::geom_smooth(
      ggplot2::aes(group = ReliableChange, fill = ReliableChange),
      method = "lm"
    ) +
    ggplot2::labs(
      title = "Simulated data with RCI ",
      y = "Observed score (higher is worse)",
      caption = "Colors show RCI categories. Bands represent 95%CI for these groups."
    )
}

#' Generate a simple table of ReliableChange
#'
#' @param x data
#' @param divideByTwo logical. Should the sample be divided by two
#'   (e.g., if there are two lines per person)? Defaults to TRUE.
#' @return A table.
#' @export
#' @examples
#' relChangeRaw_table(generate_data())
#' relChangeRaw_table(generate_data(), divideByTwo = FALSE)
relChangeRaw_table <- function(x, divideByTwo = TRUE){
  if (divideByTwo) return(base::table(x$ReliableChange) / 2)
  else return(base::table(x$ReliableChange))
}

#' Make a ReliableChange table by percentages
#'
#' @param x data
#' @return A table
#' @export
#' @examples
#' relChangePct_table(generate_data())
relChangePct_table <- function(x){
  100 * relChangeRaw_table(x) / base::nrow(x)
}

# ---- metrics helpers ----

counting_func <- function(x){
  x_original <- x
  x <- dplyr::filter(x, time == 1)
  
  NumRelImp <-  base::sum(x$ReliableChange == "RelImp")
  NumRelDet <-  base::sum(x$ReliableChange == "RelDet")
  NumNoRel  <-  base::sum(x$ReliableChange == "NoRel")
  PctRelImp <-  base::sum(x$ReliableChange == "RelImp") / base::nrow(x)
  PctRelDet <-  base::sum(x$ReliableChange == "RelDet") / base::nrow(x)
  PctNoRel  <-  base::sum(x$ReliableChange == "NoRel")  / base::nrow(x)
  
  # observed
  NumObsImp <-  base::sum(x$obsChange == "ObsImp")
  NumObsDet <-  base::sum(x$obsChange == "ObsDet")
  NumObsNo  <-  base::sum(x$obsChange == "ObsNo")
  PctObsImp <-  base::sum(x$obsChange == "ObsImp") / base::nrow(x)
  PctObsDet <-  base::sum(x$obsChange == "ObsDet") / base::nrow(x)
  PctObsNo  <-  base::sum(x$obsChange == "ObsNo")  / base::nrow(x)
  
  # true
  NumTrueImp <- base::sum(x$TrueChange == "TrueImp")
  NumTrueDet <- base::sum(x$TrueChange == "TrueDet")
  NumTrueNo  <- base::sum(x$TrueChange == "TrueNo")
  PctTrueImp <- base::sum(x$TrueChange == "TrueImp") / base::nrow(x)
  PctTrueDet <- base::sum(x$TrueChange == "TrueDet") / base::nrow(x)
  PctTrueNo  <- base::sum(x$TrueChange == "TrueNo")  / base::nrow(x)
  
  # total accuracy
  RCITotalAcc <-  base::sum(x$RCICorrect) / base::nrow(x)
  ObsTotalAcc <-  base::sum(x$obsCorrect) / base::nrow(x)
  
  # RCI accuracy when it provides a decision
  RCIAccIfSig <- (
    x |>
      dplyr::filter(ReliableChange != "NoRel") |>
      dplyr::summarise(numcorrect = base::sum(RCICorrect), .groups = "drop") |>
      dplyr::pull(numcorrect)
  ) / (
    x |>
      dplyr::filter(ReliableChange != "NoRel") |>
      base::nrow()
  )
  
  # mean accuracy of the difference score in the RCI-nochange group
  ObsAccInNoRel <- (
    x |>
      dplyr::filter(ReliableChange == "NoRel") |>
      dplyr::summarise(n_correct = base::sum(obsCorrect), .groups = "drop") |>
      dplyr::pull(n_correct)
  ) / (
    x |>
      dplyr::filter(ReliableChange == "NoRel") |>
      base::nrow()
  )
  
  # Sensitivity and Specificity (wrap denominators)
  SensDetRCI <- (
    x |> dplyr::filter(TrueChange == "TrueDet") |> dplyr::summarise(base::sum(RCICorrect), .groups = "drop") |> dplyr::pull()
  ) / (
    x |> dplyr::filter(TrueChange == "TrueDet") |> base::nrow()
  )
  SensImpRCI <- (
    x |> dplyr::filter(TrueChange == "TrueImp") |> dplyr::summarise(base::sum(RCICorrect), .groups = "drop") |> dplyr::pull()
  ) / (
    x |> dplyr::filter(TrueChange == "TrueImp") |> base::nrow()
  )
  
  SensDetObs <- (
    x |> dplyr::filter(TrueChange == "TrueDet") |> dplyr::summarise(base::sum(obsCorrect), .groups = "drop") |> dplyr::pull()
  ) / (
    x |> dplyr::filter(TrueChange == "TrueDet") |> base::nrow()
  )
  SensImpObs <- (
    x |> dplyr::filter(TrueChange == "TrueImp") |> dplyr::summarise(base::sum(obsCorrect), .groups = "drop") |> dplyr::pull()
  ) / (
    x |> dplyr::filter(TrueChange == "TrueImp") |> base::nrow()
  )
  
  SpecDetRCI <- (
    x |> dplyr::filter(TrueChange != "TrueDet" & ReliableChange != "RelDet") |> base::nrow()
  ) / (
    x |> dplyr::filter(TrueChange != "TrueDet") |> base::nrow()
  )
  SpecImpRCI <- (
    x |> dplyr::filter(TrueChange != "TrueImp" & ReliableChange != "RelImp") |> base::nrow()
  ) / (
    x |> dplyr::filter(TrueChange != "TrueImp") |> base::nrow()
  )
  
  SpecDetObs <- (
    x |> dplyr::filter(TrueChange != "TrueDet" & obsChange != "ObsDet") |> base::nrow()
  ) / (
    x |> dplyr::filter(TrueChange != "TrueDet") |> base::nrow()
  )
  SpecImpObs <- (
    x |> dplyr::filter(TrueChange != "TrueImp" & obsChange != "ObsImp") |> base::nrow()
  ) / (
    x |> dplyr::filter(TrueChange != "TrueImp") |> base::nrow()
  )
  
  # PPP and NPP
  PPPDetRCI <- (
    x |> dplyr::filter(TrueChange == "TrueDet" & ReliableChange == "RelDet") |> base::nrow()
  ) / base::sum(x$ReliableChange == "RelDet")
  PPPImpRCI <- (
    x |> dplyr::filter(TrueChange == "TrueImp" & ReliableChange == "RelImp") |> base::nrow()
  ) / base::sum(x$ReliableChange == "RelImp")
  PPPDetObs <- (
    x |> dplyr::filter(TrueChange == "TrueDet" & obsChange == "ObsDet") |> base::nrow()
  ) / base::sum(x$obsChange == "ObsDet")
  PPPImpObs <- (
    x |> dplyr::filter(TrueChange == "TrueImp" & obsChange == "ObsImp") |> base::nrow()
  ) / base::sum(x$obsChange == "ObsImp")
  
  NPPDetRCI <- (
    x |> dplyr::filter(TrueChange != "TrueDet" & ReliableChange != "RelDet") |> base::nrow()
  ) / (
    x |> dplyr::filter(ReliableChange != "RelDet") |> base::nrow()
  )
  NPPImpRCI <- (
    x |> dplyr::filter(TrueChange != "TrueImp" & ReliableChange != "RelImp") |> base::nrow()
  ) / (
    x |> dplyr::filter(ReliableChange != "RelImp") |> base::nrow()
  )
  NPPDetObs <- (
    x |> dplyr::filter(TrueChange != "TrueDet" & obsChange != "ObsDet") |> base::nrow()
  ) / (
    x |> dplyr::filter(obsChange != "ObsDet") |> base::nrow()
  )
  NPPImpObs <- (
    x |> dplyr::filter(TrueChange != "TrueImp" & obsChange != "ObsImp") |> base::nrow()
  ) / (
    x |> dplyr::filter(obsChange != "ObsImp") |> base::nrow()
  )
  
  # Type S errors
  TypeSRCI <- (
    x |> dplyr::filter(!RCICorrect & ReliableChange != "NoRel") |> base::nrow()
  ) / (
    x |> dplyr::filter(ReliableChange != "NoRel") |> base::nrow()
  )
  TypeSRCIDet <- (
    x |> dplyr::filter(!RCICorrect & ReliableChange == "RelDet") |> base::nrow()
  ) / (
    x |> dplyr::filter(ReliableChange == "RelDet") |> base::nrow()
  )
  TypeSRCIImp <- (
    x |> dplyr::filter(!RCICorrect & ReliableChange == "RelImp") |> base::nrow()
  ) / (
    x |> dplyr::filter(ReliableChange == "RelImp") |> base::nrow()
  )
  
  TypeSObs <- (
    x |> dplyr::filter(!obsCorrect & obsChange != "ObsNo") |> base::nrow()
  ) / (
    x |> dplyr::filter(obsChange != "ObsNo") |> base::nrow()
  )
  TypeSObsDet <- (
    x |> dplyr::filter(!obsCorrect & obsChange == "ObsDet") |> base::nrow()
  ) / (
    x |> dplyr::filter(obsChange == "ObsDet") |> base::nrow()
  )
  TypeSObsImp <- (
    x |> dplyr::filter(!obsCorrect & obsChange == "ObsImp") |> base::nrow()
  ) / (
    x |> dplyr::filter(obsChange == "ObsImp") |> base::nrow()
  )
  
  # MAE and Bias
  MAEdiff <- base::mean(base::abs(x$delta_err))
  AbsErrBiasDiff <- base::mean(x$delta_err)
  
  MAEdiffRCI <- x |>
    dplyr::group_by(ReliableChange) |>
    dplyr::summarise(MAEDiff = base::mean(base::abs(delta_err)), .groups = "drop")
  AbsErrBiasDiffRCI <- x |>
    dplyr::group_by(ReliableChange) |>
    dplyr::summarise(AbsBias = base::mean(delta_err), .groups = "drop")
  
  MAEdiffObs <- x |>
    dplyr::group_by(obsChange) |>
    dplyr::summarise(MAEDiff = base::mean(base::abs(delta_err)), .groups = "drop")
  AbsErrBiasDiffObs <- x |>
    dplyr::group_by(obsChange) |>
    dplyr::summarise(AbsBias = base::mean(delta_err), .groups = "drop")
  
  # classification tables
  trueVsObs <- base::table(x$TrueChange, x$obsChange)
  trueVsRCI <- base::table(x$TrueChange, x$ReliableChange)
  
  # reliability
  rxx <- rxx_empirical(x_original)
  RCI <- RCIfunc(
    rxx = rxx,
    s1 = stats::sd(
      x_original |>
        dplyr::filter(time == 0) |>
        dplyr::pull(obs)
    )
  )
  
  PctClassifiedRCI <- (
    x |> dplyr::filter(ReliableChange != "NoRel") |> base::nrow()
  ) / base::nrow(x)
  
  # call info
  delta <- x$delta[1]
  
  base::return(base::mget(base::objects(), ifnotfound = NA))
}

# take the list output of counting_func() into a row tibble
into_output_tbl <- function(x){
  x$x <- NULL
  x$x_original <- NULL
  x$AbsErrBiasDiffObs <- NULL
  x$trueVsRCI <- NULL
  x$trueVsObs <- NULL
  x$MAEdiffRCI <- NULL
  x$MAEdiffObs <- NULL
  x$AbsErrBiasDiffRCI <- NULL
  x$AbsErrBiasDiffObs <- NULL
  tibble::as_tibble_row(x, .name_repair = "unique")
}

# ---- plotting over rxx ----

plotting_func <- function(comparison.data){
  # Total Accuracy
  plotAcc <- ggplot2::ggplot(comparison.data,
                             ggplot2::aes(x = rxx, y = ObsTotalAcc)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(y = RCITotalAcc), linetype = "dashed", color = "gray40") +
    ggplot2::geom_point(ggplot2::aes(y = RCITotalAcc), color = "gray40") +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Total accuracy", x = expression(r[xx]), y = "Total accuracy")
  
  # Type S
  plotTypeS <- ggplot2::ggplot(comparison.data,
                               ggplot2::aes(x = rxx, y = TypeSObs)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(y = TypeSRCI), linetype = "dashed", color = "gray40") +
    ggplot2::geom_point(ggplot2::aes(y = TypeSRCI), color = "gray40") +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Type S Error", x = expression(r[xx]), y = "Type S Error")
  
  # RCI value
  plotRCI <- ggplot2::ggplot(comparison.data,
                             ggplot2::aes(x = rxx, y = RCI)) +
    ggplot2::geom_line(linetype = "dashed", color = "gray40") +
    ggplot2::geom_point() +
    ggplot2::theme_bw() +
    ggplot2::lims(y = c(0, NA)) +
    ggplot2::labs(title = "RCI value", x = expression(r[xx]), y = "RCI value")
  
  # percent classified
  plotClassifiedRCI <- ggplot2::ggplot(comparison.data,
                                       ggplot2::aes(x = rxx, y = PctClassifiedRCI)) +
    ggplot2::geom_line(linetype = "dashed", color = "gray40") +
    ggplot2::geom_point() +
    ggplot2::theme_bw() +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::labs(title = "Percent \"Reliably\" Changed",
                  x = expression(r[xx]),
                  y = "Percent Classified as Reliably Changed")
  
  # group-level accuracy (Det)
  plotGrpAccDet <- comparison.data |>
    dplyr::select(rxx, PctTrueDet, PctRelDet, PctObsDet) |>
    tidyr::pivot_longer(cols = -rxx, values_to = "Proportion", names_to = "Metric") |>
    ggplot2::ggplot(ggplot2::aes(x = rxx, y = Proportion, linetype = Metric, shape = Metric)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_linetype_discrete(labels = c("Observed Sign", "RCI", "True")) +
    ggplot2::scale_shape_discrete(labels = c("Observed Sign", "RCI", "True")) +
    ggplot2::labs(title = "Deterioration: Group Accuracy", y = "Portion with deterioration", x = expression(r[xx]))
  
  # group-level accuracy (Imp)
  plotGrpAccImp <- comparison.data |>
    dplyr::select(rxx, PctTrueImp, PctRelImp, PctObsImp) |>
    tidyr::pivot_longer(cols = -rxx, values_to = "Proportion", names_to = "Metric") |>
    ggplot2::ggplot(ggplot2::aes(x = rxx, y = Proportion, linetype = Metric, shape = Metric)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_linetype_discrete(labels = c("Observed Sign", "RCI", "True")) +
    ggplot2::scale_shape_discrete(labels = c("Observed Sign", "RCI", "True")) +
    ggplot2::labs(title = "Improvement: Group Accuracy", y = "Portion with improvement", x = expression(r[xx]))
  
  # Sensitivity
  plotSensDet <- ggplot2::ggplot(comparison.data,
                                 ggplot2::aes(x = rxx, y = SensDetObs)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(y = SensDetRCI), linetype = "dashed", color = "gray40") +
    ggplot2::geom_point(ggplot2::aes(y = SensDetRCI), color = "gray40") +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Sensitivity to Deterioration", x = expression(r[xx]), y = "Sensitivity")
  
  plotSensImp <- ggplot2::ggplot(comparison.data,
                                 ggplot2::aes(x = rxx, y = SensImpObs)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(y = SensImpRCI), linetype = "dashed", color = "gray40") +
    ggplot2::geom_point(ggplot2::aes(y = SensImpRCI), color = "gray40") +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Sensitivity to Improvement", x = expression(r[xx]), y = "Sensitivity")
  
  # Specificity
  plotSpecDet <- ggplot2::ggplot(comparison.data,
                                 ggplot2::aes(x = rxx, y = SpecDetObs)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(y = SpecDetRCI), linetype = "dashed", color = "gray40") +
    ggplot2::geom_point(ggplot2::aes(y = SpecDetRCI), color = "gray40") +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Specificity to Deterioration", x = expression(r[xx]), y = "Specificity")
  
  plotSpecImp <- ggplot2::ggplot(comparison.data,
                                 ggplot2::aes(x = rxx, y = SpecImpObs)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(y = SpecImpRCI), linetype = "dashed", color = "gray40") +
    ggplot2::geom_point(ggplot2::aes(y = SpecImpRCI), color = "gray40") +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Specificity to Improvement", x = expression(r[xx]), y = "Specificity")
  
  # PPP
  plotPPPDet <- ggplot2::ggplot(comparison.data,
                                ggplot2::aes(x = rxx, y = PPPDetObs)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(y = PPPDetRCI), linetype = "dashed", color = "gray40") +
    ggplot2::geom_point(ggplot2::aes(y = PPPDetRCI), color = "gray40") +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "PPP to Deterioration", x = expression(r[xx]), y = "PPP")
  
  plotPPPImp <- ggplot2::ggplot(comparison.data,
                                ggplot2::aes(x = rxx, y = PPPImpObs)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(y = PPPImpRCI), linetype = "dashed", color = "gray40") +
    ggplot2::geom_point(ggplot2::aes(y = PPPImpRCI), color = "gray40") +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "PPP to Improvement", x = expression(r[xx]), y = "PPP")
  
  # NPP
  plotNPPDet <- ggplot2::ggplot(comparison.data,
                                ggplot2::aes(x = rxx, y = NPPDetObs)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(y = NPPDetRCI), linetype = "dashed", color = "gray40") +
    ggplot2::geom_point(ggplot2::aes(y = NPPDetRCI), color = "gray40") +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "NPP to Deterioration", x = expression(r[xx]), y = "NPP")
  
  plotNPPImp <- ggplot2::ggplot(comparison.data,
                                ggplot2::aes(x = rxx, y = NPPImpObs)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(y = NPPImpRCI), linetype = "dashed", color = "gray40") +
    ggplot2::geom_point(ggplot2::aes(y = NPPImpRCI), color = "gray40") +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "NPP to Improvement", x = expression(r[xx]), y = "NPP")
  
  base::list(
    plotAcc = plotAcc,
    plotTypeS = plotTypeS,
    plotRCI = plotRCI,
    plotClassifiedRCI = plotClassifiedRCI,
    plotGrpAccDet = plotGrpAccDet,
    plotGrpAccImp = plotGrpAccImp,
    plotSensDet = plotSensDet,
    plotSensImp = plotSensImp,
    plotSpecDet = plotSpecDet,
    plotSpecImp = plotSpecImp,
    plotPPPDet = plotPPPDet,
    plotPPPImp = plotPPPImp,
    plotNPPDet = plotNPPDet, 
    plotNPPImp = plotNPPImp)
}
    

#' Plotting simulation results from effect size simulation
#'
#' @param comparison.data A data frame, likely generated from
#'   bind_rows(into_output_tbl()).
#'
#' @return a list of objects
#' @export
#'
#' @examples
#' \dontrun{
#' plotting_func_delta(generate_data())
#' }
plotting_func_delta <- function(comparison.data){
  # Total Accuracy
  plotAcc <- ggplot2::ggplot(comparison.data,
                             ggplot2::aes(x = delta, y = ObsTotalAcc)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(y = RCITotalAcc),
                       linetype = "dashed", color = "gray40") +
    ggplot2::geom_point(ggplot2::aes(y = RCITotalAcc), color = "gray40") +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Total accuracy", x = "Mean change", y = "Total accuracy")
  
  # Type S
  plotTypeS <- ggplot2::ggplot(comparison.data,
                               ggplot2::aes(x = delta, y = TypeSObs)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(y = TypeSRCI),
                       linetype = "dashed", color = "gray40") +
    ggplot2::geom_point(ggplot2::aes(y = TypeSRCI), color = "gray40") +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Type S Error", x = "Mean change", y = "Type S Error")
  
  ## RCI value
  plotRCI <- ggplot2::ggplot(comparison.data,
                             ggplot2::aes(x = delta, y = RCI)) +
    ggplot2::geom_line(linetype = "dashed", color = "gray40") +
    ggplot2::geom_point() +
    ggplot2::theme_bw() +
    ggplot2::lims(y = c(0, NA)) +
    ggplot2::labs(title = "RCI value", x = "Mean change", y = "RCI value")
  
  ## percent classified
  plotClassifiedRCI <- ggplot2::ggplot(comparison.data,
                                       ggplot2::aes(x = delta, y = PctClassifiedRCI)) +
    ggplot2::geom_line(linetype = "dashed", color = "gray40") +
    ggplot2::geom_point() +
    ggplot2::theme_bw() +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::labs(title = "Percent \"Reliably\" Changed",
                  x = "Mean change", y = "Percent Classified as Reliably Changed")
  
  # group-level accuracy (Det)
  plotGrpAccDet <- comparison.data |>
    dplyr::select(delta, PctTrueDet, PctRelDet, PctObsDet) |>
    tidyr::pivot_longer(cols = -delta, values_to = "Proportion", names_to = "Metric") |>
    ggplot2::ggplot(ggplot2::aes(x = delta, y = Proportion, linetype = Metric, shape = Metric)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_linetype_discrete(labels = c("Observed Sign", "RCI", "True")) +
    ggplot2::scale_shape_discrete(labels = c("Observed Sign", "RCI", "True")) +
    ggplot2::labs(title = "Deterioration: Group Accuracy",
                  y = "Portion with deterioration", x = "Mean change")
  
  # group-level accuracy (Imp)
  plotGrpAccImp <- comparison.data |>
    dplyr::select(delta, PctTrueImp, PctRelImp, PctObsImp) |>
    tidyr::pivot_longer(cols = -delta, values_to = "Proportion", names_to = "Metric") |>
    ggplot2::ggplot(ggplot2::aes(x = delta, y = Proportion, linetype = Metric, shape = Metric)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_linetype_discrete(labels = c("Observed Sign", "RCI", "True")) +
    ggplot2::scale_shape_discrete(labels = c("Observed Sign", "RCI", "True")) +
    ggplot2::labs(title = "Improvement: Group Accuracy",
                  y = "Portion with improvement", x = "Mean change")
  
  # Sensitivity
  plotSensDet <- ggplot2::ggplot(comparison.data,
                                 ggplot2::aes(x = delta, y = SensDetObs)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(y = SensDetRCI), linetype = "dashed", color = "gray40") +
    ggplot2::geom_point(ggplot2::aes(y = SensDetRCI), color = "gray40") +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Sensitivity to Deterioration", x = "Mean change", y = "Sensitivity")
  
  plotSensImp <- ggplot2::ggplot(comparison.data,
                                 ggplot2::aes(x = delta, y = SensImpObs)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(y = SensImpRCI), linetype = "dashed", color = "gray40") +
    ggplot2::geom_point(ggplot2::aes(y = SensImpRCI), color = "gray40") +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Sensitivity to Improvement", x = "Mean change", y = "Sensitivity")
  
  # Specificity
  plotSpecDet <- ggplot2::ggplot(comparison.data,
                                 ggplot2::aes(x = delta, y = SpecDetObs)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(y = SpecDetRCI), linetype = "dashed", color = "gray40") +
    ggplot2::geom_point(ggplot2::aes(y = SpecDetRCI), color = "gray40") +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Specificity to Deterioration", x = "Mean change", y = "Specificity")
  
  plotSpecImp <- ggplot2::ggplot(comparison.data,
                                 ggplot2::aes(x = delta, y = SpecImpObs)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(y = SpecImpRCI), linetype = "dashed", color = "gray40") +
    ggplot2::geom_point(ggplot2::aes(y = SpecImpRCI), color = "gray40") +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Specificity to Improvement", x = "Mean change", y = "Specificity")
  
  # PPP
  plotPPPDet <- ggplot2::ggplot(comparison.data,
                                ggplot2::aes(x = delta, y = PPPDetObs)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(y = PPPDetRCI), linetype = "dashed", color = "gray40") +
    ggplot2::geom_point(ggplot2::aes(y = PPPDetRCI), color = "gray40") +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "PPP to Deterioration", x = "Mean change", y = "PPP")
  
  plotPPPImp <- ggplot2::ggplot(comparison.data,
                                ggplot2::aes(x = delta, y = PPPImpObs)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(y = PPPImpRCI), linetype = "dashed", color = "gray40") +
    ggplot2::geom_point(ggplot2::aes(y = PPPImpRCI), color = "gray40") +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "PPP to Improvement", x = "Mean change", y = "PPP")
  
  # NPP
  plotNPPDet <- ggplot2::ggplot(comparison.data,
                                ggplot2::aes(x = delta, y = NPPDetObs)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(y = NPPDetRCI), linetype = "dashed", color = "gray40") +
    ggplot2::geom_point(ggplot2::aes(y = NPPDetRCI), color = "gray40") +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "NPP to Deterioration", x = "Mean change", y = "NPP")
  
  plotNPPImp <- ggplot2::ggplot(comparison.data,
                                ggplot2::aes(x = delta, y = NPPImpObs)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(y = NPPImpRCI), linetype = "dashed", color = "gray40") +
    ggplot2::geom_point(ggplot2::aes(y = NPPImpRCI), color = "gray40") +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "NPP to Improvement", x = "Mean change", y = "NPP")
  
  # Return only the plots in a named list
  list(
    plotAcc = plotAcc,
    plotTypeS = plotTypeS,
    plotRCI = plotRCI,
    plotClassifiedRCI = plotClassifiedRCI,
    plotGrpAccDet = plotGrpAccDet,
    plotGrpAccImp = plotGrpAccImp,
    plotSensDet = plotSensDet,
    plotSensImp = plotSensImp,
    plotSpecDet = plotSpecDet,
    plotSpecImp = plotSpecImp,
    plotPPPDet = plotPPPDet,
    plotPPPImp = plotPPPImp,
    plotNPPDet = plotNPPDet,
    plotNPPImp = plotNPPImp
  )
}


# this computes independent linear regressions (without prespecified measurement error) on a data set.
compute_lm <- function(x) {
  
  # fit per-id models once, reuse for p-values and slopes
  models <- x |>
    (\(d) split(d, d$id))() |>
    purrr::map(\(df) stats::lm(obs ~ time, data = df)) |>
    purrr::map(broom::tidy)
  
  pval.data <- models |>
    purrr::map_dfr("p.value") |>
    (\(df) df[2, , drop = FALSE])() |>
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "id",
      values_to = "lm.p.value"
    ) |>
    dplyr::mutate(
      lm.Rel = dplyr::case_when(lm.p.value < 0.05 ~ "lmRel", TRUE ~ "lmNoRel"),
      id = as.integer(id)
    )
  
  slp.data <- models |>
    purrr::map_dfr("estimate") |>
    (\(df) df[2, , drop = FALSE])() |>
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to = "id",
      values_to = "lm.est"
    ) |>
    dplyr::mutate(
      lmDir = dplyr::case_when(
        lm.est <  0 ~ "lmImp",
        lm.est >  0 ~ "lmDet",
        TRUE        ~ "lmNoChange"
      ),
      id = as.integer(id)
    )
  
  x |>
    dplyr::full_join(pval.data, by = "id") |>
    dplyr::full_join(slp.data,  by = "id") |>
    dplyr::mutate(
      lm.95.rel = dplyr::case_when(
        lm.Rel == "lmRel" & lmDir == "lmImp" ~ "lmRelImp",
        lm.Rel == "lmRel" & lmDir == "lmDet" ~ "lmRelDet",
        TRUE                                 ~ "lmNoRel"
      )
    )
}


# this is the metafor::rma() function automated for our simulation:
compute_rma <- function(x) {
  
  # fit per-id models once, reuse for p-values and slopes
  models <- x |>
    (\(d) split(d, d$id))() |>
    purrr::map(\(df)
               metafor::rma.uni(
                 obs ~ time,
                 SEm^2,
                 method = "FE",
                 data   = df,
                 digits = 4
               )
    ) |>
    purrr::map(broom::tidy)
  
  pval.data <- models |>
    purrr::map_dfr("p.value") |>
    (\(df) df[2, , drop = FALSE])() |>
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to  = "id",
      values_to = "rma.p.value"
    ) |>
    dplyr::mutate(
      rma.Rel = dplyr::case_when(rma.p.value < 0.05 ~ "rmaRel", TRUE ~ "rmaNoRel"),
      id = as.integer(id)
    )
  
  slp.data <- models |>
    purrr::map_dfr("estimate") |>
    (\(df) df[2, , drop = FALSE])() |>
    tidyr::pivot_longer(
      cols = dplyr::everything(),
      names_to  = "id",
      values_to = "rma.est"
    ) |>
    dplyr::mutate(
      rmaDir = dplyr::case_when(
        rma.est <  0 ~ "rmaImp",
        rma.est >  0 ~ "rmaDet",
        TRUE         ~ "rmaNoChange"
      ),
      id = as.integer(id)
    )
  
  x |>
    dplyr::full_join(pval.data, by = "id") |>
    dplyr::full_join(slp.data,  by = "id") |>
    dplyr::mutate(
      rma.95.rel = dplyr::case_when(
        rma.Rel == "rmaRel" & rmaDir == "rmaImp" ~ "rmaRelImp",
        rma.Rel == "rmaRel" & rmaDir == "rmaDet" ~ "rmaRelDet",
        TRUE                                     ~ "rmaNoRel"
      )
    )
}


# functions to compute the total accuracy from each method
total_accuracy_rma <- function(x){
  (sum(x$rma.95.rel   == "rmaRelDet"  & x$TrueChange == "TrueDet")  +
     sum(x$rma.95.rel == "rmaRelImp"  & x$TrueChange == "TrueImp") +
     sum(x$rma.95.rel == "rmaNoRel"   & x$TrueChange == "TrueNo")) / 
    nrow(x)
}

total_accuracy_rci <- function(x){
  (sum(x$ReliableChange   == "RelDet" & x$TrueChange == "TrueDet")  +
     sum(x$ReliableChange == "RelImp" & x$TrueChange == "TrueImp") + 
     sum(x$ReliableChange == "NoRel"  & x$TrueChange == "TrueNo")) / 
    nrow(x)
}

total_accuracy_lm <- function(x){
  (sum(x$lm.95.rel   == "lmRelDet" & x$TrueChange == "TrueDet")  +
     sum(x$lm.95.rel == "lmRelImp" & x$TrueChange == "TrueImp") +
     sum(x$lm.95.rel == "lmNoRel"  & x$TrueChange == "TrueNo")) / 
    nrow(x)
}

total_accuracy_obs <- function(x){
  (sum(x$obsChange   == "ObsDet" & x$TrueChange == "TrueDet")  +
     sum(x$obsChange == "ObsImp" & x$TrueChange == "TrueImp") +
     sum(x$obsChange == "obsNo" & x$TrueChange == "TrueNo")) / 
    nrow(x)
}

total_accuracy_rmaDir <- function(x){
  (sum(x$rmaDir   == "rmaDet"      & x$TrueChange == "TrueDet")  +
     sum(x$rmaDir == "rmaImp"      & x$TrueChange == "TrueImp")  +
     sum(x$rmaDir == "rmaNoChange" & x$TrueChange == "TrueNo")) / 
    nrow(x)
}

total_accuracy_lmDir <- function(x){
  (sum(x$lmDir  == "lmDet" & x$TrueChange == "TrueDet")  +
     sum(x$lmDir  == "lmImp" & x$TrueChange == "TrueImp") + 
     sum(x$lmDir == "lmNoChange" & x$TrueChange == "TrueNo")) / 
    nrow(x)
}

