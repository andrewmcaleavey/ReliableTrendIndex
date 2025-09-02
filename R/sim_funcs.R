##### RCI SIM FUNCTIONS (internal; not exported or documented) ##################

# Generate 2 timepoint data for RCI analysis
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
  
  sim_dat1 <- dplyr::bind_cols(id   = rep(seq(1:n_ppl), each = 2),
                               time = rep(c(0, 1), n_ppl))
  
  t0 <- tibble::tibble(id = 1:n_ppl,
                       time = 0,
                       obs = t0_obs,
                       tru = t0_true)
  t1 <- tibble::tibble(id = 1:n_ppl,
                       time = 1,
                       obs = t1_obs,
                       tru = t1_true)
  
  simdat2 <- dplyr::bind_rows(t0, t1) %>%
    dplyr::arrange(id, time) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(obs_diff  = obs - dplyr::first(obs),
                  true_diff = tru - dplyr::first(tru)) %>%
    dplyr::ungroup()
  
  # RCI
  SEm <- stats::sd(simdat2 %>%
                     dplyr::filter(time == 0) %>%
                     dplyr::pull(obs)) *
    sqrt(1 - (sd_tot^2 / (sd_tot^2 + meas_err^2)))
  
  Sdiff <<- sqrt(2) * SEm
  RCI   <<- 1.96 * Sdiff
  
  simdat3 <- simdat2 %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(ReliableChange = ifelse(dplyr::last(obs_diff) >  RCI, "RelDet",
                                          ifelse(dplyr::last(obs_diff) < -RCI, "RelImp", "NoRel")),
                  TrueChange = ifelse(dplyr::last(true_diff) > 0, "TrueDet",
                                      ifelse(dplyr::last(true_diff) < 0, "TrueImp", "TrueNo")),
                  obsChange = ifelse(dplyr::last(obs_diff) > 0, "ObsDet",
                                     ifelse(dplyr::last(obs_diff) < 0, "ObsImp", "ObsNo"))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      wrongRCI = dplyr::case_when(
        ReliableChange == "RelDet" & TrueChange == "TrueDet" ~ 0,
        ReliableChange == "RelImp" & TrueChange == "TrueImp" ~ 0,
        ReliableChange == "NoRel"  & TrueChange == "TrueNo"  ~ 0,
        TRUE ~ 1
      ),
      trueRCI = dplyr::case_when(
        true_diff < -RCI ~ "TrueRCIImp",
        true_diff >  RCI ~ "TrueRCIDet",
        TRUE ~ "TrueNotSure"
      ),
      borderMisclassified = dplyr::case_when(
        ReliableChange == "RelDet" & trueRCI == "TrueNotSure" ~ 1,
        ReliableChange == "NoRel"  & trueRCI == "TrueRCIDet"  ~ 1,
        ReliableChange == "NoRel"  & trueRCI == "TrueRCIImp"  ~ 1,
        ReliableChange == "RelImp" & trueRCI == "TrueNotSure" ~ 1,
        TRUE ~ 0
      ),
      delta_err = obs_diff - true_diff
    ) %>%
    dplyr::mutate(
      obsCorrect = dplyr::case_when(
        true_diff > 0 & obs_diff > 0 ~ TRUE,
        true_diff < 0 & obs_diff < 0 ~ TRUE,
        TRUE ~ FALSE
      ),
      RCICorrect = dplyr::case_when(
        wrongRCI == 1 ~ FALSE,
        TRUE ~ TRUE
      )
    ) %>%
    dplyr::mutate(
      nullTile = stats::pnorm(q = obs_diff, mean = 0, sd = Sdiff),
      delta = delta
    )
  return(simdat3)
}

# Generate X timepoint data for RCI analysis
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
  
  data_1 <- tibble::tibble(
    id = rep(1:n_ppl, each = n_obs),
    time = rep(seq(0, 1, by = 1/(n_obs - 1)), n_ppl),
    true_t0 = rep(t0_true, each = n_obs),
    true_slope = rep(slope_true, each = n_obs)
  ) %>%
    dplyr::mutate(
      true_value = true_t0 + (true_slope * time),
      obs = true_value + rnorm(n_obs * n_ppl, mean = 0, sd = meas_err)
    ) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(
      obs_diff  = obs - dplyr::first(obs),
      true_diff = true_value - dplyr::first(true_t0)
    ) %>%
    dplyr::ungroup()
  
  # RCI
  SEm <- stats::sd(data_1 %>% dplyr::filter(time == 0) %>% dplyr::pull(obs)) *
    sqrt(1 - (sd_tot^2 / (sd_tot^2 + meas_err^2)))
  Sdiff <<- sqrt(2) * SEm
  RCI   <<- 1.96 * Sdiff
  
  data_2 <- data_1 %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(
      ReliableChange = ifelse(dplyr::last(obs_diff) >  RCI, "RelDet",
                              ifelse(dplyr::last(obs_diff) < -RCI, "RelImp", "NoRel")),
      TrueChange = ifelse(dplyr::last(true_diff) > 0, "TrueDet",
                          ifelse(dplyr::last(true_diff) < 0, "TrueImp", "TrueNo")),
      obsChange = ifelse(dplyr::last(obs_diff) > 0, "ObsDet",
                         ifelse(dplyr::last(obs_diff) < 0, "ObsImp", "ObsNo"))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      wrongRCI = dplyr::case_when(
        ReliableChange == "RelDet" & TrueChange == "TrueDet" ~ 0,
        ReliableChange == "RelImp" & TrueChange == "TrueImp" ~ 0,
        ReliableChange == "NoRel"  & TrueChange == "TrueNo"  ~ 0,
        TRUE ~ 1
      ),
      trueRCI = dplyr::case_when(
        true_diff < -RCI ~ "TrueRCIImp",
        true_diff >  RCI ~ "TrueRCIDet",
        TRUE ~ "TrueNotSure"
      ),
      borderMisclassified = dplyr::case_when(
        ReliableChange == "RelDet" & trueRCI == "TrueNotSure" ~ 1,
        ReliableChange == "NoRel"  & trueRCI == "TrueRCIDet"  ~ 1,
        ReliableChange == "NoRel"  & trueRCI == "TrueRCIImp"  ~ 1,
        ReliableChange == "RelImp" & trueRCI == "TrueNotSure" ~ 1,
        TRUE ~ 0
      ),
      delta_err = obs_diff - true_diff
    ) %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(
      obsCorrect = dplyr::case_when(
        true_slope > 0 & dplyr::last(obs_diff) > 0 ~ TRUE,
        true_slope < 0 & dplyr::last(obs_diff) < 0 ~ TRUE,
        TRUE ~ FALSE
      ),
      RCICorrect = dplyr::case_when(
        wrongRCI == 1 ~ FALSE,
        TRUE ~ TRUE
      )
    ) %>%
    dplyr::mutate(
      nullTile = stats::pnorm(q = obs_diff, mean = 0, sd = Sdiff),
      delta = delta
    ) %>%
    dplyr::ungroup()
  return(data_2)
}

RCIfunc <- function(rxx, s1 = 1, cut = 1.96){
  cut * sqrt(2 * (s1 * sqrt(1 - rxx))^2)
}

# Simple plot
plot_raw <- function(x){
  x %>%
    ggplot2::ggplot(ggplot2::aes(x = time, y = obs)) +
    ggplot2::geom_line(ggplot2::aes(group = id), alpha = .02) +
    ggplot2::geom_point(size = 3, alpha = .02, shape = 21) +
    ggplot2::theme_bw() +
    ggplot2::geom_smooth(method = "lm") +
    ggplot2::labs(title = "Simulated data",
                  y = "Observed score (higher is worse)",
                  caption = "There is some range of starting places and outcomes")
}

# Simple raw histogram of observed difference scores
hist_obs_diff_raw <- function(x){
  x %>%
    dplyr::filter(time == 1) %>%
    ggplot2::ggplot(ggplot2::aes(x = obs_diff)) +
    ggplot2::geom_histogram(binwidth = .2) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Simulated data",
                  subtitle = "Observed difference scores")
}

# Compute reliability from known true and observed scores
rxx_empirical <- function(x){
  stats::sd(x %>% dplyr::filter(time == 0) %>% dplyr::pull(tru))^2 /
    stats::sd(x %>% dplyr::filter(time == 0) %>% dplyr::pull(obs))^2
}

# Line plot with RCI groups
plot_rci_grps <- function(x){
  x %>%
    ggplot2::ggplot(ggplot2::aes(x = time, y = obs, group = id, color = ReliableChange)) +
    ggplot2::geom_line(ggplot2::aes(color = ReliableChange), alpha = .02)  +
    ggplot2::geom_point(ggplot2::aes(color = ReliableChange), size = 3, alpha = .04) +
    ggplot2::theme_bw() +
    ggplot2::geom_smooth(ggplot2::aes(group = ReliableChange, fill = ReliableChange), method = "lm") +
    ggplot2::labs(title = "Simulated data with RCI ",
                  y = "Observed score (higher is worse)",
                  caption = "Colors show RCI categories. Bands represent 95%CI for these groups.")
}

# Generate a simple table of ReliableChange
relChangeRaw_table <- function(x, divideByTwo = TRUE){
  if (divideByTwo) return(table(x$ReliableChange) / 2)
  else return(table(x$ReliableChange))
}

# Make a ReliableChange table by percentages
relChangePct_table <- function(x){
  100 * relChangeRaw_table(x) / nrow(x)
}

# counting summary over many metrics
counting_func <- function(x){
  x_original <- x
  x <- dplyr::filter(x, time == 1)
  
  NumRelImp <-  sum(x$ReliableChange == "RelImp")
  NumRelDet <-  sum(x$ReliableChange == "RelDet")
  NumNoRel  <-  sum(x$ReliableChange == "NoRel")
  PctRelImp <-  sum(x$ReliableChange == "RelImp") / nrow(x)
  PctRelDet <-  sum(x$ReliableChange == "RelDet") / nrow(x)
  PctNoRel  <-  sum(x$ReliableChange == "NoRel") / nrow(x)
  
  # observed
  NumObsImp <-  sum(x$obsChange == "ObsImp")
  NumObsDet <-  sum(x$obsChange == "ObsDet")
  NumObsNo  <-  sum(x$obsChange == "ObsNo")
  PctObsImp <-  sum(x$obsChange == "ObsImp") / nrow(x)
  PctObsDet <-  sum(x$obsChange == "ObsDet") / nrow(x)
  PctObsNo  <-  sum(x$obsChange == "ObsNo")  / nrow(x)
  
  # true
  NumTrueImp  <-  sum(x$TrueChange == "TrueImp")
  NumTrueDet  <-  sum(x$TrueChange == "TrueDet")
  NumTrueNo   <-  sum(x$TrueChange == "TrueNo")
  PctTrueImp  <-  sum(x$TrueChange == "TrueImp") / nrow(x)
  PctTrueDet  <-  sum(x$TrueChange == "TrueDet") / nrow(x)
  PctTrueNo   <-  sum(x$TrueChange == "TrueNo")  / nrow(x)
  
  # total accuracy
  RCITotalAcc <-  sum(x$RCICorrect) / nrow(x)
  ObsTotalAcc <-  sum(x$obsCorrect) / nrow(x)
  
  # RCI accuracy when it provides a decision
  RCIAccIfSig <-  x %>%
    dplyr::filter(ReliableChange != "NoRel") %>%
    dplyr::summarise(numcorrect  = sum(RCICorrect)) %>%
    dplyr::pull() / nrow(x %>% dplyr::filter(ReliableChange != "NoRel"))
  
  # mean accuracy of the difference score in the RCI-nochange group
  ObsAccInNoRel <- x %>%
    dplyr::filter(ReliableChange == "NoRel") %>%
    dplyr::summarise(sum(obsCorrect)) %>%
    dplyr::pull() /
    nrow(dplyr::filter(x, ReliableChange == "NoRel"))
  
  # Sensitivity and Specificity
  SensDetRCI <- x %>%
    dplyr::filter(TrueChange == "TrueDet") %>%
    dplyr::summarise(sum(RCICorrect)) %>%
    dplyr::pull() / nrow(dplyr::filter(x, TrueChange == "TrueDet"))
  SensImpRCI <- x %>%
    dplyr::filter(TrueChange == "TrueImp") %>%
    dplyr::summarise(sum(RCICorrect)) %>%
    dplyr::pull() / nrow(dplyr::filter(x, TrueChange == "TrueImp"))
  
  SensDetObs <- x %>%
    dplyr::filter(TrueChange == "TrueDet") %>%
    dplyr::summarise(sum(obsCorrect)) %>%
    dplyr::pull() / nrow(dplyr::filter(x, TrueChange == "TrueDet"))
  SensImpObs <- x %>%
    dplyr::filter(TrueChange == "TrueImp") %>%
    dplyr::summarise(sum(obsCorrect)) %>%
    dplyr::pull() / nrow(dplyr::filter(x, TrueChange == "TrueImp"))
  
  SpecDetRCI <- nrow(dplyr::filter(x, TrueChange != "TrueDet" & ReliableChange != "RelDet")) /
    nrow(dplyr::filter(x, TrueChange != "TrueDet"))
  SpecImpRCI <- nrow(dplyr::filter(x, TrueChange != "TrueImp" & ReliableChange != "RelImp")) /
    nrow(dplyr::filter(x, TrueChange != "TrueImp"))
  
  SpecDetObs <- nrow(dplyr::filter(x, TrueChange != "TrueDet" & obsChange != "ObsDet")) /
    nrow(dplyr::filter(x, TrueChange != "TrueDet"))
  SpecImpObs <- nrow(dplyr::filter(x, TrueChange != "TrueImp" & obsChange != "ObsImp")) /
    nrow(dplyr::filter(x, TrueChange != "TrueImp"))
  
  # PPP and NPP
  PPPDetRCI <- nrow(dplyr::filter(x, TrueChange == "TrueDet" & ReliableChange == "RelDet")) /
    sum(x$ReliableChange == "RelDet")
  PPPImpRCI <- nrow(dplyr::filter(x, TrueChange == "TrueImp" & ReliableChange == "RelImp")) /
    sum(x$ReliableChange == "RelImp")
  PPPDetObs <- nrow(dplyr::filter(x, TrueChange == "TrueDet" & obsChange == "ObsDet")) /
    sum(x$obsChange == "ObsDet")
  PPPImpObs <- nrow(dplyr::filter(x, TrueChange == "TrueImp" & obsChange == "ObsImp")) /
    sum(x$obsChange == "ObsImp")
  
  NPPDetRCI <- nrow(dplyr::filter(x, TrueChange != "TrueDet" & ReliableChange != "RelDet")) /
    nrow(dplyr::filter(x, ReliableChange != "RelDet"))
  NPPImpRCI <- nrow(dplyr::filter(x, TrueChange != "TrueImp" & ReliableChange != "RelImp")) /
    nrow(dplyr::filter(x, ReliableChange != "RelImp"))
  NPPDetObs <- nrow(dplyr::filter(x, TrueChange != "TrueDet" & obsChange != "ObsDet")) /
    nrow(dplyr::filter(x, obsChange != "ObsDet"))
  NPPImpObs <- nrow(dplyr::filter(x, TrueChange != "TrueImp" & obsChange != "ObsImp")) /
    nrow(dplyr::filter(x, obsChange != "ObsImp"))
  
  # Type S errors
  TypeSRCI <- nrow(dplyr::filter(x, !RCICorrect & ReliableChange != "NoRel")) /
    nrow(dplyr::filter(x, ReliableChange != "NoRel"))
  TypeSRCIDet <- nrow(dplyr::filter(x, !RCICorrect & ReliableChange == "RelDet")) /
    nrow(dplyr::filter(x, ReliableChange == "RelDet"))
  TypeSRCIImp <- nrow(dplyr::filter(x, !RCICorrect & ReliableChange == "RelImp")) /
    nrow(dplyr::filter(x, ReliableChange == "RelImp"))
  
  TypeSObs <- nrow(dplyr::filter(x, !obsCorrect & obsChange != "ObsNo")) /
    nrow(dplyr::filter(x, obsChange != "NoRel"))
  TypeSObsDet <- nrow(dplyr::filter(x, !obsCorrect & obsChange == "ObsDet")) /
    nrow(dplyr::filter(x, obsChange == "ObsDet"))
  TypeSObsImp <- nrow(dplyr::filter(x, !obsCorrect & obsChange == "ObsImp")) /
    nrow(dplyr::filter(x, obsChange == "ObsImp"))
  
  # MAE and Bias
  MAEdiff <- mean(abs(x$delta_err))
  AbsErrBiasDiff <- mean(x$delta_err)
  
  MAEdiffRCI <- dplyr::summarise(dplyr::group_by(x, ReliableChange), MAEDiff = mean(abs(delta_err)))
  AbsErrBiasDiffRCI <- dplyr::summarise(dplyr::group_by(x, ReliableChange), AbsBias = mean(delta_err))
  
  MAEdiffObs <- dplyr::summarise(dplyr::group_by(x, obsChange), MAEDiff = mean(abs(delta_err)))
  AbsErrBiasDiffObs <- dplyr::summarise(dplyr::group_by(x, obsChange), AbsBias = mean(delta_err))
  
  # classification tables
  trueVsObs <- table(x$TrueChange, x$obsChange)
  trueVsRCI <- table(x$TrueChange, x$ReliableChange)
  
  # reliability
  rxx <- rxx_empirical(x_original)
  RCI <- RCIfunc(rxx = rxx,
                 s1 = stats::sd(dplyr::pull(dplyr::filter(x_original, time == 0), obs)))
  
  PctClassifiedRCI <- nrow(dplyr::filter(x, ReliableChange != "NoRel")) / nrow(x)
  
  # call info
  delta <- x$delta[1]
  
  return(mget(objects(), ifnotfound = NA))
}

# make a function to take the output and put in a combinable data frame
# here, x is the list output of counting_func()
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

plotting_func <- function(comparison.data){
  # Total Accuracy
  plotAcc <- ggplot2::ggplot(comparison.data,
                             ggplot2::aes(x = rxx, y = ObsTotalAcc)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(y = RCITotalAcc),
                       linetype = "dashed",
                       color = "gray40") +
    ggplot2::geom_point(ggplot2::aes(y = RCITotalAcc),
                        color = "gray40") +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Total accuracy",
                  x = expression(r[xx]),
                  y = "Total accuracy")
  
  # Type S
  plotTypeS <- ggplot2::ggplot(comparison.data,
                               ggplot2::aes(x = rxx, y = TypeSObs)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(y = TypeSRCI),
                       linetype = "dashed",
                       color = "gray40") +
    ggplot2::geom_point(ggplot2::aes(y = TypeSRCI),
                        color = "gray40") +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Type S Error",
                  x = expression(r[xx]),
                  y = "Type S Error")
  
  ## RCI value
  plotRCI <- ggplot2::ggplot(comparison.data,
                             ggplot2::aes(x = rxx, y = RCI)) +
    ggplot2::geom_line(linetype = "dashed", color = "gray40") +
    ggplot2::geom_point() +
    ggplot2::theme_bw() +
    ggplot2::lims(y = c(0, NA)) +
    ggplot2::labs(title = "RCI value",
                  x = expression(r[xx]),
                  y = "RCI value")
  
  ## percent classified
  plotClassifiedRCI <- ggplot2::ggplot(comparison.data,
                                       ggplot2::aes(x = rxx, y = PctClassifiedRCI)) +
    ggplot2::geom_line(linetype = "dashed", color = "gray40") +
    ggplot2::geom_point() +
    ggplot2::theme_bw() +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::labs(title = "Percent \"Reliably\" Changed",
                  x = expression(r[xx]),
                  y = "Percent Classified as Reliably Changed")
  
  # group-level accuracy
  plotGrpAccDet <- comparison.data %>%
    dplyr::select(rxx, PctTrueDet, PctRelDet, PctObsDet) %>%
    tidyr::pivot_longer(cols = -rxx, values_to = "Proportion", names_to = "Metric") %>%
    ggplot2::ggplot(ggplot2::aes(x = rxx, y = Proportion, linetype = Metric, shape = Metric)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position="bottom") +
    ggplot2::scale_linetype_discrete(labels = c("Observed Sign", "RCI", "True")) +
    ggplot2::scale_shape_discrete(labels = c("Observed Sign", "RCI", "True")) +
    ggplot2::labs(title = "Deterioration: Group Accuracy",
                  y = "Portion with deterioration",
                  x = expression(r[xx]))
  
  plotGrpAccImp <- comparison.data %>%
    dplyr::select(rxx, PctTrueImp, PctRelImp, PctObsImp) %>%
    tidyr::pivot_longer(cols = -rxx, values_to = "Proportion", names_to = "Metric") %>%
    ggplot2::ggplot(ggplot2::aes(x = rxx, y = Proportion, linetype = Metric, shape = Metric)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position="bottom") +
    ggplot2::scale_linetype_discrete(labels = c("Observed Sign", "RCI", "True")) +
    ggplot2::scale_shape_discrete(labels = c("Observed Sign", "RCI", "True")) +
    ggplot2::labs(title = "Improvement: Group Accuracy",
                  y = "Portion with improvement",
                  x = expression(r[xx]))
  
  # Sensitivity
  plotSensDet <- ggplot2::ggplot(comparison.data,
                                 ggplot2::aes(x = rxx, y = SensDetObs)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(y = SensDetRCI),
                       linetype = "dashed",
                       color = "gray40") +
    ggplot2::geom_point(ggplot2::aes(y = SensDetRCI),
                        color = "gray40") +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Sensitivity to Deterioration",
                  x = expression(r[xx]),
                  y = "Sensitivity")
  
  plotSensImp <- ggplot2::ggplot(comparison.data,
                                 ggplot2::aes(x = rxx, y = SensImpObs)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(y = SensImpRCI),
                       linetype = "dashed",
                       color = "gray40") +
    ggplot2::geom_point(ggplot2::aes(y = SensImpRCI),
                        color = "gray40") +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Sensitivity to Improvement",
                  x = expression(r[xx]),
                  y = "Sensitivity")
  
  # Specificity
  plotSpecDet <- ggplot2::ggplot(comparison.data,
                                 ggplot2::aes(x = rxx, y = SpecDetObs)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(y = SpecDetRCI),
                       linetype = "dashed",
                       color = "gray40") +
    ggplot2::geom_point(ggplot2::aes(y = SpecDetRCI),
                        color = "gray40") +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Specificity to Deterioration",
                  x = expression(r[xx]),
                  y = "Specificity")
  
  plotSpecImp <- ggplot2::ggplot(comparison.data,
                                 ggplot2::aes(x = rxx, y = SpecImpObs)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(y = SpecImpRCI),
                       linetype = "dashed",
                       color = "gray40") +
    ggplot2::geom_point(ggplot2::aes(y = SpecImpRCI),
                        color = "gray40") +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Specificity to Improvement",
                  x = expression(r[xx]),
                  y = "Specificity")
  
  # PPP
  plotPPPDet <- ggplot2::ggplot(comparison.data,
                                ggplot2::aes(x = rxx, y = PPPDetObs)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(y = PPPDetRCI),
                       linetype = "dashed",
                       color = "gray40") +
    ggplot2::geom_point(ggplot2::aes(y = PPPDetRCI),
                        color = "gray40") +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "PPP to Deterioration",
                  x = expression(r[xx]),
                  y = "PPP")
  
  plotPPPImp <- ggplot2::ggplot(comparison.data,
                                ggplot2::aes(x = rxx, y = PPPImpObs)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(y = PPPImpRCI),
                       linetype = "dashed",
                       color = "gray40") +
    ggplot2::geom_point(ggplot2::aes(y = PPPImpRCI),
                        color = "gray40") +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "PPP to Improvement",
                  x = expression(r[xx]),
                  y = "PPP")
  
  # NPP
  plotNPPDet <- ggplot2::ggplot(comparison.data,
                                ggplot2::aes(x = rxx, y = NPPDetObs)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(y = NPPDetRCI),
                       linetype = "dashed",
                       color = "gray40") +
    ggplot2::geom_point(ggplot2::aes(y = NPPDetRCI),
                        color = "gray40") +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "NPP to Deterioration",
                  x = expression(r[xx]),
                  y = "NPP")
  
  plotNPPImp <- ggplot2::ggplot(comparison.data,
                                ggplot2::aes(x = rxx, y = NPPImpObs)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(y = NPPImpRCI),
                       linetype = "dashed",
                       color = "gray40") +
    ggplot2::geom_point(ggplot2::aes(y = NPPImpRCI),
                        color = "gray40") +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "NPP to Improvement",
                  x = expression(r[xx]),
                  y = "NPP")
  
  return(mget(objects()))
}

# Plotting simulation results from effect size simulation
plotting_func_delta <- function(comparison.data){
  # Total Accuracy
  plotAcc <- ggplot2::ggplot(comparison.data,
                             ggplot2::aes(x = delta, y = ObsTotalAcc)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(y = RCITotalAcc),
                       linetype = "dashed",
                       color = "gray40") +
    ggplot2::geom_point(ggplot2::aes(y = RCITotalAcc),
                        color = "gray40") +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Total accuracy",
                  x = "Mean change",
                  y = "Total accuracy")
  
  # Type S
  plotTypeS <- ggplot2::ggplot(comparison.data,
                               ggplot2::aes(x = delta, y = TypeSObs)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(y = TypeSRCI),
                       linetype = "dashed",
                       color = "gray40") +
    ggplot2::geom_point(ggplot2::aes(y = TypeSRCI),
                        color = "gray40") +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Type S Error",
                  x = "Mean change",
                  y = "Type S Error")
  
  ## RCI value
  plotRCI <- ggplot2::ggplot(comparison.data,
                             ggplot2::aes(x = delta, y = RCI)) +
    ggplot2::geom_line(linetype = "dashed", color = "gray40") +
    ggplot2::geom_point() +
    ggplot2::theme_bw() +
    ggplot2::lims(y = c(0, NA)) +
    ggplot2::labs(title = "RCI value",
                  x = "Mean change",
                  y = "RCI value")
  
  ## percent classified
  plotClassifiedRCI <- ggplot2::ggplot(comparison.data,
                                       ggplot2::aes(x = delta, y = PctClassifiedRCI)) +
    ggplot2::geom_line(linetype = "dashed", color = "gray40") +
    ggplot2::geom_point() +
    ggplot2::theme_bw() +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::labs(title = "Percent \"Reliably\" Changed",
                  x = "Mean change",
                  y = "Percent Classified as Reliably Changed")
  
  # group-level accuracy
  plotGrpAccDet <- comparison.data %>%
    dplyr::select(delta, PctTrueDet, PctRelDet, PctObsDet) %>%
    tidyr::pivot_longer(cols = -delta, values_to = "Proportion", names_to = "Metric") %>%
    ggplot2::ggplot(ggplot2::aes(x = delta, y = Proportion, linetype = Metric, shape = Metric)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position="bottom") +
    ggplot2::scale_linetype_discrete(labels = c("Observed Sign", "RCI", "True")) +
    ggplot2::scale_shape_discrete(labels = c("Observed Sign", "RCI", "True")) +
    ggplot2::labs(title = "Deterioration: Group Accuracy",
                  y = "Portion with deterioration",
                  x = "Mean change")
  
  plotGrpAccImp <- comparison.data %>%
    dplyr::select(delta, PctTrueImp, PctRelImp, PctObsImp) %>%
    tidyr::pivot_longer(cols = -delta, values_to = "Proportion", names_to = "Metric") %>%
    ggplot2::ggplot(ggplot2::aes(x = delta, y = Proportion, linetype = Metric, shape = Metric)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position="bottom") +
    ggplot2::scale_linetype_discrete(labels = c("Observed Sign", "RCI", "True")) +
    ggplot2::scale_shape_discrete(labels = c("Observed Sign", "RCI", "True")) +
    ggplot2::labs(title = "Improvement: Group Accuracy",
                  y = "Portion with improvement",
                  x = "Mean change")
  
  # Sensitivity
  plotSensDet <- ggplot2::ggplot(comparison.data,
                                 ggplot2::aes(x = delta, y = SensDetObs)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(y = SensDetRCI),
                       linetype = "dashed",
                       color = "gray40") +
    ggplot2::geom_point(ggplot2::aes(y = SensDetRCI),
                        color = "gray40") +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Sensitivity to Deterioration",
                  x = "Mean change",
                  y = "Sensitivity")
  
  plotSensImp <- ggplot2::ggplot(comparison.data,
                                 ggplot2::aes(x = delta, y = SensImpObs)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(y = SensImpRCI),
                       linetype = "dashed",
                       color = "gray40") +
    ggplot2::geom_point(ggplot2::aes(y = SensImpRCI),
                        color = "gray40") +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Sensitivity to Improvement",
                  x = "Mean change",
                  y = "Sensitivity")
  
  # Specificity
  plotSpecDet <- ggplot2::ggplot(comparison.data,
                                 ggplot2::aes(x = delta, y = SpecDetObs)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(y = SpecDetRCI),
                       linetype = "dashed",
                       color = "gray40") +
    ggplot2::geom_point(ggplot2::aes(y = SpecDetRCI),
                        color = "gray40") +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Specificity to Deterioration",
                  x = "Mean change",
                  y = "Specificity")
  
  plotSpecImp <- ggplot2::ggplot(comparison.data,
                                 ggplot2::aes(x = delta, y = SpecImpObs)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(y = SpecImpRCI),
                       linetype = "dashed",
                       color = "gray40") +
    ggplot2::geom_point(ggplot2::aes(y = SpecImpRCI),
                        color = "gray40") +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "Specificity to Improvement",
                  x = "Mean change",
                  y = "Specificity")
  
  # PPP
  plotPPPDet <- ggplot2::ggplot(comparison.data,
                                ggplot2::aes(x = delta, y = PPPDetObs)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(y = PPPDetRCI),
                       linetype = "dashed",
                       color = "gray40") +
    ggplot2::geom_point(ggplot2::aes(y = PPPDetRCI),
                        color = "gray40") +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "PPP to Deterioration",
                  x = "Mean change",
                  y = "PPP")
  
  plotPPPImp <- ggplot2::ggplot(comparison.data,
                                ggplot2::aes(x = delta, y = PPPImpObs)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(y = PPPImpRCI),
                       linetype = "dashed",
                       color = "gray40") +
    ggplot2::geom_point(ggplot2::aes(y = PPPImpRCI),
                        color = "gray40") +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "PPP to Improvement",
                  x = "Mean change",
                  y = "PPP")
  
  # NPP
  plotNPPDet <- ggplot2::ggplot(comparison.data,
                                ggplot2::aes(x = delta, y = NPPDetObs)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(y = NPPDetRCI),
                       linetype = "dashed",
                       color = "gray40") +
    ggplot2::geom_point(ggplot2::aes(y = NPPDetRCI),
                        color = "gray40") +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "NPP to Deterioration",
                  x = "Mean change",
                  y = "NPP")
  
  plotNPPImp <- ggplot2::ggplot(comparison.data,
                                ggplot2::aes(x = delta, y = NPPImpObs)) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::geom_line(ggplot2::aes(y = NPPImpRCI),
                       linetype = "dashed",
                       color = "gray40") +
    ggplot2::geom_point(ggplot2::aes(y = NPPImpRCI),
                        color = "gray40") +
    ggplot2::lims(y = c(0, 1)) +
    ggplot2::theme_bw() +
    ggplot2::labs(title = "NPP to Improvement",
                  x = "Mean change",
                  y = "NPP")
  
  return(mget(objects()))
}

# independent linear regressions (without prespecified measurement error)
compute_lm <- function(x){
  pval.data <- x %>%
    split(.$id) %>%
    purrr::map(~ stats::lm(obs ~ time, data = .x)) %>%
    purrr::map(broom::tidy) %>%
    purrr::map_dfr("p.value") %>%
    .[2, ] %>%
    tidyr::pivot_longer(cols = tidyselect::everything(),
                        names_to = "id",
                        values_to = "lm.p.value") %>%
    dplyr::mutate(lm.Rel = dplyr::case_when(lm.p.value < .05 ~ "lmRel", TRUE ~ "lmNoRel"),
                  id = as.integer(id))
  
  slp.data <- x %>%
    split(.$id) %>%
    purrr::map(~ stats::lm(obs ~ time, data = .x)) %>%
    purrr::map(broom::tidy) %>%
    purrr::map_dfr("estimate") %>%
    .[2, ] %>%
    tidyr::pivot_longer(cols = tidyselect::everything(),
                        names_to = "id",
                        values_to = "lm.est") %>%
    dplyr::mutate(lmDir = dplyr::case_when(lm.est < 0 ~ "lmImp",
                                           lm.est > 0 ~ "lmDet",
                                           TRUE ~ "lmNoChange"),
                  id = as.integer(id))
  
  dplyr::full_join(x, pval.data, by = "id") %>%
    dplyr::full_join(slp.data, by = "id") %>%
    dplyr::mutate(lm.95.rel = dplyr::case_when(
      lm.Rel == "lmRel" & lmDir == "lmImp" ~ "lmRelImp",
      lm.Rel == "lmRel" & lmDir == "lmDet" ~ "lmRelDet",
      TRUE ~ "lmNoRel"))
}

# metafor::rma() automated for our simulation (requires metafor installed)
compute_rma <- function(x){
  pval.data <- x %>%
    split(.$id) %>%
    purrr::map(~ metafor::rma.uni(obs ~ time,
                                  SEm^2,
                                  method = "FE",
                                  data = .x,
                                  digits = 4)) %>%
    purrr::map(broom::tidy) %>%
    purrr::map_dfr("p.value") %>%
    .[2, ] %>%
    tidyr::pivot_longer(cols = tidyselect::everything(),
                        names_to = "id",
                        values_to = "rma.p.value") %>%
    dplyr::mutate(rma.Rel = dplyr::case_when(rma.p.value < .05 ~ "rmaRel", TRUE ~ "rmaNoRel"),
                  id = as.integer(id))
  
  slp.data <- x %>%
    split(.$id) %>%
    purrr::map(~ metafor::rma.uni(obs ~ time,
                                  SEm^2,
                                  method = "FE",
                                  data = .x,
                                  digits = 4)) %>%
    purrr::map(broom::tidy) %>%
    purrr::map_dfr("estimate") %>%
    .[2, ] %>%
    tidyr::pivot_longer(cols = tidyselect::everything(),
                        names_to = "id",
                        values_to = "rma.est") %>%
    dplyr::mutate(rmaDir = dplyr::case_when(rma.est < 0 ~ "rmaImp",
                                            rma.est > 0 ~ "rmaDet",
                                            TRUE ~ "rmaNoChange"),
                  id = as.integer(id))
  
  dplyr::full_join(x, pval.data, by = "id") %>%
    dplyr::full_join(slp.data, by = "id") %>%
    dplyr::mutate(rma.95.rel = dplyr::case_when(
      rma.Rel == "rmaRel" & rmaDir == "rmaImp" ~ "rmaRelImp",
      rma.Rel == "rmaRel" & rmaDir == "rmaDet" ~ "rmaRelDet",
      TRUE ~ "rmaNoRel"))
}

# total accuracy helpers
total_accuracy_rma <- function(x){
  (sum(x$rma.95.rel   == "rmaRelDet" & x$TrueChange == "TrueDet") +
     sum(x$rma.95.rel == "rmaRelImp" & x$TrueChange == "TrueImp") +
     sum(x$rma.95.rel == "rmaNoRel"  & x$TrueChange == "TrueNo")) / nrow(x)
}

total_accuracy_rci <- function(x){
  (sum(x$ReliableChange   == "RelDet" & x$TrueChange == "TrueDet") +
     sum(x$ReliableChange == "RelImp" & x$TrueChange == "TrueImp") +
     sum(x$ReliableChange == "NoRel"  & x$TrueChange == "TrueNo")) / nrow(x)
}

total_accuracy_lm <- function(x){
  (sum(x$lm.95.rel == "lmRelDet" & x$TrueChange == "TrueDet") +
     sum(x$lm.95.rel == "lmRelImp" & x$TrueChange == "TrueImp") +
     sum(x$lm.95.rel == "lmNoRel"  & x$TrueChange == "TrueNo")) / nrow(x)
}

total_accuracy_obs <- function(x){
  (sum(x$obsChange == "ObsDet" & x$TrueChange == "TrueDet") +
     sum(x$obsChange == "ObsImp" & x$TrueChange == "TrueImp") +
     sum(x$obsChange == "obsNo"   & x$TrueChange == "TrueNo")) / nrow(x)
}

total_accuracy_rmaDir <- function(x){
  (sum(x$rmaDir == "rmaDet"      & x$TrueChange == "TrueDet") +
     sum(x$rmaDir == "rmaImp"      & x$TrueChange == "TrueImp") +
     sum(x$rmaDir == "rmaNoChange" & x$TrueChange == "TrueNo")) / nrow(x)
}

total_accuracy_lmDir <- function(x){
  (sum(x$lmDir == "lmDet" & x$TrueChange == "TrueDet") +
     sum(x$lmDir == "lmImp" & x$TrueChange == "TrueImp") +
     sum(x$lmDir == "lmNoChange" & x$TrueChange == "TrueNo")) / nrow(x)
}
