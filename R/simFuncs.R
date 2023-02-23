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
  sim_dat1 <- bind_cols(id = rep(seq(1:n_ppl), each = 2), 
                        time    = rep(c(0, 1), n_ppl))
  
  t0 <- tibble(id = 1:n_ppl, 
               time = 0,
               obs = t0_obs, 
               tru = t0_true)
  t1 <- tibble(id = 1:n_ppl, 
               time = 1, 
               obs = t1_obs, 
               tru = t1_true)
  
  simdat2 <- bind_rows(t0, t1) %>% 
    arrange(id, time) %>% 
    group_by(id) %>% 
    mutate(obs_diff = obs - first(obs), 
           true_diff = tru - first(tru)) %>% 
    ungroup()
  
  # RCI
  SEm <- sd(simdat2 %>% 
              filter(time == 0) %>% 
              pull(obs)) * sqrt(1 - (sd_tot^2 / (sd_tot^2 + meas_err^2)))
  
  Sdiff <<- sqrt(2) * SEm
  
  RCI <<- 1.96 * Sdiff
  
  simdat3 <- simdat2 %>% 
    group_by(id) %>% 
    mutate(ReliableChange = ifelse(last(obs_diff) > RCI, 
                                   "RelDet", 
                                   ifelse(last(obs_diff) < -1*RCI, 
                                          "RelImp", 
                                          "NoRel")), 
           TrueChange = ifelse(last(true_diff) > 0, 
                               "TrueDet", 
                               ifelse(last(true_diff) < 0, 
                                      "TrueImp", 
                                      "TrueNo")), 
           obsChange = ifelse(last(obs_diff) > 0, 
                              "ObsDet", 
                              ifelse(last(obs_diff) < 0, 
                                     "ObsImp", 
                                     "ObsNo"))) %>% 
    ungroup() %>% 
    mutate(wrongRCI = case_when(ReliableChange == "RelDet" & TrueChange == "TrueDet" ~ 0, 
                                ReliableChange == "RelImp" & TrueChange == "TrueImp" ~ 0, 
                                ReliableChange == "NoRel"  & TrueChange == "TrueNo" ~ 0,
                                TRUE ~ 1), 
           trueRCI = case_when(true_diff < -RCI ~ "TrueRCIImp", 
                               true_diff > RCI  ~ "TrueRCIDet", 
                               TRUE ~ "TrueNotSure"), 
           borderMisclassified = case_when(ReliableChange == "RelDet" & trueRCI == "TrueNotSure" ~ 1, 
                                           ReliableChange == "NoRel" & trueRCI == "TrueRCIDet" ~ 1,
                                           ReliableChange == "NoRel" & trueRCI == "TrueRCIImp" ~ 1,
                                           ReliableChange == "RelImp" & trueRCI == "TrueNotSure" ~ 1, 
                                           TRUE ~ 0), 
           delta_err = obs_diff - true_diff) %>% 
    mutate(obsCorrect = case_when(true_diff > 0 & obs_diff > 0 ~ TRUE, 
                                  true_diff < 0 & obs_diff < 0 ~ TRUE, 
                                  TRUE ~ FALSE), 
           RCICorrect = case_when(wrongRCI == 1 ~ FALSE, 
                                  TRUE ~ TRUE)) %>% 
    mutate(nullTile = pnorm(q = obs_diff,  # observed change score
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
#' generate_xt_data()
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
  
  data_1 <- tibble(id = rep(1:n_ppl, each = n_obs), 
                   time = rep(seq(0, 1, by = 1/(n_obs-1)), n_ppl),
                   true_t0 = rep(t0_true, each = n_obs),
                   true_slope = rep(slope_true, each = n_obs)) %>% 
    mutate(true_value = true_t0 + (true_slope * time), 
           obs = true_value + rnorm(n_obs * n_ppl, 
                                    mean = 0, 
                                    sd = meas_err)) %>% 
    group_by(id) %>% 
    mutate(obs_diff = obs - first(obs), 
           true_diff = true_value - first(true_t0)) %>% 
    ungroup()
  
  # RCI
  SEm <- sd(data_1 %>% 
              filter(time == 0) %>% 
              pull(obs)) * sqrt(1 - (sd_tot^2 / (sd_tot^2 + meas_err^2)))
  
  Sdiff <<- sqrt(2) * SEm
  
  RCI <<- 1.96 * Sdiff
  
  data_2 <- data_1 %>% 
    group_by(id) %>% 
    mutate(ReliableChange = ifelse(last(obs_diff) > RCI, 
                                   "RelDet", 
                                   ifelse(last(obs_diff) < -1*RCI, 
                                          "RelImp", 
                                          "NoRel")), 
           TrueChange = ifelse(last(true_diff) > 0, 
                               "TrueDet", 
                               ifelse(last(true_diff) < 0, 
                                      "TrueImp", 
                                      "TrueNo")), 
           obsChange = ifelse(last(obs_diff) > 0, 
                              "ObsDet", 
                              ifelse(last(obs_diff) < 0, 
                                     "ObsImp", 
                                     "ObsNo"))) %>% 
    ungroup() %>% 
    mutate(wrongRCI = case_when(ReliableChange == "RelDet" & TrueChange == "TrueDet" ~ 0, 
                                ReliableChange == "RelImp" & TrueChange == "TrueImp" ~ 0, 
                                ReliableChange == "NoRel"  & TrueChange == "TrueNo" ~ 0,
                                TRUE ~ 1), 
           trueRCI = case_when(true_diff < -RCI ~ "TrueRCIImp", 
                               true_diff > RCI  ~ "TrueRCIDet", 
                               TRUE ~ "TrueNotSure"), 
           borderMisclassified = case_when(ReliableChange == "RelDet" & trueRCI == "TrueNotSure" ~ 1, 
                                           ReliableChange == "NoRel" & trueRCI == "TrueRCIDet" ~ 1,
                                           ReliableChange == "NoRel" & trueRCI == "TrueRCIImp" ~ 1,
                                           ReliableChange == "RelImp" & trueRCI == "TrueNotSure" ~ 1, 
                                           TRUE ~ 0), 
           delta_err = obs_diff - true_diff) %>% 
    group_by(id) %>% 
    mutate(obsCorrect = case_when(true_slope > 0 & last(obs_diff) > 0 ~ TRUE, 
                                  true_slope < 0 & last(obs_diff) < 0 ~ TRUE, 
                                  TRUE ~ FALSE), 
           RCICorrect = case_when(wrongRCI == 1 ~ FALSE, 
                                  TRUE ~ TRUE)) %>% 
    mutate(nullTile = pnorm(q = obs_diff,  # observed change score
                            mean = 0,  # assume no change
                            sd = Sdiff), # assume SD is Sdiff, which is the measurement-error related distribution
           delta = delta)  %>% 
    ungroup()
  return(data_2)
}

RCIfunc <- function(rxx, s1 = 1, cut = 1.96){
  cut * sqrt(2 * (s1 * sqrt(1 - rxx))^2)
}

#' Simple plot
#'
#' @param x 
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' plot_raw(generate_data())
plot_raw <- function(x){
  x %>% 
    ggplot(aes(x = time, 
               y = obs)) + 
    geom_line(aes(group = id), 
              alpha = .02) +
    geom_point(size = 3, alpha = .02, shape = 21) +
    theme_bw() +
    geom_smooth(method = "lm") +
    labs(title = "Simulated data", 
         y = "Observed score (higher is worse)", 
         caption = "There is some range of starting places and outcomes")
}

#' Simple raw histogram of observed difference scores
#'
#' @param x data containing columns labeled `time` and `obs_diff`.
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' hist_obs_diff_raw(generate_data())
hist_obs_diff_raw <- function(x){
  x %>% 
    filter(time == 1) %>% 
    ggplot(aes(x = obs_diff)) + 
    geom_histogram(binwidth = .2) +
    theme_bw() +
    labs(title = "Simulated data", 
         subtitle = "Observed difference scores")
}

#' Compute reliability from known true and observed scores
#'
#' @param x data
#'
#' @return empirical reliability from a data set for simulation
#' @export
#'
#' @examples
#' rxx_empirical(generate_data())
rxx_empirical <- function(x){
  sd(x %>% 
       filter(time == 0) %>% 
       pull(tru))^2 / sd(x %>% 
                           filter(time == 0) %>% 
                           pull(obs))^2 
}

#' Line plot with RCI groups
#'
#' @param x data from simulation
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' plot_rci_grps(generate_data())
plot_rci_grps <- function(x){
  x %>% 
    ggplot(aes(x = time, 
               y = obs, 
               group = id, 
               color = ReliableChange)) +
    geom_line(aes(color = ReliableChange), 
              alpha = .02)  +
    geom_point(aes(color = ReliableChange),
               size = 3, 
               alpha = .04) +
    theme_bw() +
    geom_smooth(aes(group = ReliableChange, 
                    fill = ReliableChange), 
                method = "lm") +
    labs(title = "Simulated data with RCI ", 
         y = "Observed score (higher is worse)", 
         caption = "Colors show RCI categories. Bands represent 95%CI for these groups.")
}


#' Generat a simple table of ReliableChange
#'
#' @param x data
#' @param divideByTwo logical. Should the sample be divided by two (e.g., if
#'   there are two lines per person)? Defaults to TRUE.
#'
#' @return A table. 
#' @export
#'
#' @examples
#' relChangeRaw_table(generate_data())
#' relChangeRaw_table(generate_data(), divideByTwo = FALSE)
relChangeRaw_table <- function(x, 
                               divideByTwo = TRUE){
  if(divideByTwo) return(table(x$ReliableChange) / 2)
  else return(table(x$ReliableChange))
}


#' Make a ReliableChange table by percentages
#'
#' @param x data
#'
#' @return A table
#' @export
#'
#' @examples
#' relChangePct_table(generate_data())
relChangePct_table <- function(x){
  100 * relChangeRaw_table(x) / nrow(x)
}


#####
##Single number values that should be computed and saved for 
# any given simulation. 
# number of each RCI category, observed, and true (7 total)
# percent in each RCI category, observed, and true (7 total)
# total accuracy of RCI, observed difference score
# sensitivity to deterioration of RCI, observed
# specificity to deterioration of RCI, observed
# sensitivity to improvement of RCI, observed
# specificity to improvement of RCI, observed
# Type S error rate of RCI, observed
# Type S error for RCI deterioration and observed
# Type S error for RCI improvement and observed
# MAE, Bias in difference scores
counting_func <- function(x){
  x_original <- x
  x <- filter(x, time == 1)
  
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
  PctObsNo  <-   sum(x$obsChange == "ObsNo")  / nrow(x) 
  
  # true
  NumTrueImp  <-  sum(x$TrueChange == "TrueImp") 
  NumTrueDet  <-  sum(x$TrueChange == "TrueDet") 
  NumTrueNo   <-  sum(x$TrueChange == "TrueNo") 
  PctTrueImp  <-  sum(x$TrueChange == "TrueImp") / nrow(x) 
  PctTrueDet  <-  sum(x$TrueChange == "TrueDet") / nrow(x) 
  PctTrueNo   <-   sum(x$TrueChange == "TrueNo")  / nrow(x)
  
  # total accuracy
  # number right / number total
  RCITotalAcc <-  sum(x$RCICorrect) / nrow(x)
  ObsTotalAcc <-  sum(x$obsCorrect) / nrow(x)
  
  # RCI accuracy when it provides a decision
  RCIAccIfSig <-  x %>%
    filter(ReliableChange != "NoRel") %>%
    summarise(numcorrect  = sum(RCICorrect)) %>% 
    pull() / nrow(x  %>% 
                    filter(ReliableChange != "NoRel"))
  
  # mean accuracy of the difference score in the RCI-nochange group
  ObsAccInNoRel <-   x %>% 
    filter(ReliableChange == "NoRel") %>% 
    summarise(sum(obsCorrect)) %>% 
    pull() /
    x %>% 
    filter(ReliableChange == "NoRel") %>% 
    nrow()
  
  # Sensitivity and Specificity
  SensDetRCI <- x %>% 
    filter(TrueChange == "TrueDet") %>% 
    summarise(sum(RCICorrect)) %>% 
    pull() / nrow(x %>% 
                    filter(TrueChange == "TrueDet"))
  SensImpRCI <- x %>% 
    filter(TrueChange == "TrueImp") %>% 
    summarise(sum(RCICorrect))  %>% 
    pull() / nrow(x %>% 
                    filter(TrueChange == "TrueImp"))
  
  SensDetObs <- x %>% 
    filter(TrueChange == "TrueDet") %>% 
    summarise(sum(obsCorrect)) %>% 
    pull() / nrow(x %>% 
                    filter(TrueChange == "TrueDet"))
  SensImpObs <- x %>% 
    filter(TrueChange == "TrueImp") %>% 
    summarise(sum(obsCorrect))  %>% 
    pull() / nrow(x %>% 
                    filter(TrueChange == "TrueImp"))
  
  SpecDetRCI <- x %>% 
    filter(TrueChange != "TrueDet" & ReliableChange != "RelDet") %>% 
    nrow() / x %>% 
    filter(TrueChange != "TrueDet") %>% 
    nrow()
  SpecImpRCI <- x %>% 
    filter(TrueChange != "TrueImp" & ReliableChange != "RelImp") %>% 
    nrow() / x %>% 
    filter(TrueChange != "TrueImp") %>% 
    nrow()
  
  SpecDetObs <- x %>% 
    filter(TrueChange != "TrueDet" & obsChange != "ObsDet") %>% 
    nrow() / x %>% 
    filter(TrueChange != "TrueDet") %>% 
    nrow()
  SpecImpObs <- x %>% 
    filter(TrueChange != "TrueImp" & obsChange != "ObsImp") %>% 
    nrow() / x %>% 
    filter(TrueChange != "TrueImp") %>% 
    nrow()
  
  # PPP and NPP
  PPPDetRCI <- x %>% 
    filter(TrueChange == "TrueDet" & ReliableChange == "RelDet") %>% 
    nrow() /
    sum(x$ReliableChange == "RelDet")
  PPPImpRCI <- x %>% 
    filter(TrueChange == "TrueImp" & ReliableChange == "RelImp") %>% 
    nrow() /
    sum(x$ReliableChange == "RelImp")
  PPPDetObs <- x %>% 
    filter(TrueChange == "TrueDet" & obsChange == "ObsDet") %>% 
    nrow() /
    sum(x$obsChange == "ObsDet")
  PPPImpObs <- x %>% 
    filter(TrueChange == "TrueImp" & obsChange == "ObsImp") %>% 
    nrow() /
    sum(x$obsChange == "ObsImp")
  
  NPPDetRCI <- x %>% 
    filter(TrueChange != "TrueDet" & ReliableChange != "RelDet") %>% 
    nrow() /
    x %>% filter(ReliableChange != "RelDet") %>% 
    nrow()
  NPPImpRCI <- x %>% 
    filter(TrueChange != "TrueImp" & ReliableChange != "RelImp") %>% 
    nrow() /
    x %>% filter(ReliableChange != "RelImp") %>% 
    nrow()
  NPPDetObs <- x %>% 
    filter(TrueChange != "TrueDet" & obsChange != "ObsDet") %>% 
    nrow() /
    x %>% filter(obsChange != "ObsDet") %>% 
    nrow()
  NPPImpObs <- x %>% 
    filter(TrueChange != "TrueImp" & obsChange != "ObsImp") %>% 
    nrow() /
    x %>% filter(obsChange != "ObsImp") %>% 
    nrow()
  
  # Type S errors
  TypeSRCI <- x %>% 
    filter(!RCICorrect & ReliableChange != "NoRel") %>% 
    nrow() / x %>% 
    filter(ReliableChange != "NoRel") %>% 
    nrow()
  TypeSRCIDet <- x %>% 
    filter(!RCICorrect & ReliableChange == "RelDet") %>% 
    nrow() / x %>% 
    filter(ReliableChange == "RelDet") %>% 
    nrow()
  TypeSRCIImp <- x %>% 
    filter(!RCICorrect & ReliableChange == "RelImp") %>% 
    nrow() / x %>% 
    filter(ReliableChange == "RelImp") %>% 
    nrow()
  
  TypeSObs <- x %>% 
    filter(!obsCorrect & obsChange != "ObsNo") %>% 
    nrow() / x %>% 
    filter(obsChange != "NoRel") %>% 
    nrow()
  TypeSObsDet <- x %>% 
    filter(!obsCorrect & obsChange == "ObsDet") %>% 
    nrow() / x %>% 
    filter(obsChange == "ObsDet") %>% 
    nrow()
  TypeSObsImp <- x %>% 
    filter(!obsCorrect & obsChange == "ObsImp") %>% 
    nrow() / x %>% 
    filter(obsChange == "ObsImp") %>% 
    nrow()
  
  # MAE and Bias
  MAEdiff <- mean(abs(x$delta_err))
  AbsErrBiasDiff <- mean(x$delta_err)
  
  MAEdiffRCI <- x %>% 
    group_by(ReliableChange) %>% 
    summarise(MAEDiff = mean(abs(delta_err)))
  AbsErrBiasDiffRCI <- x %>% 
    group_by(ReliableChange) %>% 
    summarise(AbsBias = mean(delta_err))
  
  MAEdiffObs <- x %>% 
    group_by(obsChange) %>% 
    summarise(MAEDiff = mean(abs(delta_err)))
  AbsErrBiasDiffObs <- x %>% 
    group_by(obsChange) %>% 
    summarise(AbsBias = mean(delta_err))
  
  # classification tables
  trueVsObs <- table(x$TrueChange, x$obsChange)
  trueVsRCI <- table(x$TrueChange, x$ReliableChange)
  
  # reliability
  rxx <- rxx_empirical(x_original)
  RCI <- RCIfunc(rxx = rxx, 
                 s1 = sd(x_original %>% 
                           filter(time == 0) %>% 
                           pull(obs)))
  
  PctClassifiedRCI <- x %>% 
    filter(ReliableChange != "NoRel") %>% 
    nrow() / nrow(x)
  
  #call info
  delta <- x$delta[1]
  
  return(mget(objects(), ifnotfound = NA))
}


# can make a function to take the output and put in a combinable data frame
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
  as_tibble_row(x, 
                .name_repair = "unique")
}


plotting_func <- function(comparison.data){
  # Total Accuracy
  plotAcc <- ggplot(comparison.data, 
                    aes(x = rxx, 
                        y = ObsTotalAcc)) +
    geom_line() +
    geom_point() +
    geom_line(aes(y = RCITotalAcc), 
              linetype = "dashed", 
              color = "gray40") +
    geom_point(aes(y = RCITotalAcc), 
               color = "gray40") +
        lims(y = c(0, 1)) +
    theme_bw() +
    labs(title = "Total accuracy", 
         x = expression(r[xx]),
         y = "Total accuracy")
  
  # Type S 
  plotTypeS <- ggplot(comparison.data, 
                      aes(x = rxx, 
                          y = TypeSObs)) +
    geom_line() +
    geom_point() +
    geom_line(aes(y = TypeSRCI), 
              linetype = "dashed",              
              color = "gray40") +
    geom_point(aes(y = TypeSRCI), 
               color = "gray40") +
    lims(y = c(0, 1)) +
    theme_bw() +
    labs(title = "Type S Error", 
         x = expression(r[xx]),
         y = "Type S Error")
  
  ## RCI value
  plotRCI <- ggplot(comparison.data, 
                      aes(x = rxx, 
                          y = RCI)) +
    geom_line(linetype = "dashed",              
              color = "gray40") +
    geom_point() +
    theme_bw() +
    lims(y = c(0, NA)) +
    labs(title = "RCI value", 
         x = expression(r[xx]),
         y = "RCI value")
  
  ## percent classified
  plotClassifiedRCI <- ggplot(comparison.data, 
                              aes(x = rxx, 
                                  y = PctClassifiedRCI)) +
    geom_line(linetype = "dashed",              
              color = "gray40") +
    geom_point() +
    theme_bw() +
    lims(y = c(0, 1)) +
    labs(title = "Percent \"Reliably\" Changed", 
         x = expression(r[xx]),
         y = "Percent Classified as Reliably Changed")
  
  # group-level accuracy
  plotGrpAccDet <- comparison.data %>% 
    select(rxx, PctTrueDet, PctRelDet, PctObsDet) %>% 
    tidyr::pivot_longer(cols = !rxx, 
                 values_to = "Proportion", 
                 names_to = "Metric") %>% 
    ggplot(aes(x = rxx, 
               y = Proportion, 
               linetype = Metric, 
               shape = Metric)) +
    geom_line() +
    geom_point() +
    lims(y = c(0, 1)) +
    theme_bw() +
    theme(legend.position="bottom") +
    scale_linetype_discrete(labels = c("Observed Sign", "RCI", "True")) +
    scale_shape_discrete(labels = c("Observed Sign", "RCI", "True")) +
    labs(title = "Deterioration: Group Accuracy", 
         y = "Portion with deterioration", 
         x = expression(r[xx])) 
  
  plotGrpAccImp <- comparison.data %>% 
    select(rxx, PctTrueImp, PctRelImp, PctObsImp) %>% 
    tidyr::pivot_longer(cols = !rxx, 
                 values_to = "Proportion", 
                 names_to = "Metric") %>% 
    ggplot(aes(x = rxx, 
               y = Proportion, 
               linetype = Metric, 
               shape = Metric)) +
    geom_line() +
    geom_point() +
    lims(y = c(0, 1)) +
    theme_bw() +
    theme(legend.position="bottom") +
    scale_linetype_discrete(labels = c("Observed Sign", "RCI", "True")) +
    scale_shape_discrete(labels = c("Observed Sign", "RCI", "True")) +
    labs(title = "Improvement: Group Accuracy", 
         y = "Portion with improvement", 
         x = expression(r[xx]))

  # Sensitivity
  plotSensDet <- ggplot(comparison.data, 
                        aes(x = rxx, 
                            y = SensDetObs)) +
    geom_line() +
    geom_point() +
    geom_line(aes(y = SensDetRCI), 
              linetype = "dashed",              
              color = "gray40") +
    geom_point(aes(y = SensDetRCI), 
               color = "gray40") +
    lims(y = c(0, 1)) +
    theme_bw() +
    labs(title = "Sensitivity to Deterioration", 
         x = expression(r[xx]),
         y = "Sensitivity")
  
  plotSensImp <- ggplot(comparison.data, 
                        aes(x = rxx, 
                            y = SensImpObs)) +
    geom_line() +
    geom_point() +
    geom_line(aes(y = SensImpRCI), 
              linetype = "dashed",              
              color = "gray40") +
    geom_point(aes(y = SensImpRCI), 
               color = "gray40") +
    lims(y = c(0, 1)) +
    theme_bw() +
    labs(title = "Sensitivity to Improvement", 
         x = expression(r[xx]),
         y = "Sensitivity")
  
  # Specificity
  plotSpecDet <- ggplot(comparison.data, 
                        aes(x = rxx, 
                            y = SpecDetObs)) +
    geom_line() +
    geom_point() +
    geom_line(aes(y = SpecDetRCI), 
              linetype = "dashed",              
              color = "gray40") +
    geom_point(aes(y = SpecDetRCI), 
               color = "gray40") +
    lims(y = c(0, 1)) +
    theme_bw() +
    labs(title = "Specificity to Deterioration", 
         x = expression(r[xx]),
         y = "Specificity")
  
  plotSpecImp <- ggplot(comparison.data, 
                        aes(x = rxx, 
                            y = SpecImpObs)) +
    geom_line() +
    geom_point() +
    geom_line(aes(y = SpecImpRCI), 
              linetype = "dashed",              
              color = "gray40") +
    geom_point(aes(y = SpecImpRCI), 
               color = "gray40") +
    lims(y = c(0, 1)) +
    theme_bw() +
    labs(title = "Specificity to Improvement", 
         x = expression(r[xx]),
         y = "Specificity")
  
  # PPP
  plotPPPDet <- ggplot(comparison.data, 
                       aes(x = rxx, 
                           y = PPPDetObs)) +
    geom_line() +
    geom_point() +
    geom_line(aes(y = PPPDetRCI), 
              linetype = "dashed",              
              color = "gray40") +
    geom_point(aes(y = PPPDetRCI), 
               color = "gray40") +
    lims(y = c(0, 1)) +
    theme_bw() +
    labs(title = "PPP to Deterioration", 
         x = expression(r[xx]),
         y = "PPP")
  
  plotPPPImp <- ggplot(comparison.data, 
                       aes(x = rxx, 
                           y = PPPImpObs)) +
    geom_line() +
    geom_point() +
    geom_line(aes(y = PPPImpRCI), 
              linetype = "dashed",              
              color = "gray40") +
    geom_point(aes(y = PPPImpRCI), 
               color = "gray40") +
    lims(y = c(0, 1)) +
    theme_bw() +
    labs(title = "PPP to Improvement", 
         x = expression(r[xx]),
         y = "PPP")
  
  # NPP
  plotNPPDet <- ggplot(comparison.data, 
                       aes(x = rxx, 
                           y = NPPDetObs)) +
    geom_line() +
    geom_point() +
    geom_line(aes(y = NPPDetRCI), 
              linetype = "dashed",              
              color = "gray40") +
    geom_point(aes(y = NPPDetRCI), 
               color = "gray40") +
    lims(y = c(0, 1)) +
    theme_bw() +
    labs(title = "NPP to Deterioration", 
         x = expression(r[xx]),
         y = "NPP")
  
  plotNPPImp <- ggplot(comparison.data, 
                       aes(x = rxx, 
                           y = NPPImpObs)) +
    geom_line() +
    geom_point() +
    geom_line(aes(y = NPPImpRCI), 
              linetype = "dashed",              
              color = "gray40") +
    geom_point(aes(y = NPPImpRCI), 
               color = "gray40") +
    lims(y = c(0, 1)) +
    theme_bw() +
    labs(title = "NPP to Improvement", 
         x = expression(r[xx]),
         y = "NPP")

  return(mget(objects()))
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
  plotAcc <- ggplot(comparison.data, 
                    aes(x = delta, 
                        y = ObsTotalAcc)) +
    geom_line() +
    geom_point() +
    geom_line(aes(y = RCITotalAcc), 
              linetype = "dashed", 
              color = "gray40") +
    geom_point(aes(y = RCITotalAcc), 
               color = "gray40") +
    lims(y = c(0, 1)) +
    theme_bw() +
    labs(title = "Total accuracy", 
         x = "Mean change",
         y = "Total accuracy")
  
  # Type S 
  plotTypeS <- ggplot(comparison.data, 
                      aes(x = delta, 
                          y = TypeSObs)) +
    geom_line() +
    geom_point() +
    geom_line(aes(y = TypeSRCI), 
              linetype = "dashed",              
              color = "gray40") +
    geom_point(aes(y = TypeSRCI), 
               color = "gray40") +
    lims(y = c(0, 1)) +
    theme_bw() +
    labs(title = "Type S Error", 
         x = "Mean change",
         y = "Type S Error")
  
  ## RCI value
  plotRCI <- ggplot(comparison.data, 
                    aes(x = delta, 
                        y = RCI)) +
    geom_line(linetype = "dashed",              
              color = "gray40") +
    geom_point() +
    theme_bw() +
    lims(y = c(0, NA)) +
    labs(title = "RCI value", 
         x = "Mean change",
         y = "RCI value")
  
  ## percent classified
  plotClassifiedRCI <- ggplot(comparison.data, 
                              aes(x = delta, 
                                  y = PctClassifiedRCI)) +
    geom_line(linetype = "dashed",              
              color = "gray40") +
    geom_point() +
    theme_bw() +
    lims(y = c(0, 1)) +
    labs(title = "Percent \"Reliably\" Changed", 
         x = "Mean change",
         y = "Percent Classified as Reliably Changed")
  
  # group-level accuracy
  plotGrpAccDet <- comparison.data %>% 
    select(delta, PctTrueDet, PctRelDet, PctObsDet) %>% 
    tidyr::pivot_longer(cols = !delta, 
                 values_to = "Proportion", 
                 names_to = "Metric") %>% 
    ggplot(aes(x = delta, 
               y = Proportion, 
               linetype = Metric, 
               shape = Metric)) +
    geom_line() +
    geom_point() +
    lims(y = c(0, 1)) +
    theme_bw() +
    theme(legend.position="bottom") +
    scale_linetype_discrete(labels = c("Observed Sign", "RCI", "True")) +
    scale_shape_discrete(labels = c("Observed Sign", "RCI", "True")) +
    labs(title = "Deterioration: Group Accuracy", 
         y = "Portion with deterioration", 
         x = "Mean change") 
  
  plotGrpAccImp <- comparison.data %>% 
    select(delta, PctTrueImp, PctRelImp, PctObsImp) %>% 
    tidyr::pivot_longer(cols = !delta, 
                 values_to = "Proportion", 
                 names_to = "Metric") %>% 
    ggplot(aes(x = delta, 
               y = Proportion, 
               linetype = Metric, 
               shape = Metric)) +
    geom_line() +
    geom_point() +
    lims(y = c(0, 1)) +
    theme_bw() +
    theme(legend.position="bottom") +
    scale_linetype_discrete(labels = c("Observed Sign", "RCI", "True")) +
    scale_shape_discrete(labels = c("Observed Sign", "RCI", "True")) +
    labs(title = "Improvement: Group Accuracy", 
         y = "Portion with improvement", 
         x = "Mean change")
  
  # Sensitivity
  plotSensDet <- ggplot(comparison.data, 
                        aes(x = delta, 
                            y = SensDetObs)) +
    geom_line() +
    geom_point() +
    geom_line(aes(y = SensDetRCI), 
              linetype = "dashed",              
              color = "gray40") +
    geom_point(aes(y = SensDetRCI), 
               color = "gray40") +
    lims(y = c(0, 1)) +
    theme_bw() +
    labs(title = "Sensitivity to Deterioration", 
         x = "Mean change",
         y = "Sensitivity")
  
  plotSensImp <- ggplot(comparison.data, 
                        aes(x = delta, 
                            y = SensImpObs)) +
    geom_line() +
    geom_point() +
    geom_line(aes(y = SensImpRCI), 
              linetype = "dashed",              
              color = "gray40") +
    geom_point(aes(y = SensImpRCI), 
               color = "gray40") +
    lims(y = c(0, 1)) +
    theme_bw() +
    labs(title = "Sensitivity to Improvement", 
         x = "Mean change",
         y = "Sensitivity")
  
  # Specificity
  plotSpecDet <- ggplot(comparison.data, 
                        aes(x = delta, 
                            y = SpecDetObs)) +
    geom_line() +
    geom_point() +
    geom_line(aes(y = SpecDetRCI), 
              linetype = "dashed",              
              color = "gray40") +
    geom_point(aes(y = SpecDetRCI), 
               color = "gray40") +
    lims(y = c(0, 1)) +
    theme_bw() +
    labs(title = "Specificity to Deterioration", 
         x = "Mean change",
         y = "Specificity")
  
  plotSpecImp <- ggplot(comparison.data, 
                        aes(x = delta, 
                            y = SpecImpObs)) +
    geom_line() +
    geom_point() +
    geom_line(aes(y = SpecImpRCI), 
              linetype = "dashed",              
              color = "gray40") +
    geom_point(aes(y = SpecImpRCI), 
               color = "gray40") +
    lims(y = c(0, 1)) +
    theme_bw() +
    labs(title = "Specificity to Improvement", 
         x = "Mean change",
         y = "Specificity")
  
  # PPP
  plotPPPDet <- ggplot(comparison.data, 
                       aes(x = delta, 
                           y = PPPDetObs)) +
    geom_line() +
    geom_point() +
    geom_line(aes(y = PPPDetRCI), 
              linetype = "dashed",              
              color = "gray40") +
    geom_point(aes(y = PPPDetRCI), 
               color = "gray40") +
    lims(y = c(0, 1)) +
    theme_bw() +
    labs(title = "PPP to Deterioration", 
         x = "Mean change",
         y = "PPP")
  
  plotPPPImp <- ggplot(comparison.data, 
                       aes(x = delta, 
                           y = PPPImpObs)) +
    geom_line() +
    geom_point() +
    geom_line(aes(y = PPPImpRCI), 
              linetype = "dashed",              
              color = "gray40") +
    geom_point(aes(y = PPPImpRCI), 
               color = "gray40") +
    lims(y = c(0, 1)) +
    theme_bw() +
    labs(title = "PPP to Improvement", 
         x = "Mean change",
         y = "PPP")
  
  # NPP
  plotNPPDet <- ggplot(comparison.data, 
                       aes(x = delta, 
                           y = NPPDetObs)) +
    geom_line() +
    geom_point() +
    geom_line(aes(y = NPPDetRCI), 
              linetype = "dashed",              
              color = "gray40") +
    geom_point(aes(y = NPPDetRCI), 
               color = "gray40") +
    lims(y = c(0, 1)) +
    theme_bw() +
    labs(title = "NPP to Deterioration", 
         x = "Mean change",
         y = "NPP")
  
  plotNPPImp <- ggplot(comparison.data, 
                       aes(x = delta, 
                           y = NPPImpObs)) +
    geom_line() +
    geom_point() +
    geom_line(aes(y = NPPImpRCI), 
              linetype = "dashed",              
              color = "gray40") +
    geom_point(aes(y = NPPImpRCI), 
               color = "gray40") +
    lims(y = c(0, 1)) +
    theme_bw() +
    labs(title = "NPP to Improvement", 
         x = "Mean change",
         y = "NPP")
  
  return(mget(objects()))
}

# this computes independent linear regressions (without prespecified measurement error) on a data set.
compute_lm <- function(x){
  
  pval.data <- x %>% 
    split(.$id) %>% 
    map(~ lm(obs ~ time, data = .)) %>% 
    map(broom::tidy) %>% 
    map_dfr("p.value") %>% 
    .[2, ] %>% 
    tidyr::pivot_longer(cols = everything(), 
                 names_to = "id", 
                 values_to = "lm.p.value") %>% 
    mutate(lm.Rel = case_when(lm.p.value < .05 ~ "lmRel", 
                              TRUE ~ "lmNoRel"), 
           id = as.integer(id))
  
  slp.data <- x %>% 
    split(.$id) %>% 
    map(~ lm(obs ~ time, data = .)) %>% 
    map(broom::tidy) %>% 
    map_dfr("estimate") %>% 
    .[2, ] %>% 
    tidyr::pivot_longer(cols = everything(), 
                 names_to = "id", 
                 values_to = "lm.est") %>% 
    mutate(lmDir = case_when(lm.est < 0 ~ "lmImp",
                             lm.est > 0 ~ "lmDet",
                             TRUE ~ "lmNoChange"), 
           id = as.integer(id))
  
  full_join(x, pval.data, 
            by = "id") %>% 
    full_join(slp.data, by = "id") %>% 
    mutate(lm.95.rel = case_when(lm.Rel == "lmRel" & lmDir == "lmImp" ~ "lmRelImp", 
                                 lm.Rel == "lmRel" & lmDir == "lmDet" ~ "lmRelDet",
                                 TRUE ~ "lmNoRel"))
}

# this is the metafor::rma() function automated for our simulation:
compute_rma <- function(x){
  
  pval.data <- x %>% 
    split(.$id) %>% 
    map(~ metafor::rma.uni(obs ~ time, 
                           SEm^2, 
                           method = "FE",
                           data = ., 
                           digits = 4)) %>% 
    map(broom::tidy) %>% 
    map_dfr("p.value") %>% 
    .[2, ] %>% 
    tidyr::pivot_longer(cols = everything(), 
                 names_to = "id", 
                 values_to = "rma.p.value") %>% 
    mutate(rma.Rel = case_when(rma.p.value < .05 ~ "rmaRel", 
                               TRUE ~ "rmaNoRel"),
           id = as.integer(id))
  
  slp.data <- x %>% 
    split(.$id) %>% 
    map(~ metafor::rma.uni(obs ~ time, 
                           SEm^2, 
                           method = "FE",
                           data = ., 
                           digits = 4)) %>% 
    map(broom::tidy) %>% 
    map_dfr("estimate") %>% 
    .[2, ] %>% 
    tidyr::pivot_longer(cols = everything(), 
                 names_to = "id", 
                 values_to = "rma.est") %>% 
    mutate(rmaDir = case_when(rma.est < 0 ~ "rmaImp",
                              rma.est > 0 ~ "rmaDet",
                              TRUE ~ "rmaNoChange"), 
           id = as.integer(id))
  
  full_join(x, pval.data, 
            by = "id") %>% 
    full_join(slp.data, by = "id") %>% 
    mutate(rma.95.rel = case_when(rma.Rel == "rmaRel" & rmaDir == "rmaImp" ~ "rmaRelImp", 
                                  rma.Rel == "rmaRel" & rmaDir == "rmaDet" ~ "rmaRelDet",
                                  TRUE ~ "rmaNoRel"))
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

