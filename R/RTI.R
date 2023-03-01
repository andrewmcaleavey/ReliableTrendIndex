## RTI calculators

#' Compute the RCI and RTI for a single person's observations..
#' 
#' This is essentially a wrapper for \code{metafor::rma()} with some specific behavior. 
#' It takes one or more observations with an indication of error variance, and computes
#' the RCI and/or RTI as applicable.  
#'
#' @param values Numeric. Either a single difference value, two observations, or more 
#' than two observations of the same variable for one person. While the three cases are 
#' treated separately under the hood and return slightly different values and text, 
#' the computation is the same and all involve primarily \code{metafor::rma.uni()}.
#' @param variance Numeric. The  SEm!!!!! The error variance of a given observation. 
#' NOT the Sdiff directly, but if you have
#' the Sdiff previously computed, you can divide it by the square root of 2. 
#' @param digits Integer. Number of digits to print for metafor::rma(). Defaults to 2.
#' @param cutpoint Cutpoint on z-scale to use for "reliability." Defaults to 1.96.
#' @param ... Additional arguments passed on.
#'
#' @return A list including the JT RCI value per observation in difs, 
#' A classification per observation in difs
#' @export
#'
#' @examples output <- rti_calc_simple(c(47.5, 32.5), 4.74^2)
#' output
#' metafor::forest(output$rmaObj)
#' output2 <- rti_calc_simple(c(5, 4, 3, 2), 1)
#' output2
#' metafor::forest(output2$rmaObj)
#' summary(output2$rmaObj)
#' forest_to_reg_plot(output2$rmaObj, StError = output2$variance)
#' 
rti_calc_simple <- function(values, variance, digits = 2, cutpoint = 1.96, ...){
  # if values is 2 or longer, should make sure it's a trend, otherwise the RCI works. 
  if(length(values) == 1){
    rmaobj <- metafor::rma.uni(yi = values, 
                               vi = variance, 
                               method = "FE")
    JT_rci = rmaobj$zval
    JT_rci_classification = ifelse(JT_rci > cutpoint, 
                                   "Reliable Increase", 
                                   ifelse(JT_rci < -cutpoint, 
                                          "Reliable Decrease", 
                                          "Less than reliable"))
    return(list(JT_rci = JT_rci, 
                RTI = JT_rci, 
                category.RCI = JT_rci_classification,
                category.RTI = JT_rci_classification,
                rmaObj = rmaobj,
                values = values, 
                variance = variance, 
                cutpoint = cutpoint))
  } else if(length(values) == 2) {
    difs <- values[-1] - values[1]
    values.prepost <- c(values[1], values[length(values)])
    time_linear <- c(0, 1)
    rmaobj <- metafor::rma.uni(yi = values, 
                               mods = ~ time_linear, 
                               vi  = variance, 
                               method = "FE")
    RTI = rmaobj$zval[length(rmaobj$zval)]
    RTI_classification = ifelse(RTI > cutpoint,
                                "Reliable Increase",
                                ifelse(RTI < -cutpoint,
                                       "Reliable Decrease",
                                       "Less than reliable"))
    return(list(JT_rci = RTI,
                RTI = RTI,
                category.RTI = RTI_classification,
                rmaObj = rmaobj,
                values = values,
                variance = variance,
                cutpoint = cutpoint))
  } else if(length(values) == 3){
    # case when exactly three observations
    warning("Three values provided, assuming they are evenly spaced in time and using a fixed intercept.")
    difs <- values[-1] - values[1]
    values.prepost <- c(values[1], values[length(values)])
    time_linear <- seq(from = 1, 
                       to = length(values), 
                       by = 1)
    rmaobj <- metafor::rma.uni(yi = values - vals_temp[1], 
                               mods = ~ 0 + time_linear,
                               vi = variance, 
                               intercept = FALSE, 
                               method = "FE")
    # rmaobj.rci <- metafor::rma.uni(yi = values.prepost  - values.prepost[1], 
    #                                mods = ~ c(0,1), 
    #                                vi  = variance, 
    #                                method = "FE")
    # RTI <-  rmaobj$zval[length(rmaobj$zval)]
    # RCI <- rmaobj.rci$zval[length(rmaobj.rci$zval)]
    # RTI_classification <-  ifelse(RTI > cutpoint, 
    #                             "Reliable Increase", 
    #                             ifelse(RTI < -cutpoint, 
    #                                    "Reliable Decrease", 
    #                                    "Less than reliable"))
    # return(reliableTrend(RCI = RCI,
    #                      RTI = RTI, 
    #                      category.RTI = RTI_classification,
    #                      rmaObj = rmaobj,
    #                      values = values, 
    #                      error_var = variance, 
    #                      cutpoint = cutpoint))
    # changing in the three-timepoint case
    return(reliableTrend(rmaobj))
  } else {
    # case when more than three observations
    warning("More than two values provided, assuming they are evenly spaced in time.")
    difs <- values[-1] - values[1]
    values.prepost <- c(values[1], values[length(values)])
    time_linear <- seq(from = 1, 
                       to = length(values), 
                       by = 1)
    rmaobj <- metafor::rma.uni(yi = values, 
                               mods = ~  time_linear,
                               vi = variance, 
                               method = "FE")
    rmaobj.rci <- metafor::rma.uni(yi = values.prepost  - values.prepost[1], 
                                   mods = ~ c(0,1), 
                                   vi  = variance, 
                                   method = "FE")
    RTI <-  rmaobj$zval[length(rmaobj$zval)]
    RCI <- rmaobj.rci$zval[length(rmaobj.rci$zval)]
    RTI_classification <-  ifelse(RTI > cutpoint, 
                                  "Reliable Increase", 
                                  ifelse(RTI < -cutpoint, 
                                         "Reliable Decrease", 
                                         "Less than reliable"))
    return(reliableTrend(RCI = RCI,
                         RTI = RTI, 
                         category.RTI = RTI_classification,
                         rmaObj = rmaobj,
                         values = values, 
                         error_var = variance, 
                         cutpoint = cutpoint))
  }
}

# output <- rti_calc_simple(c(47.5, 32.5), 4.74^2)
# output
# 
# rti_calc_simple(c(15, 5), 4.74^2)
# metafor::forest(output$rmaObj)
# output2 <- rti_calc_simple(c(5, 4, 3, 2), 1)
# output2
# metafor::forest(output2$rmaObj)
# summary(output2$rmaObj)
# forest_to_reg_plot(output2$rmaObj, StError = output2$variance)
# 

# now would like a way to do it for a lot of people in a long-format dataset. 
# need a data set on which to do it. 

# compute_rma <- function(data, 
#                         id_var, 
#                         obs_var,
#                         time_var, 
#                         sterror){
#   
#   pval.data <- data %>% 
#     split( {{id_var}} ) %>% 
#     purrr::map(~ metafor::rma.uni({{obs_var}} ~ {{time_var}}, 
#                            {{sterror}}^2, 
#                            method = "FE",
#                            data = ., 
#                            digits = 2)) %>% 
#     purrr::map(broom::tidy) %>% 
#     purrr::map_dfr("p.value") %>% 
#     .[2, ] %>% 
#     tidyr::pivot_longer(cols = everything(), 
#                  names_to = "id", 
#                  values_to = "rma.p.value") %>% 
#     dplyr::mutate(rma.Rel = dplyr::case_when(rma.p.value < .05 ~ "rmaRel", 
#                                TRUE ~ "rmaNoRel"),
#            id = as.integer(id))
#   
#   slp.data <- data %>% 
#     split(.$id) %>% 
#     purrr::map(~ metafor::rma.uni(obs ~ time, 
#                            SEm^2, 
#                            method = "FE",
#                            data = ., 
#                            digits = 4)) %>% 
#     purrr::map(broom::tidy) %>% 
#     purrr::map_dfr("estimate") %>% 
#     .[2, ] %>% 
#     tidyr::pivot_longer(cols = everything(), 
#                  names_to = "id", 
#                  values_to = "rma.est") %>% 
#     dplyr::mutate(rmaDir = case_when(rma.est < 0 ~ "rmaImp",
#                               rma.est > 0 ~ "rmaDet",
#                               TRUE ~ "rmaNoChange"), 
#            id = as.integer(id))
#   
#   dplyr::full_join(data, pval.data, 
#             by = "id") %>% 
#     dplyr::full_join(slp.data, by = "id") %>% 
#     dplyr::mutate(rma.95.rel = dplyr::case_when(rma.Rel == "rmaRel" & rmaDir == "rmaImp" ~ "rmaRelImp", 
#                                   rma.Rel == "rmaRel" & rmaDir == "rmaDet" ~ "rmaRelDet",
#                                   TRUE ~ "rmaNoRel"))
# }
# compute_rma(data = simulated_data, id_var = "id", obs_var = "obs_score", time_var = "index", sterror = .2)


# this is horrible coding, is brittle as hell, but works. 
compute_rti_data <- function(data, 
                             id_var, 
                             obs_var, 
                             error_value){
  ppl <- unique(data[[id_var]])
  # print(ppl)
  output <- list(rep(NA, length(ppl)))
  for(i in 1:length(ppl)){
    data_use <- data %>% 
      filter(id == ppl[i])
    # output[[i]] <- rti_calc_simple(values = data_use$obs_score, variance = .2)
    output[[i]] <- reliableTrend(metafor::rma(yi = data_use$obs_score, 
                                              vi = error_value, 
                                              method = "FE", 
                                              data = data_use))
  }
  return(output)
}
# compute_rti_data(data = simdata1, id_var = "id", obs_var = value, error_value = .5)

#' A wrapper for `metafor::rma()` with some convenient defaults for my personal use
#' 
#' Not for external use.
#'
#' @param x A data.frame, vector, or single value. If not a single value, will be
#' converted into difference scores!
#' @param error_var Variance of the error, not SD or Sdiff
#' @param observed Name of the variable used for observations in `x`. Must be a character. 
#' @param time_var Name of the variable use for time in `x`. Must be a character.
#'
#' @return A single object of class `rma`.
#' @export
#'
#' @examples 
#' # simple entry: 
#' simple_rma(15, 4.74^2)
#' simple_rma(c(47.5, 32.5), 4.74^2)
#' # Data.frame entry: 
#' simple_rma(jt_data, error_var = 4.74^2, observed = "obs", time_var = "time")
simple_rma <- function(x, error_var = .5, 
                       observed = "obs_score", 
                       time_var = NULL){
  if(is.data.frame(x)){
    difs <- x[[ observed ]][-1] - x[[ observed ]][1]
    if(!is.null(time_var)){
      time_difs <- x[[ time_var ]][-1] - x[[ time_var ]][1]
    } else {
      time_var <- seq(1, length(difs), by = 1)
    }
    output <- metafor::rma(yi = difs, 
                           vi = error_var, 
                           mods = time_var, 
                           method = "FE", 
                           intercept = FALSE)
  } else if(length(x) > 1) {
    difs <- x[-1] - x[1]
    output <- metafor::rma(yi = difs, 
                           vi = error_var, 
                           method = "FE")
  } else if(length(x == 1)){
    difs <- x
    output <- metafor::rma(yi = difs, 
                           vi = error_var, 
                           method = "FE")
  }
  return(output)
}
# simple_rma(simdata1)




# how about a function that takes a dataset and outputs another dataset with the 
# reliableTrend objects as required?
# rti_data <- function(df, error_var, 
#                      yi = obs_score)

# could write a function that does that and then instead of returning everything, 
# just returns a new data set with the key values, could be added to the existing data
# this works, sort of.
# test4 <- simulated_data %>%
#   split(.$id) %>%
#   purrr::map(~ simple_rma(., error_var = .2^2)) %>%
#   purrr::map(reliableTrend) %>%
#   purrr::map_dfr(rti_to_df) %>% 
#   mutate(id = simulated_data %>% group_by(id) %>% slice(1) %>% pull(id)) %>% 
#   right_join(simulated_data)
# 
# table(test4$category.RCI, test4$true_change)
# table(test4$category.RTI, test4$true_change)
# 
# add_rti <- function(data, id_var, obs_var, error_value, ...){
#   temp <- compute_rti_data(data = data, 
#                            id_var = id_var, 
#                            obs_var = obs_var, 
#                            error_value = error_value)
#   
#   outdata <- tibble(id = unique(data[[id_var]]), 
#                     RTI = temp[])
#   outdata
# }
# # add_rti(data = simdata1, id_var = "id", obs_var = value, error_value = .2)


#' Compute RTI in a simple way
#' 
#' Given only values, compute a reliableTrend object
#'
#' @param values Numeric. Either a vector of observed scores that contain measurement error 
#' or a single difference score. 
#' @param sdiff Numeric. Standard error of the difference score. Represents the SD of observed
#' difference scores if there is no true score change.
#' @param sem Numeric. Standard error of measurement. Represents the SD of observed scores 
#' derived from the same true score.
#' @param scale_rci Numeric. The "scale's RCI," meaning the number of scale points required
#' to change in order to declare a change is "reliable."
#' @param cutpoint Numeric, default is 1.96. Cutpoint on standard normal curve above
#' which difference scores are considered reliable. 
#' @param sd Numeric. SD to use in calculations. 
#' Required if `sdiff`, `sem`, and `scale_rci`
#' are not provided, and them must be accompanied by `rxx`. 
#' J&T suggest this is the SD of a normal 
#' population. this is probably what is done most commonly.
#' @param rxx Numeric. Reliability coefficient of the observed scores. 
#' Required if `sdiff`, `sem`, and `scale_rci`
#' are not provided, and them must be accompanied by `sd`. 
#' This should always be a test-retest reliability coefficient, not an internal consistency
#' parameter.
#'
#' @return An object of class `reliableTrend`.
#' @export
#'
#' @examples
#' rti(jt_example_data_1$obs, sdiff = 4.74)
#' rti(mac_height$obs, sdiff = .707)
#' rti(jt_example_data_1$obs, sem = 3.35)
#' rti(mac_height$obs, sem = .5)
#' rti(jt_example_data_1$obs, scale_rci = 9.2904)
#' rti(mac_height$obs, scale_rci = 1.385)
#' rti(jt_example_data_1$obs, sd = 7.5, rxx = .8)
#' rti(mac_height$obs, sd = 1.02, rxx = .77)
#' rti(15, sdiff = 4.74)
rti <- function(values, 
                sdiff = NULL, 
                sem = NULL,
                scale_rci = NULL,
                cutpoint = 1.96,
                sd = NULL, 
                rxx = NULL){
  #' Given a single difference, should add a leading 0
  if(length(values) == 1){
    values <- c(0, values)
  }
  #' need one of these:
  #' 1 sdiff
  #' 2 sem
  #' 3 scale_rci
  #' 4 sd and rxx
  if(!is.null(sdiff)){
    temp <- rti_calc_simple(values = values, 
                            variance = (sdiff / sqrt(2))^2, 
                            cutpoint = cutpoint)
    return(reliableTrend(temp$rmaObj))
  } else if(!is.null(sem)) {
    temp <- rti_calc_simple(values = values, 
                            variance = sem^2, 
                            cutpoint = cutpoint)
    return(reliableTrend(temp$rmaObj))
  } else if(!is.null(scale_rci)) {
    sdiff <- scale_rci / cutpoint
    rti(values = values, 
        sdiff = sdiff, 
        cutpoint = cutpoint)
  } else if(!is.null(sd) & !is.null(rxx)){
    sem <- sd * sqrt(1 - rxx)
    rti(values = values, 
        sem = sem, 
        cutpoint = cutpoint)
  }
}

