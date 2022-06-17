#####
## function to take {metafor} rma() output and produce a regression-like plot


#' Turning metafor's forest plots into regression plots with {ggplot2}
#'
#' @param x A model from the {metafor} package. Designed only to work with univariate models.
#' @param StError Numeric. Standard error of the difference score to use. 
#' Within the package should be available as the output of a call to rti_calulator()$variance
#' @param compare_lm_model Either NULL (default) or an object from lm(). 
#'
#' @return a ggplot2 object
#' @export
#'
#' @examples test <- rti_calc_simple(c(47.5, 32.5), 4.74^2)
#' forest_to_reg_plot(test$rmaObj, StError = sqrt(test$variance))
forest_to_reg_plot <- function(x, 
                               StError, 
                               compare_lm_model = NULL){
  
  # need to make it detect either predictor or intercept variable name as x.....
  # x is an output from metafor::rma.uni()
  # it has one predictor. 
  # compare_lm_model is either NULL or an object from lm().  
  xdat <- data.frame(predict(x))
  npred <- length(coef(x))  # how many predictors total?
  predictor = names(coef(x)[npred])  # take the name of the last one
  
  if(is.null(compare_lm_model)) {
    ggplot2::ggplot(xdat, 
                    ggplot2::aes(x = dplyr::case_when(ncol(data.frame(x$X)) > 1 ~ data.frame(x$X) %>% pull(predictor), 
                             TRUE ~ data.frame(x$X) %>% 
                               pull(predictor)), 
               y = pred)) + 
      ggplot2::geom_ribbon(ggplot2::aes(ymin = pred - 1.96*se, 
                      ymax = pred + 1.96*se), 
                  fill = "gray80") + 
      ggplot2::geom_line() + 
      ggplot2::geom_pointrange(ggplot2::aes(y = x$yi, 
                          ymin = x$yi - 1.96*StError, 
                          ymax = x$yi + 1.96*StError), 
                      linetype = "dashed") +
      labs(x = "time", 
           y = "outcome", 
           caption = "Error bar represents the single-timepoint RCI CI.
           For more than two time points, the shaded area represents the RTI CI.")
  }
  else {
    compare_lm_predict <- predict(compare_lm_model, se.fit =  TRUE)
    ggplot2::ggplot(xdat, 
                    ggplot2::aes(x = data.frame(x$X) %>% pull(as.name(predictor)), 
               y = pred)) + 
      ggplot2::geom_ribbon(ggplot2::aes(ymin = pred - 1.96*se, 
                      ymax = pred + 1.96*se), 
                  fill = "gray80", 
                  color = "black") + 
      ggplot2::geom_line() + 
      ggplot2::geom_pointrange(ggplot2::aes(y = x$yi, 
                          ymin = x$yi - 1.96*StError, 
                          ymax = x$yi + 1.96*StError), 
                      linetype = "dashed") +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = compare_lm_predict$fit - 1.96 * compare_lm_predict$se.fit,
                      ymax = compare_lm_predict$fit + 1.96 * compare_lm_predict$se.fit),
                  color = "blue",
                  fill = "blue",
                  alpha = .2, 
                  linetype = "dotted") +
      labs(x = "time", 
           y = "outcome", 
           caption = "Error bar represents the single-timepoint RCI CI.
           For more than two time points, the shaded area represents the RTI CI.")
  }
}

