#####
## function to take {metafor} rma() output and produce a regression-like plot


#' Turning metafor's forest plots into regression plots with {ggplot2}
#'
#' @param x A model from the {metafor} package. Designed only to work with univariate models.
#' @param StError Numeric. Standard error of the difference score to use. 
#' Defaults to `SEm` which WILL cause problems.  Within the package should be available as the output of a call to rti_calulator()$variance
#' @param compare_lm_model Either NULL (default) or an object from lm(). 
#'
#' @return a ggplot2 object
#' @export
#'
#' @examples forest_to_reg_plot(MacMeta)
forest_to_reg_plot <- function(x, 
                               StError = SEm, 
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
           aes(x = dplyr::case_when(ncol(data.frame(x$X)) > 1 ~ data.frame(x$X) %>% pull(predictor), 
                             TRUE ~ data.frame(x$X) %>% pull(predictor)), 
               y = pred)) + 
      geom_ribbon(aes(ymin = pred - 1.96*se, 
                      ymax = pred + 1.96*se), 
                  fill = "gray80") + 
      geom_line() + 
      geom_pointrange(aes(y = x$yi, 
                          ymin = x$yi - 1.96*StError, 
                          ymax = x$yi + 1.96*StError), 
                      linetype = "dashed") 
  }
  else {
    compare_lm_predict <- predict(compare_lm_model, se.fit =  TRUE)
    ggplot2::ggplot(xdat, 
           aes(x = data.frame(x$X) %>% pull(as.name(predictor)), 
               y = pred)) + 
      geom_ribbon(aes(ymin = pred - 1.96*se, 
                      ymax = pred + 1.96*se), 
                  fill = "gray80", 
                  color = "black") + 
      geom_line() + 
      geom_pointrange(aes(y = x$yi, 
                          ymin = x$yi - 1.96*StError, 
                          ymax = x$yi + 1.96*StError), 
                      linetype = "dashed") +
      geom_ribbon(aes(ymin = compare_lm_predict$fit - 1.96 * compare_lm_predict$se.fit,
                      ymax = compare_lm_predict$fit + 1.96 * compare_lm_predict$se.fit),
                  color = "blue",
                  fill = "blue",
                  alpha = .2, 
                  linetype = "dotted") 
  }
}

