#' @title get_fitvals: extract fit values from glm fit object
#' 
#' @description Helper function which extracts needed parameters from glm 
#' fit object  
#'
#' @usage get_fitvals <- function(glm.fit, data)
#' 
#' @param alpha The threshold for CI interval, default is 0.05 (one-sided).  
#' 
#' @param offset Specify if offset term to be used, must specify log(object)  
#' 
#' @export
#' 
get_fitvals <- function(glm.fit, data, alpha=0.05) {
  
  pred <- predict(glm.fit, 
                  newdata=data, 
                  se.fit=TRUE, 
                  type="link")
  
  fitted <- glm.fit$family$linkinv(pred$fit)
  upper <- pred$fit + (qnorm(1-alpha) * pred$se.fit)
  lower <- pred$fit - (qnorm(1-alpha) * pred$se.fit)
  upper.fit <- glm.fit$family$linkinv(upper)
  lower.fit <- glm.fit$family$linkinv(lower)
  
  res <- list(fitted = fitted, 
              upper.fit = upper.fit,
              lower.fit = lower.fit)
  return(res)
}