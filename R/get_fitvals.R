#' @title glm_fitvals: extract fit values from glm fit object
#' 
#' @description Helper function which extracts needed parameters from glm 
#' fit object  
#'
#' @usage glm_fitvals(glm.fit, data, alpha=0.05)
#' 
#' @param alpha The threshold for CI interval, default is 0.05 (one-sided).  
#' 
#' @param offset Specify if offset term to be used, must specify log(object)  
#' 
#' @export
#' 
glm_fitvals <- function(glm.fit, data, alpha=0.05) {
  
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

#' @title gam_fitvals: extract fit values from glm fit object
#' 
#' @description Helper function which extracts needed parameters from gam 
#' fit object  
#'
#' @usage gam_fitvals(gam.fit, data, alpha=0.05)
#' 
#' @param alpha The threshold for CI interval, default is 0.05 (one-sided).  
#' 
#' @export
#' 
gam_fitvals <- function(gam.fit, data, alpha=0.05) {
  
  pred <- predict(gam.fit, 
                  newdata=data, 
                  se.fit=TRUE, 
                  type="link")
  
  fitted <- gam.fit$family$linkinv(pred$fit)
  upper <- pred$fit + (qnorm(1-alpha) * pred$se.fit)
  lower <- pred$fit - (qnorm(1-alpha) * pred$se.fit)
  upper.fit <- gam.fit$family$linkinv(upper)
  lower.fit <- gam.fit$family$linkinv(lower)
  
  res <- list(fitted = fitted, 
              upper.fit = upper.fit,
              lower.fit = lower.fit)
  return(res)
}