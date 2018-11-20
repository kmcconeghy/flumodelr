fluglm.viral <- function() {
  #VIROLOGY MODEL
  flu_form <- paste0(flu_form, 
                     "+ ", paste(viral, collapse=" + "))
  if (echo==T) {print(paste0("Model formula: ", flu_form))}
  
  ## check viral parameters are canon  
  for (i in viral) {
    if (any(sapply(data[, i], test_prop))==F) {
      warning('variable: ', i, 'exceeds boundary 0 - 1')
    }
  }
  
  #compute baseline regression 
  argslist <- list(formula=flu_form, 
                   data=data,
                   na.action = na.exclude,
                   ...)
  
  base_fit <- do.call(glm, args=argslist)  
  
  ## Fitted values + prediction interval
  dta_noviral <- data
  dta_noviral[, viral] <- sapply(dta_noviral[, viral], 
                                 function(x) x=0)
  
  
  pred <- dta_noviral %>%
    predict(base_fit, 
            newdata=., 
            se.fit=TRUE, 
            type="link")
  
  return(get_fitvals(base_fit, data))
}