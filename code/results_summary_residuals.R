results_summary_residuals <- function(data,
                            X = vars,
                            Xlab = NA,
                            XX = controls,
                            standardize = FALSE) {
  ## If standardize
  if (standardize) {
    data <- data %>%
      mutate_at(vars(one_of(X)),
                list(~./sd(., na.rm = T)))
  }
  
  out <-  lapply(X, function(x) {
    
    data_use <- filter(data,
                       complete.cases(dplyr::select(data, one_of(c(x, XX)))))
    
    rhs <- paste0(unique(c(XX)), collapse = " + ")  
    
    my_formula <- paste0(x, " ~ ",  rhs)
    
    m <- lm(as.formula(my_formula), data = data_use)
    n <- m$N
    
    ## Return residuals
    
    data.frame(resid = m$residuals,
               geoid2 = data_use$geoid2) %>% 
      mutate(X = x)
  }) 
  
  names(out) <- X
  
  ## Return
  
  out
  
}
