results_summary <- function(data,
                            Y,
                            X = vars,
                            Xlab = NA,
                            XX = controls,
                            bivariate = FALSE,
                            standardize = FALSE) {
  ## If standardize
  if (standardize)
    data <- data %>%
      mutate_at(vars(one_of(X)),
                list(~./sd(., na.rm = T)))
  
  out <-  lapply(X, function(x) {
    
    rhs <- ifelse(bivariate, x, paste(unique(c(x, XX)), collapse = " + "))  
    
    my_formula <- paste0(Y, " ~ ",  rhs)
    
    m <- lm_robust(as.formula(my_formula), data = data)
    n <- m$N
    
    ## Return
    
    m %>% tidy() %>% slice(2) %>%
      mutate(n = n)
  }) %>%
    bind_rows()  %>%                   
    dplyr::select(term, estimate, conf.low, conf.high, p.value, n)
  
  
  ## Check if variable labels are desired
  ## If yes, recode the term variable
  
  if (!is.na(Xlab[1])) {
    tempdf <- data.frame(term = X, label = Xlab)
    out <- left_join(out, tempdf) %>%
      mutate(term = label)
  }
  
  ## Return
  
  out
}
