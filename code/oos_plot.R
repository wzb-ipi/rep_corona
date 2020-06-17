oos_plot <- function(
  data = df_today,
  depvar = "deaths_cum_log",
  controls,
  ylabel = "Actual deaths per capita") {
  

  oos_df(data, depvar, controls, ylabel) %>%
    ggplot(aes(exp(pred), exp(actual))) +   
    geom_point(alpha = 0.5, size = 2) +
    xlab("Prediction based on experiences elsewhere")+
    ylab(ylabel) +
    geom_abline(slope = 1, intercept = 0, linetype = 'dotted') +
    theme_bw() +  
    theme(panel.grid.minor = element_blank()) +
    ggrepel::geom_text_repel(data = . %>% filter(do_label == 1),
                             aes(label = cases)) +
    scale_x_continuous(trans='log10') +
    scale_y_continuous(trans='log10') 
    

}



oos_df <- function(
  data = df_today,
  depvar = "deaths_cum_log",
  controls,
  ylabel = "Actual deaths (logged)") {
  
  formula = paste(depvar, "~", paste(controls, collapse = "+"))
  
  pred <- function(case, data) {
    
    M <- lm(as.formula(formula), data = filter(data, geoid2 != case))
    
    out <- c(actual = filter(data, geoid2 == case) %>% pull(depvar),
             prediction = predict(M, filter(data, geoid2 == case), se.fit = FALSE))
    
    if(!any(is.na(out))) out else c(NA, NA)
  }
  
  cbind(
    cases = unique(as.character(data$geoid2)),
    sapply(unique(as.character(data$geoid2)), function(case) (pred(case, data))) %>% t %>% data.frame()) %>%
    filter(!is.na(prediction.1)) %>%
    rename(pred = prediction.1) %>%
    mutate(resid_abs = abs(pred-actual)) %>%
    mutate(do_label = ifelse(resid_abs > quantile(resid_abs, 0.85)  
                             | actual >  quantile(actual, 0.9) 
                             | pred > quantile(pred, 0.9)
                             | cases == "UGA"
                             | cases == "LSO",
                             1, 0))
}
  
  