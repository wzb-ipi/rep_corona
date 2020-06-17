## Get results

results_list_wd_families_residuals <- function(data, 
                                               Y_list, 
                                               Y_labels,
                                               wd_subfamily, 
                                               measures_df = filter(measures,
                                                                    wd_subfamily
                                                                    %in% wd_subfamily &  
                                                                      include == 1),
                                               controls_use) {
  
  
  ## 1. Get the list of controls from controls_use
  
  for (Y in Y_list) {
    if (!(Y %in% controls_use$outcome)) {
      message(paste0('No controls for', Y, 'found in <controls_use> - will default to controls for log total deaths'))
    }
  }
  
  ## Create list of lists of controls to use 
  
  control_list <- lapply(Y_list, function(Y) {
    
    if (Y %in% controls_use$outcome) {
      controls_use %>% filter(outcome == Y) %>% 
        pull(vars) 
    } else {
      ## Use log deaths controls if no controls in DF
      controls_use %>% filter(outcome == 'deaths_cum_log') %>% 
        pull(vars) 
    }
    
  })
  
  ## Results by outcome
  
  # restrict to nonmissingness in controls  
  data_model <- filter(data, 
                       complete.cases(dplyr::select(data, 
                                                    all_of(control_list[[1]]))))
  
  ## Run model
  results_summary_residuals(data = data_model,
                  X = as.character(measures_df$vars),
                  standardize = T,
                  XX = controls_use[[1]])
}
