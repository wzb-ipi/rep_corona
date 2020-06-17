## Get results

results_list <- function(data, 
                         Y_list, 
                         Y_labels,
                         family, 
                         family_labels,
                         family_selection = family,
                         measures_df = filter(measures,
                                              family %in% family_selection &  
                                                include == 1),
                         controls_use,
                         standardize = TRUE,
                         mc_correct = T,
                         mc_method = 'BH',
                         break_line_label = F) {
  
  
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
  
  res_list <- mapply(function(Y, con) {
    lapply(c(TRUE,FALSE), function(bivariate) {
      
      # restrict to nonmissingness in controls  
      data_model <- filter(data, complete.cases(dplyr::select(data, all_of(con))))
      
      ## Run model
      results_summary(data = data_model,
                      Y = Y,
                      X = as.character(measures_df$vars),
                      standardize = standardize,
                      XX = con,
                      bivariate = bivariate) %>%
        mutate(bivariate = ifelse(bivariate, 1, 0))
    }) %>%
      reduce(rbind) %>%
      mutate(Y = Y)},
    Y_list,
    control_list, 
    SIMPLIFY = F) %>% 
    reduce(rbind) %>%
    rename(vars = term) %>%
    left_join(measures_df %>% dplyr::select(vars, labels, family)) %>%
    mutate(bivariate = ifelse(bivariate == 1, 'Bivariate', 'Controls')) %>%
    left_join(data.frame(Y_list, Y_labels,
                         stringsAsFactors  = F),
              by = c('Y' = 'Y_list')) %>%
    left_join(data.frame(family, family_labels,
                         stringsAsFactors  = F)) %>% 
    mutate(family_labels = str_replace(family_labels, "Political", "Pol."))
  
  ## MC Correction
  
  if (mc_correct) {
    res_list <- res_list %>%
      group_by(family) %>%
      mutate(p.value.adj = p.adjust(p.value, method = mc_method))
  }
  
  if (break_line_label) {
    
    ## Add line breaks to labels
    
    res_list$labels <- sapply(as.character(res_list$labels),
                              add_linebreak, min_length = 18)
    
  }
  
  res_list
}
