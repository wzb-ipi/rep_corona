## Get results

results_list_wd_families <- function(data, 
                                     Y_list, 
                                     Y_labels,
                                     wd_subfamily, 
                                     measures_df = filter(measures,
                                                          wd_subfamily
                                                          %in% wd_subfamily &  
                                                            wd_include == 1),
                                     controls_use,
                                     standardize = TRUE,
                                     mc_correct = T,
                                     mc_method = 'BH',
                                     break_line_label = F,
                                     break_line_family = T,
                                     break_line_label_minlength = 18) {
  
  
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
    left_join(measures_df %>% dplyr::select(one_of(c('vars', 
                                                     'labels', 
                                                     'wd_subfamily',
                                                     'wd_subfamily_labels',
                                                     'wd_family_order',
                                                     'wd_within_order')))) %>%
    mutate(bivariate = ifelse(bivariate == 1, 'Bivariate', 'Controls')) %>%
    left_join(data.frame(Y_list, 
                         Y_labels,
                         stringsAsFactors  = F),
              by = c('Y' = 'Y_list')) %>%
    mutate(wd_subfamily_labels = str_replace(wd_subfamily_labels, "Political", "Pol."))
  
  ## MC Correction
  
  if (mc_correct) {
    res_list <- res_list %>%
      group_by(wd_subfamily) %>%
      mutate(p.value.adj = p.adjust(p.value, method = mc_method))
  }
  
  ## Label ordering
  
  order_vars <- res_list %>% ungroup() %>% 
    mutate(order_var = as.numeric(paste0(wd_family_order, wd_within_order))) %>% 
    dplyr::select(labels, order_var) %>% 
    distinct(labels, .keep_all = T) %>% 
    arrange(order_var) %>% 
    pull(labels) %>% as.character()
  
  ## Do ordering
  
  res_list <- res_list %>% 
    mutate(labels = factor(labels, levels = order_vars[length(order_vars):1]))
  
  if (break_line_family) {
    
    ## Add line breaks to labels
    
    res_list$wd_subfamily_labels <- sapply(as.character(res_list$wd_subfamily_labels),
                                           add_linebreak, 
                                           min_length = break_line_label_minlength,
                                           add_multiple_linebreaks = T)
    
  }
  
  res_list
}
