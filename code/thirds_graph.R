thirds_graphs <- function(Y, k = 3, labels = FALSE,
                          lab_q_x = 0.95, lab_q_y = 0.95,
                          plot_title = '', df = df_full) {

  data_date <- max(as.Date(df$date_rep), na.rm = TRUE)
  df_today  <- df %>% filter(as.Date(df$date_rep) == data_date)
  
  parts <- df_today %>%
    mutate(my_group  = rank(!!sym(Y), na.last = "keep")) %>%
    dplyr::select(geoid2, my_group) %>%
    mutate(my_group = cut(my_group, breaks = k, labels = labels))
  
  ## Plot data
  
  plot_data <- left_join(df, parts) %>%
    filter(elapsed_rel > -5 & !is.na(my_group))
  
  ## Get ranges of Y within groups
  
  range_list <- lapply(levels(plot_data$my_group), function(x) {
    temp <- plot_data %>% filter(my_group == !!x) %>% 
      dplyr::select(!!Y) %>% range(na.rm = T) %>% 
      round(2)
    paste0('(', temp[1], ' to ', temp[2], ')')
  }) %>% reduce(c) %>% 
    data.frame(my_group = levels(plot_data$my_group),
               group_range = .) %>% 
    mutate(label_combined = paste(my_group, group_range)) %>% 
    mutate(label_combined = factor(label_combined, levels = label_combined))
  
  ## Add to DF
  
  plot_data <- plot_data %>% 
    left_join(range_list)
  
  ## Label data
  
  label_data <- left_join(df, parts) %>%
    filter(elapsed_rel > -5 & !is.na(my_group)) %>%
    arrange(elapsed_rel) %>%
    group_by(geoid2) %>%
    filter(row_number() == n()) %>%
    ungroup() %>% 
    group_by(my_group) %>%
    mutate(high_x = ifelse(elapsed_rel > quantile(elapsed_rel, lab_q_x, na.rm = T),
                           1, 0)) %>%
    mutate(high_y = ifelse(deaths_cum > quantile(deaths_cum, lab_q_y, na.rm = T),
                           1, 0)) %>%
    mutate(sparse = (n() < 20)) %>%
    ungroup() %>%
    filter((high_x == 1) | (high_y == 1) | sparse ==1) %>% 
    left_join(range_list)
  
  ## BW Countries
  plot_data_bw <- plot_data %>%
    filter(!(geoid2 %in% label_data$geoid2))
  
  ## Color countries
  plot_data_col <- plot_data %>%
    filter(geoid2 %in% label_data$geoid2)
  
  ## Plot
  ggplot(plot_data_bw, aes(x=elapsed_rel,
                           y=deaths_cum,
                           group = geoid2)) +
    #geom_point() +
    geom_line(color = 'grey70', alpha = 0.4) +
    geom_line(data = plot_data_col, mapping = aes(x=elapsed_rel,
                                                  y=deaths_cum,
                                                  color = geoid2)) +
    xlab("Days since 10th reported case") +
    ylab("deaths (log scale)") +
    geom_text_repel(data = label_data,
                    mapping  =aes(label = geoid2, x = elapsed_rel,
                                  y = deaths_cum, color = geoid2),
                    nudge_x = 5)  +
    facet_grid( ~ label_combined) +
    theme_bw()+
    theme(panel.grid.minor = element_blank()) +
    theme(legend.position = "none") +
    ggtitle(plot_title) +
    scale_y_continuous(trans='log10', labels = function(x) format(x, scientific = FALSE)) 
}
