short_table <- function(Y, 
                        Y_label, 
                        controls_use, 
                        caption = "",
                        note = "",
                        mc_correct = TRUE) {
  
  res <- results_list(data = df_today,
                      Y_list = Y,
                      Y_labels = Y_label,
                      family=family,
                      family_labels=family_labels,
                      controls_use=controls_use,
                      mc_correct = mc_correct) %>%
    ungroup()  %>%
    arrange(family_labels) %>%  
    dplyr::select(labels, estimate, matches('p.value'),
                  n, Y_labels, bivariate, family_labels) %>%
    filter(!bivariate == 'Bivariate') %>%
    dplyr::select(-bivariate) %>%
    mutate_at(vars(labels),
              list(~str_replace(., '\n', ' ')))%>%
    mutate_at(vars(labels),
              list(~str_replace(., '%', 'Perc.'))) %>%
    left_join(measures %>% dplyr::select(labels))
  
  ## Get the groupings for tables
  
  gr <- res %>% ungroup() %>%
    mutate(rowid = 1:n()) %>%
    group_by(family_labels) %>%
    summarise(first = first(rowid),
              last = last(rowid)) %>%
    data.frame(stringsAsFactors = F)
  
  ## Table ##
  
  res %>%
    dplyr::select(labels, estimate, matches('p.value'),
                  n) %>%
    #mutate(p.value.adj = round(p.value.adj, 2)) %>%
    #mutate(p.value.adj = cell_spec(p.value.adj,
    #                               "latex",
    #                               bold = ifelse(p.value.adj < 0.05, T, F))) %>%
    kable(format = "latex",
          align = c(rep('l', 1), rep('l', 1), rep('r', 4)),
          booktabs = TRUE,
          escape = F,
          caption = caption,
          col.names = c("Variable", "Estimate",
                        "P", "P (adj.)",
                        "N"), digits = 2) %>%
    column_spec(1, width = '5cm') %>%
    kable_styling(font_size = 8, full_width = FALSE, latex_options = c("hold_position",
                                                                       "repeat_header")) %>%
    footnote(general = note, threeparttable = T) %>%
    pack_rows(gr[1, 'family_labels'], gr[1, 'first'], gr[1, 'last'],
              hline_after = T) %>%
    pack_rows(gr[2, 'family_labels'], gr[2, 'first'], gr[2, 'last'],
              hline_after = T) %>%
    pack_rows(gr[3, 'family_labels'], gr[3, 'first'], gr[3, 'last'],
              hline_after = T) %>%  
    pack_rows(gr[4, 'family_labels'], gr[4, 'first'], gr[4, 'last'],
              hline_after = T) %>%
    pack_rows(gr[5, 'family_labels'], gr[5, 'first'], gr[5, 'last'],
              hline_after = T) %>%
    pack_rows(gr[6, 'family_labels'], gr[6, 'first'], gr[6, 'last'],
              hline_after = T) %>%
    pack_rows(gr[7, 'family_labels'], gr[7, 'first'], gr[7, 'last'],
              hline_after = T)
}
