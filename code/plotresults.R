plotresults <- function(data = res_list,
                        Y_labels = unique(data$Y_labels),
                        pd = position_dodge(0.4),
                        fontsize = 11){
  
  p1 <- ggplot(data, aes(labels, estimate, bivariate)) +
    geom_hline(yintercept = 0, linetype = 'dotted') +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = bivariate),
                  width = 0, position = pd) +
    geom_point(position = pd, aes(shape = bivariate,
                                  fill = bivariate,
                                  color = bivariate)) +
    theme_bw(base_size = fontsize) +
    theme(panel.grid.minor = element_blank()) +
    theme(panel.grid.major = element_blank()) +
    facet_grid(family_labels ~ Y_labels, scales = "free", space = "free_y") +
    coord_flip() +
    scale_color_grey(name = '', labels = c('Bivariate', 'With Controls'),
                     start = 0.6, end = 0.2) +
    scale_fill_grey(name = '', labels = c('Bivariate', 'With Controls'),
                    start = 0.6, end = 0.2) +
    scale_shape_manual(name = '', labels = c('Bivariate', 'With Controls'),
                       values = c(21, 22)) +
    theme(legend.position = 'bottom') +
    theme(axis.title = element_blank(), 
          legend.box.margin=margin(-10,-10,-10,-10), legend.key.width = unit(2, 'cm'))
  
  p1
}
