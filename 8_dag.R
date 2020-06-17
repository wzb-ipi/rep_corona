
# Draw DAG

f_dag <- function(x,
                   y,
                   names,
                   arcs = cbind(0,0),
                   add_points = FALSE,
                   solids = rep(1, length(x)),
                   title = "",
                   contraction = .1,
                   add_functions = 0,
                   add_functions_text = NULL,
                   text_shift = .2*add_points,
                   padding = .5,
                   length = 0.2,
                   cex = 1,
                   box = TRUE,
                  font = NULL) {
  if(add_points)  plot(x, y, pch=ifelse(solids == 1, 19, 1), cex = 2, axes = FALSE, xlab = "", ylab = "",
                       xlim = c(min(x)-padding, max(x)+padding),
                       ylim = c(min(y)-padding-add_functions, max(y)+padding),
                       main = title)
  if(!add_points)  plot(x, y, type = "n", cex = 2, axes = FALSE, xlab = "", ylab = "",
                        xlim = c(min(x)-padding, max(x)+padding),
                        ylim = c(min(y)-padding-add_functions, max(y)+padding),
                        main = title)
  arrow_length = ((x[arcs[,1]] - x[arcs[,2]])^2 + (y[arcs[,1]] - y[arcs[,2]])^2)^.5
  arrow_length <-  arrow_length/max(arrow_length)
  contraction  <- (1-3*arrow_length/4)*contraction
    
  arrows(x[arcs[,1]]*(1-contraction) + x[arcs[,2]]*contraction,
         y[arcs[,1]]*(1-contraction) + y[arcs[,2]]*contraction,
         x[arcs[,2]]*(1-contraction) + x[arcs[,1]]*contraction,
         y[arcs[,2]]*(1-contraction) + y[arcs[,1]]*contraction, length = length)
  text(x, y + text_shift, names, cex = cex, font = font)
  if(!is.null(add_functions_text)) text(((min(x)+max(x))/2), min(y)-1, add_functions_text)
  if(box) box()
}

dagdf <- data.frame(
      x = c(1,1,1,2,  3, 4.5,4,6.5, 5.5,5,6,7),
      y = c(1,2,3,1.5,2, 3,  2,3,   3,  2,2,1.5),
      names = c("4. Social\nStructures",
                "2. Political\nInstitutions",
                "1. State\nCapacity",
                "3. Political\nPriorities",
                "Policies",
                "5. Global Linkages \n 6. Environmental risks",
                "Behavior\nChange",
                "8. Health\nSystems",
                "7. Health \n Risks",
                "Infections",
                "Deaths",
                "Reported\nDeaths"),
      position = 1:12,
      font = c(rep(2,4),rep(1, 8)))

dagdf


# jpeg("figs/dag_wd.jpg", height  = 700, width = 1400)
f_dag(dagdf$x, dagdf$y, dagdf$names, font = dagdf$font, 
      arcs = rbind(c(1,4),
                   c(1,10),
                   c(1,7),
                   c(2,4),
                   c(3,5),
                   c(3,7),
                   c(4,5),
                   c(5,7),
                   c(6,10),
                   c(7,10),
                   c(8,11),
                   c(8,12),
                   c(9,11),
                   c(10,11),
                   c(4,12),
                   c(11,12)),
      box = FALSE,
      padding = .05,
      contraction = .2,
      cex = 1.2
)

# dev.off()