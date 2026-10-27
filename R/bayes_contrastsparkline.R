
bayes_contrastsparkline <- function(bcontrasts){
  #' Plot the distribution of contrast estimates
  #'
  #' Draws a small plot, with a panel for each condition, of the posterior
  #' distribution of the difference between conditions. The central 95% (the highest
  #' density interval) is shaded and zero is marked with a vertical line. It is
  #' designed to be put underneath a means plot. Used by [bayes_mm_mainFX()] and
  #' [bayes_mm_interact()].
  #'
  #' @param bcontrasts A data.table with columns `cond` (the name of the condition)
  #'   and `diff_est` (samples from the posterior distribution of the difference).
  #' @return A ggplot.
  #' @keywords internal

conden <- bcontrasts[, with(density(diff_est),data.table(x,y)),by=cond]

bfplot <- ggplot(conden,aes(x=x,y=y)) +
  geom_line(size=1,colour="dark grey") +
  theme_bw() + labs(x="",y="")+
  geom_vline(xintercept=0, color ="black", size = 1)+facet_grid(.~cond)

for (cplot in unique(conden$cond)){
  ci <- bayestestR::hdi(bcontrasts[cond==cplot]$diff_est)
  bfplot <- bfplot+geom_area(fill="lightgrey",data=conden[cplot==cond & x> ci$CI_low & x< ci$CI_high],
                   aes(x=x,y=y),alpha=0.4)
}

bfplot <-   bfplot+xlab("condition difference estimate")+theme(strip.background = element_blank(),
                                                         axis.text.y = element_text(colour = "white"),
                                                         strip.text.x = element_blank(),
                                                         panel.border =element_blank(),
                                                         panel.grid.major = element_blank(),
                                                         axis.ticks.y.left = element_blank(),
                                                         panel.grid.minor = element_blank(),
                                                        # axis.line.y.left =element_blank()
                                                         )

bfxlim <- max(abs(ggplot_build(bfplot)$layout$panel_params[[1]]$x.range))

bfplot <-  bfplot+xlim(-bfxlim,bfxlim)

# cowplot::plot_grid(plotlist = list(p,bfplot),ncol = 1,rel_heights = c(6,1),
#                    align = "v",axis="b")


return(bfplot)
}
