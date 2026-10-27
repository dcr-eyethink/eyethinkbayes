bayes_mm_mainFX <- function(bmm,mainfxs=NULL,title=NULL,contrast_sparkline=F,
                            show_mpe=T,show_preddist=T,...){
  #' Plot and report the main effects from a Bayesian mixed model
  #'
  #' For each main effect (condition) in the model, prints a table of the
  #' estimated means and contrasts, and draws a plot showing the observed data and
  #' the posterior distributions of the estimated means. Any arguments of
  #' [eyethinkdata::pirateye()] can be passed in to change the plot.
  #'
  #' @details
  #' For conditions that are factors, the plot shows the observed data (violins),
  #' the estimated mean for each level, and the posterior distribution of each
  #' estimate (the smaller violins). When a condition has two levels, the MPE
  #' (maximum probability of effect, ie the probability that the difference between
  #' the two is not zero) is written between the two levels. The MPE is the `pd`
  #' (probability of direction) from the `modelbased` package.
  #'
  #' With `contrast_sparkline = TRUE`, the distribution of the difference between
  #' conditions is drawn underneath, with the central 95% shaded in grey and zero
  #' marked by a black line. If the line is outside of the grey area, there is more
  #' than a 95% probability that the difference is not zero.
  #'
  #' For conditions that are continuous variables, a separate plot shows the
  #' predicted line, with the MPE for the slope.
  #'
  #' The text output is printed to the console. (When the function is called by
  #' [bayes_mm()] it is saved to a file.) Plots are saved as pdfs by
  #' [eyethinkdata::pirateye()]: to change where, pass `outp` in `...`
  #' (use `outp = ""` to not save them).
  #'
  #' For binomial models the means are on the probability scale.
  #'
  #' @param bmm A model from [bayes_mm_run()] (or the list returned by
  #'   [bayes_mm()]).
  #' @param mainfxs A vector of the names of the conditions to plot. By default all
  #'   of the main effects in the model are plotted.
  #' @param title Extra text for the title of the plot (and the file name).
  #' @param contrast_sparkline Show the distribution of the contrast estimate below
  #'   the plot?
  #' @param show_mpe Write the MPEs on the plot?
  #' @param show_preddist Show the posterior distributions of the means?
  #' @param ... Further arguments for [eyethinkdata::pirateye()], eg `bars = TRUE`,
  #'   `violin = FALSE`, `w` and `h` (the width and height of the saved pdf).
  #' @return A list with `plot_mainfx`, the plot (a cowplot grid, so you can also
  #'   draw it with `print()`), and `data_mainfx`, a list of the data behind it:
  #'   `observed`, `predicted` (the posterior samples), `m` (the estimated means),
  #'   `mpe` and `bcontrasts`.
  #' @export
  #' @seealso [bayes_mm()], [bayes_mm_interact()], [eyethinkdata::pirateye()]
  #' @examples
  #' \dontrun{
  #' mfx <- bayes_mm_mainFX(bmm, mainfxs = "adcon")
  #' mfx$plot_mainfx
  #' bayes_mm_mainFX(bmm, mainfxs = c("adcon", "tv"), bars = TRUE, violin = FALSE,
  #'                 contrast_sparkline = TRUE, show_mpe = FALSE)
  #' }

  if (names(bmm[1])=="bmm"){bmm <- bmm$bmm}

  binom <- ifelse ( family(bmm)$family=="binomial",T,F)

  if(is.null(mainfxs)){mainfxs <- insight::find_terms(bmm)$conditional}

  data <- bmm$data
  dvname <- insight::find_terms(bmm)$response

  plot_mainfx <- NULL
  data_mainfx <- NULL
  bcontrasts <- data.table()
  m <- data.frame()
  mpe <- data.frame()
  obsdata <- data.frame()
  post <- data.table()

  all_plots <- list()

  for (cond in na.omit(mainfxs)){

    #means tables

    means <- suppressWarnings(modelbased::estimate_means(model = bmm,at=cond))

    cat("\nMain effects means: ", cond,"\n")
    print(means)
    setDT(means)
    cat("\n")

    if (binom){setnames(means,"Probability","Mean")}

    if (is.factor(data[[cond]])) {
##########################################
      # we have a factor variable
##########################################
      cons <- modelbased::estimate_contrasts(model = bmm,contrast = cond)

      cat("\nMain effects contrasts: ", cond,"\n")
      print(cons)
      cat("\n")

      condpost <- emmeans::emmeans(bmm,specs = cond,data=data)
      ## Get the distribution of the condition differences
      constrast_bfpost <- pairs(condpost)
      bfpost <- data.frame(as.matrix(emmeans::as.mcmc.emmGrid(constrast_bfpost)))
      if (binom){bfpost <- psych::logistic(bfpost)}

      bcontrasts <- rbind(bcontrasts,data.table(cond=cond,diff_est=bfpost[,1]))

      # now get dist of differnet levels
      condpost<-data.table(as.matrix(emmeans::as.mcmc.emmGrid(condpost)))
      if (binom){condpost <- data.table(psych::logistic(condpost))}
      condpost <- melt(condpost,measure.vars = colnames(condpost),value.name = dvname)
      post <- rbind(post,data.frame(condition=cond,condpost))

      ## gather mean estimates
      m <- rbind(m,data.frame(condition=cond,levels=means[[cond]],mean=means$Mean))
     # if(binom){m$Median <- psych::logistic(m$mean)}

      ## if there are only 2 levels, then we can gather mpes to put on a plot
      if (length(unique(data[[cond]]))<3){
        mpe <- rbind(mpe,data.frame(condition=cond,
                                    mpes=scales::percent(cons$pd)))

        }


      ## gather the observed data to plot
      obsdata <- rbind(obsdata,data.frame(data[[dvname]],
                                          condition=cond,levels=data[[cond]]))


    }else{
      ##########################################
      # we have a continuous variable
      ##########################################

      # make a separate plot
      setnames(means,"Mean",dvname)
      slopes <- NULL
     if (show_mpe){
       slopes <- data.table(modelbased::estimate_slopes(bmm, trend = cond))
      slopes$lab <- scales::percent(slopes$pd)
      slopes <- cbind(means[.N,1:2],slopes)
}
      all_plots[[cond]] <- do.call(pirateye,resolve.args(data=data,...,
                                                          pred_means =means,xlabs =slopes,pred_line = T,
                                                          dv=dvname,x_condition=cond,
                                                          line=F,violin=F,dots=T,w=4,
                                                          error_bars = F,legend=F))+facet_wrap(condcol~.)


    }

  }

# we've gone through all the mfxs, and have a compiled data set of factor data to plot together
# and maybe a continuous plot too

  if (dim(obsdata)[1]>0){
    # we have at least some main effects factor data

    # make a composite plot
    # label variables separately
    if (show_preddist){
    setDT(post)

    post[,levels:=tstrsplit(variable,split=" ")[2],by=variable]

    post$variable <- NULL
    post$condition <- as.factor( post$condition)
    post$levels <- as.factor( post$levels)

    }else{post <- NULL}

    obsdata$condition <- as.factor( obsdata$condition)
    obsdata$levels <- as.factor( obsdata$levels)

    colnames(obsdata)[1] <- dvname
    setnames(m,"mean",dvname)


    if (dim(mpe)[1]==0 | !(show_mpe)){mpe <- NULL}else{mpe$lab <- mpe$mpes}



      plot_mainfx <- do.call(pirateye,resolve.args(data = obsdata, colour_condition = "levels",
                                                   pred = post,pred_means = m,facet_condition = "condition",
                                                   facet_scales="free_x",xlabs = mpe,
                                                   dv = dvname,
                                                   ...,
                                                   dots=F,error_bars = F,
                                                   title=paste(dvname,paste0(mainfxs,collapse="_"),"MainFX BayesPirate",title),
                                                   title_overide=T))

     # do we want a sparkline for the contrasts?

      if (contrast_sparkline){
      # check to see if we have exlcusively 2 level conditions, otherwise this isn't going to work
      bcplot <-  bayes_contrastsparkline(bcontrasts = bcontrasts)
      plot_mainfx <-  cowplot::plot_grid(plotlist = list(plot_mainfx,bcplot),ncol = 1,rel_heights = c(6,1),
                                              align = "v",axis="b")
      }

      all_plots <- append(list(plot_mainfx=plot_mainfx),all_plots)

  }else{
      # no further data so return continuous plot alone

    }

### combine  plot_mainfx and cont_plots (if we have them)

  combined_plot_mainfx <- cowplot::plot_grid(plotlist =  all_plots,
                                             nrow = 1,align = 'v',axis = "l")



  data_mainfx <- list(observed=data.table(obsdata),
                      predicted=data.table(post),
                      m=data.table(m),
                      mpe=data.table(mpe),
                      bcontrasts=bcontrasts)


  return(list(plot_mainfx=combined_plot_mainfx,data_mainfx=data_mainfx))

}
