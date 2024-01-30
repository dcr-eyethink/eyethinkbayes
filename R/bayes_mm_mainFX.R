bayes_mm_mainFX <- function(bmm,mainfxs=NULL,title=NULL,contrast_sparkline=F,
                            show_mpe=T,show_preddist=T,...){
  #' output plot and means table for main effects. Can be passed plotting variables for pirateye
  #' @param mainfxs vector of all main effect conditions to be plotted, defaults to all
  #' @param bmm needs a bayes mixed model stanreg object (or a list with one at start)
  #' @param title additional text for the title
  #' @param contrast_sparkline show distribution of contrast estimate below plot
  #' @param show_mpe report the mpes between pairs of mean estimates for the first names condition
  #' @param show_preddist show distributions for the mean estimates
  #' @export

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
      if(binom){m$Median <- psych::logistic(m$mean)}

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


  return(list(plot_mainfx=combined_plot_mainfx))

}
