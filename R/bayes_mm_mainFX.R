bayes_mm_mainFX <- function(bmm,mainfxs=NULL,binom=F,xlabpos=1.1,title=NULL,
                            mainFXcomposite=T,denplot=F,pirate_diffplot=F,...){
  #' output plot and means table for main effects. Can be passed plotting variables for pirateye
  #' @param mainfxs vector of all main effect conditions to be plotted, defaults to all
  #' @param bmm needs a bayes mixed model stanreg object (or a list with one at start)
  #' @param mainFXcomposite make a composite pirate plot of all main effects
  #' @param denplot also give a density plot of individual main effects
  #' @param pirate_diffplot give a pirate difference plot for individual main effects
  #' @export

  if (names(bmm[1])=="bmm"){bmm <- bmm$bmm}

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

  cont_plots <- list()
  other_plots <- list()

  for (cond in na.omit(mainfxs)){

    #means tables

    means <- suppressWarnings(modelbased::estimate_means(model = bmm,at=cond))
    cat("\nMain effects means: ", cond)
    print(means)
    setDT(means)
    #clipr::write_clip(means)

    if (is.factor(data[[cond]])) {
      # we have a factor variable

      cons <- modelbased::estimate_contrasts(model = bmm,contrast = cond)

      cat("\nMain effects contrasts: ", cond)
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
                                    mpes=scales::percent(cons$pd)))}


      ## gather the observed data to plot
      obsdata <- rbind(obsdata,data.frame(data[[dvname]],
                                          condition=cond,levels=data[[cond]]))

      # optional den plot
      if(denplot){
        other_plots$density <- bayes_plot(data=data,dvname = dvname,cond = cond,bfpost = bfpost,
                                          t = paste("DenplotMFX_",cond),...)    }

      # optional pirate_diffplot
      if(pirate_diffplot){
        other_plots$pirate_diff <-bayes_pirate_denplot(data=data,dvname = dvname,cond = cond,bfpost = bfpost,
                                                       t = paste("PirateplotMFX_",cond),...)    }

    }else{
      # we have a continuous variable so make a separate plot
      setnames(means,"Mean",dvname)
      slopes <- data.table(modelbased::estimate_slopes(bmm, trend = cond))
      slopes$lab <- scales::percent(slopes$pd)
      slopes <- cbind(means[.N,1:2],slopes)

      cont_plots[[cond]] <- do.call(pirateye,resolve.args(...,data=data,
                                                          pred_means =means,xlabs =slopes,pred_line = T,
                                                          dv=dvname,x_condition=cond,
                                                          line=F,violin=F,dots=T,w=4,
                                                          error_bars = F,legend=F))+facet_wrap(condcol~.)


    }

  }


  if (dim(obsdata)[1]>0){
    # we have at least some main effects factor data

    # make a composite plot
    # label variables separately
    post[,levels:=unlist(strsplit(as.character(variable),split = " "))[2],by=variable]
    post$variable <- NULL
    colnames(obsdata)[1] <- dvname
    setnames(m,"mean",dvname)

    mpe$lab <- mpe$mpes

    if (mainFXcomposite){


      # this makes sure that if duplicate arguments are passed via ...
      # those listed first will be used
      # eg there might be a data argument that was used when generating a model

      plot_mainfx <- do.call(pirateye,resolve.args(...,data = obsdata, colour_condition = "levels",xlabs = mpe,
                                                   pred = post,pred_means = m,facet_condition = "condition",
                                                   facet_scales="free_x",
                                                   dv = dvname,dots=F,error_bars = F,
                                                   title=paste(dvname,paste0(mainfxs,collapse="_"),"MainFX BayesPirate",title),
                                                   title_overide=T,
                                                   combine_plots=cont_plots))


    }}




  data_mainfx <- list(observed=data.table(obsdata),
                      predicted=data.table(post),
                      m=data.table(m),
                      mpe=data.table(mpe),
                      bcontrasts=bcontrasts)


  return(list(plot_mainfx=plot_mainfx,data_mainfx=data_mainfx,other_plots=other_plots))

}
