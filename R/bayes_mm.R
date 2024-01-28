bayes_mm <- function(bmm=list(),outp="analysis",title=NULL,plotcheck=F,...){
  #' Wrapper function for a bayesian mixed model, analysis and plots
  #' can be given an already generated model, or will run it
  #' with data and specified dvname, conditions and groups
  #' In ... can pass variables to bayes_mm_mainFX or bayes_mm_interact and pirateye for plotting
  #' @export
  #' @param bmm if not supplied, need model specs in ... if supplied just the analysis will be run
  #' @param outp folder name for output, or NULL to stop
  #' @param plotcheck plots traces to check
  #' @param title for output files and plots
  #' @param ... parameters for running a new model with \code{\link{bayes_mm_run}}, eg data, dvname, conditions, groups. Or for plotting
  #' @export

  ## if we don't have the model yet run it
  if ( length(bmm)==0) {bmm <- bayes_mm_run(...)}else{
    if (names(bmm[1])=="bmm"){bmm <- bmm$bmm}
  }

  mterms <- insight::find_terms(bmm)
  conditions <- mterms$conditional
  groups <- mterms$random
  dvname <- mterms$response

  ## report on model stats

  if (is.character(outp)){dir.create(file.path(outp),showWarnings = F)
    sink(paste0(outp,"/",dvname," ",paste(conditions,collapse = "-"),
                paste("BayesMM",title),".txt"), append=FALSE, split=TRUE)}
  cat("Bayes Mixed model on ",dvname," by ",paste(conditions,collapse = " ")," nested in ",groups)
  cat("\n================================================================================\n\n")

  # give main model analysis
  print(bmm)
  cat("\n")
  print(rstanarm::prior_summary(bmm))
  ## analyse and plot all main effects
  cat("\n")
  param <- parameters::model_parameters(bmm)
  clipr::write_clip(param)
  print(param)

  ## main effects, all of them
  #mainFX <- bayes_mm_mainFX(bmm=bmm,mainfxs = conditions,title=title,...)
  mainFX <- bayes_mm_mainFX(bmm=bmm,title=title,...)

  print(mainFX$plot_mainfx)

  #interacts plot the first condition nested inside the others
  if (length(conditions)>1){
    interacts <- unique( gsub(pattern = " ",replacement = ":",
                              x=c(paste(conditions[1],conditions[-1]),
                                  paste(c(conditions[1],conditions[-1]),collapse = " "))))

    interactFX <- bayes_mm_interact(bmm,interacts,title=title,...)
    print(interactFX)
  }else{interactFX <- NULL}

  plotcheck_plots <- list()
  if (plotcheck){
    plotcheck_plots$trace <- plot(bmm, plotfun = "trace")
    plotcheck_plots$means <- rstanarm::pp_check(bmm, plotfun = "stat", stat = "mean")
    plotcheck_plots$den <- rstanarm::pp_check(bmm, plotfun = "dens_overlay")
  }

  if (is.character(outp)){ sink()}

  return(list(bmm=bmm,mainFX=mainFX,interactFX=interactFX,plotcheck_plots=plotcheck_plots,param=param))

}
