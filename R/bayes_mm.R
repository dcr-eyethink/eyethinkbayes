bayes_mm <- function(bmm=list(),outp="analysis",title=NULL,plotcheck=F,...){
  #' Run, analyse and plot a Bayesian mixed model in one step
  #'
  #' A wrapper that runs a Bayesian mixed model (or takes one you have already
  #' run), prints a report of the model, and then plots all of the main effects
  #' and all of the interactions. It is the quickest way to analyse a data set; the
  #' functions it calls ([bayes_mm_run()], [bayes_mm_mainFX()] and
  #' [bayes_mm_interact()]) can also be used on their own, for more control.
  #'
  #' @details
  #' **The report.** The model, its priors, and a table of parameters (from
  #' `parameters::model_parameters`) are printed to the console, followed by the
  #' means and contrasts for the main effects and interactions. If `outp` is a
  #' folder name, the same text is also saved to a file in that folder called
  #' `<dv> <conditions>BayesMM <title>.txt`.
  #'
  #' **The plots.** All the main effects are plotted by [bayes_mm_mainFX()]. If
  #' there is more than one condition, the interactions are plotted by
  #' [bayes_mm_interact()]: the first condition with each of the others, and then
  #' all of them together. The plots are drawn by [eyethinkdata::pirateye()], and are
  #' saved as pdfs in the `outp` folder.
  #'
  #' **Passing arguments.** If you supply `bmm`, everything in `...` goes to the
  #' plotting functions, so you can use any argument of [bayes_mm_mainFX()],
  #' [bayes_mm_interact()] or [eyethinkdata::pirateye()] (eg `bars = TRUE`). If you
  #' want [bayes_mm()] to run the model for you, then `...` must contain the
  #' arguments for [bayes_mm_run()] (`data`, `dvname`, `conditions`, `groups` and so
  #' on); these are also passed to the plotting functions, which ignore them.
  #' Because [bayes_mm_run()] does not accept plotting arguments, it is best to
  #' run the model first with [bayes_mm_run()] if you want to customise the plots.
  #'
  #' @param bmm A model that you have already run, either the output of
  #'   [bayes_mm_run()] or the list returned by this function. If it is not given
  #'   (the default), a new model is run using the arguments in `...`.
  #' @param outp The name of a folder in which to save the text report and the
  #'   plots as pdfs (it is created if needed). Set to `NULL` or "" to not save
  #'   anything.
  #' @param title Extra text to add to the plot titles and the report file name.
  #' @param plotcheck If `TRUE`, also return plots to check the model: the traces
  #'   of the chains, a posterior check of the mean, and a posterior check of the
  #'   density of the data.
  #' @param ... Arguments for [bayes_mm_run()] (`data`, `dvname`, `conditions`,
  #'   `groups`, `binom`, `iter` etc.) if a model is to be run, and/or for
  #'   [bayes_mm_mainFX()], [bayes_mm_interact()] and [eyethinkdata::pirateye()] for
  #'   plotting.
  #' @return A list with:
  #' \describe{
  #'   \item{bmm}{the model (a stanreg object), which you can pass back to the
  #'   plotting functions}
  #'   \item{mainFX}{output of [bayes_mm_mainFX()]}
  #'   \item{interactFX}{the plots from [bayes_mm_interact()], or `NULL` if there
  #'   was only one condition}
  #'   \item{plotcheck_plots}{the check plots, if `plotcheck = TRUE`}
  #'   \item{param}{the table of model parameters}
  #' }
  #' @export
  #' @seealso [bayes_mm_run()], [bayes_mm_mainFX()], [bayes_mm_interact()]
  #' @examples
  #' \dontrun{
  #' res <- bayes_mm(data = ad_data, dvname = "rating",
  #'                 conditions = c("adcon", "dim", "tv"), groups = "pid",
  #'                 plotcheck = TRUE)
  #' # replot, using the model that was returned
  #' bayes_mm_interact(res, interacts = "adcon:dim:tv", violin = FALSE)
  #' }

  ## if we don't have the model yet run it
  if ( length(bmm)==0) {bmm <- bayes_mm_run(...)}else{
    if (names(bmm[1])=="bmm"){bmm <- bmm$bmm}
  }

  mterms <- insight::find_terms(bmm)
  conditions <- mterms$conditional
  groups <- mterms$random
  dvname <- mterms$response

  ## report on model stats

  nsink <- sink.number()
  on.exit(while (sink.number() > nsink) sink())   # never leave output diverted if there is an error
  saving <- is.character(outp) && nzchar(outp)
  # (a NULL would be dropped on its way to pirateye, so "" is used to mean do not save)
  outp_plots <- if (is.null(outp)){""}else{outp}
  if (saving){dir.create(file.path(outp),showWarnings = F)
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
  # clipr::write_clip(param)
  print(param)

  ## main effects, all of them
  #mainFX <- bayes_mm_mainFX(bmm=bmm,mainfxs = conditions,title=title,...)
  mainFX <- bayes_mm_mainFX(bmm=bmm,title=title,outp=outp_plots,...)

  print(mainFX$plot_mainfx)

  #interacts plot the first condition nested inside the others
  if (length(conditions)>1){
    interacts <- unique( gsub(pattern = " ",replacement = ":",
                              x=c(paste(conditions[1],conditions[-1]),
                                  paste(c(conditions[1],conditions[-1]),collapse = " "))))

    interactFX <- bayes_mm_interact(bmm,interacts,title=title,outp=outp_plots,...)
    print(interactFX)
  }else{interactFX <- NULL}

  plotcheck_plots <- list()
  if (plotcheck){
    plotcheck_plots$trace <- plot(bmm, plotfun = "trace")
    plotcheck_plots$means <- rstanarm::pp_check(bmm, plotfun = "stat", stat = "mean")
    plotcheck_plots$den <- rstanarm::pp_check(bmm, plotfun = "dens_overlay")
  }

  if (saving){ sink()}

  return(list(bmm=bmm,mainFX=mainFX,interactFX=interactFX,plotcheck_plots=plotcheck_plots,param=param))

}
