bayes_mm_run <- function(data,dvname,conditions,groups,
                         f=NULL, mmtype="intercept",binom=F,
                         adapt_delta=0.8,explore_browser=F,iter=2000){


  #' Run a Bayesian hierarchical mixed model
  #'
  #' Runs a Bayesian mixed model with up to three conditions as fixed effects and
  #' any number of grouping factors (such as participants) as random effects,
  #' using the `rstanarm` package. You give it the names of the columns and it
  #' writes the model formula for you. This can take a few minutes.
  #'
  #' @details
  #' All of the conditions are crossed (`a * b * c`), and a random effect is added
  #' for each of the groups. With the default `mmtype = "intercept"` each group
  #' gets a random intercept (`(1 | pid)`); with "slopes" each group also gets
  #' random slopes for the conditions (`(1 + a + b | pid)`).
  #'
  #' To avoid problems later on, punctuation and spaces in the names of the
  #' conditions, and in the levels of any condition that is a factor or text, are
  #' replaced by underscores (so "Not addressable" becomes "Not_addressable"), and
  #' text columns are turned into factors. This is done to a copy of your data, which
  #' is stored in the model (`bmm$data`). The `conditions` are cleaned in the same
  #' way, so when you ask for plots, use the cleaned names.
  #'
  #' The model is run with the default priors of `rstanarm::stan_glmer`. The
  #' function also sets the R option `mc.cores` to the number of cores on your
  #' computer, so that the chains run in parallel.
  #'
  #' @param data The data in long format, with a row for each observation.
  #' @param dvname The name of the column with the dependent variable.
  #' @param conditions A vector of up to three names of columns, the fixed effects.
  #'   Factors (or text) and numeric variables can be used.
  #' @param groups A vector of names of columns with the grouping factors for the
  #'   random effects, eg "pid".
  #' @param f A model formula (as text) to use instead of making one from
  #'   `conditions` and `groups`.
  #' @param mmtype "intercept" (the default) for random intercepts, or "slopes" to
  #'   have random slopes too.
  #' @param binom If `TRUE`, the dv is modelled as binomial, with a logit link
  #'   (for 0/1 data). The default is a normal (gaussian) model.
  #' @param adapt_delta Defaults to 0.8. Raise it towards 1 if the model has
  #'   convergence problems (it is slower).
  #' @param explore_browser If `TRUE`, open a browser window to explore the model
  #'   with `shinystan`.
  #' @param iter The number of iterations for each chain. Default 2000.
  #' @return The model, a stanreg object from `rstanarm`. Pass it to
  #'   [bayes_mm()], [bayes_mm_mainFX()] or [bayes_mm_interact()].
  #' @export
  #' @seealso [bayes_mm()]
  #' @examples
  #' \dontrun{
  #' bmm <- bayes_mm_run(data = ad_data, dvname = "dprime",
  #'                     conditions = c("adcon", "age", "tv"),
  #'                     groups = c("pid", "dim"))
  #' }

  print("running model")
  options(mc.cores = parallel::detectCores())

  ### clean the variables and data from punctuation in column names and levels in data
  ### and in condition names
  ### cos it's too much heartache

  data <- bayes_punctuationclean(dt = data,cvec = conditions)
  conditions <- bayes_punctuationclean(cvec = conditions)

  if (is.null(f)){   f <- bayes_formula_gen(dvname,conditions,groups,mmtype) }

  # for (c in conditions){
  #   if ("character" %in% class(data[[c]])){data[[c]] <- as.factor(data[[c]])}
  # }


  if (binom){fam <-  binomial(link = "logit")}else{fam <- gaussian()}

  bmm <- rstanarm::stan_glmer	(formula = f,data=data,iter=iter,
                               control = list(adapt_delta = adapt_delta),family =fam)


  if(explore_browser){shinystan::launch_shinystan(bmm)}

  return(bmm)

}
