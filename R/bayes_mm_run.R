bayes_mm_run <- function(data,dvname,conditions,groups,
                         f=NULL, mmtype="intercept",binom=F,
                         adapt_delta=0.8,explore_browser=F,iter=2000){


  #' Runs Bayesian hierarchical mixed model with up to three conditions
  #' and nested factors (groups).Uses rstanarm
  #' @param data data in long format
  #' @param dvname column name for DV
  #' @param conditions up to 3 conditions
  #' @param groups a set of random factors
  #' @param explore_browser launch a browser interface to explore model (uses shinystan)
  #' @param adapt_delta defults to 0.8, but can get closer to 1 if convergences probs
  #' @param iter iterations for the model to run
  #' @param f formula, if omitted this will be generated for you
  #' @param mmtype default to random intercepts, but can have 'slopes' too
  #' @param binom If true uses the binomial / logit family
  #' @return Returns the bayes model
  #' @export

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


  if(explore_browser){launch_shinystan(bmm)}

  return(bmm)

}
