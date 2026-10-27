bayes_formula_gen <- function(dvname,conditions=NULL,groups,mmtype="intercept"){
  #' Make a model formula
  #'
  #' Combines the names of the dv, the conditions and the grouping factors into a
  #' model formula, as text. Used by [bayes_mm_run()].
  #'
  #' @param dvname The name of the dependent variable.
  #' @param conditions The names of the conditions, which are crossed (`a * b`).
  #' @param groups The names of the grouping factors, each of which gets a random
  #'   effect.
  #' @param mmtype "intercept" for random intercepts, "slopes" for random slopes by
  #'   the conditions as well.
  #' @return The formula as text, eg "rating ~ adcon * tv + (1 | pid)".
  #' @keywords internal
  #' @examples
  #' eyethinkbayes:::bayes_formula_gen("rating", c("adcon", "tv"), "pid")

  f <- paste(dvname,"~", paste(conditions,collapse =" * "))
  g <- paste(conditions,collapse ="+")

  for (gv in groups){
    if (mmtype=="intercept"){
      f <-  paste(f,"+ (1 |",gv,")")

    }else{
      f <-   paste(f,"+ (1 + ",g,"|",gv,")")
    }
  }

  return(f)
}
