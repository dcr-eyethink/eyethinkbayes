bayes_formula_gen <- function(dvname,conditions=NULL,groups,mmtype="intercept"){
  #' combines terms into formula
  #' @return Returns bayes formulae

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
