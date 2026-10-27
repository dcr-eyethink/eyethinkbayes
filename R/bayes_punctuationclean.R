bayes_punctuationclean <- function(dt=NULL,cvec,subpat="[[:punct:]|[:space:]]"){
  #' Remove punctuation from column names and levels
  #'
  #' Replaces punctuation and spaces with underscores, in the names of the
  #' conditions and the levels of any that are factors or text, which makes the
  #' models and plots simpler to work with. Used by [bayes_mm_run()].
  #'
  #' @param dt A data.table. If it is missing, then `cvec` is cleaned and returned.
  #' @param cvec The names of the columns to clean.
  #' @param subpat The pattern (a regular expression) to replace.
  #' @return The cleaned data.table (with text columns turned into factors), or if
  #'   `dt` is missing the cleaned `cvec`.
  #' @keywords internal
  #' @examples
  #' eyethinkbayes:::bayes_punctuationclean(cvec = c("Ad condition", "tv-choice"))

  if (is.null(dt)){
    return(gsub(subpat, "_",cvec))
  }else{
 for (ci in cvec){
 if (ci %in% colnames(dt)){
   if (class(dt[[ci]])=="factor" | class(dt[[ci]])=="character"){
   # clean the levels in cols named by cvec, and make into a factor
    dt[[ci]] <- gsub(subpat, "_",as.character(dt[[ci]]))
   dt[[ci]] <- as.factor(dt[[ci]] )
   setnames(dt,old = ci,new=gsub(subpat, "_",ci))
   }
 }else{
   cat("\n",ci," is not a column name in that data")
   }
  }
  return(dt)
  }

}
