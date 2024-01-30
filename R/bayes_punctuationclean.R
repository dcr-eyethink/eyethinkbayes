bayes_punctuationclean <- function(dt=NULL,cvec,subpat="[[:punct:]|[:space:]]"){
  #' cleans dt of punctuation from levels and colnames specified in cvec, replacing with _
  #' or cvec itself if no dt

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
