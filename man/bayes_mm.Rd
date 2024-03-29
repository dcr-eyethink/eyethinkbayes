% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bayes_mm.R
\name{bayes_mm}
\alias{bayes_mm}
\title{Wrapper function for a bayesian mixed model, analysis and plots
can be given an already generated model, or will run it
with data and specified dvname, conditions and groups
In ... can pass variables to other bayes functions, and pirateye for plotting}
\usage{
bayes_mm(bmm = list(), outp = "analysis", title = NULL, plotcheck = F, ...)
}
\arguments{
\item{bmm}{if not supplied, need model specs in ... if supplied just the analysis will be run}

\item{outp}{folder name for output, or NULL to stop}

\item{title}{for output files and plots}

\item{plotcheck}{plots traces to check}

\item{...}{parameters for running a new model with \code{\link{bayes_mm_run}}, eg data, dvname, conditions, groups. Or for plotting with pirateye. Can also model with explore_browser=T}
}
\description{
Wrapper function for a bayesian mixed model, analysis and plots
can be given an already generated model, or will run it
with data and specified dvname, conditions and groups
In ... can pass variables to other bayes functions, and pirateye for plotting
}
