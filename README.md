README eyethink bayes
================
2024-01-23

## Eyethink Bayes analysis tools

This is a toolbox for running Bayesian mixed models. It is part of the eyethink toolbox and requires the [eyethinkdata package](https://dcr-eyethink.github.io/eyethinkdata/).

It will run a Baysian mixed model using rstanarm, output metrics and plots for the main effects and interactions. 

Use the code below to install or update from github

``` r
devtools::install_github("dcr-eyethink/eyethinkbayes")
library(eyethinkbayes)
```

There is a vignette walk through using the package and some of its
functions. You can access this in R with

``` r
vignette("eyethinkbayes_vignette",package = "eyethinkbayes")
```

You can also read this vignette [online at the github website](https://dcr-eyethink.github.io/eyethinkbayes/articles/eyethinkbayes_vignette.html).
