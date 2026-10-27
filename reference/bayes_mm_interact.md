# Plot and report the interactions from a Bayesian mixed model

For each interaction you ask for, prints a table of the estimated means
and contrasts, and draws a plot of the estimated means with their
posterior distributions. Works for interactions between factors, and for
interactions between one continuous variable and one or two factors. Any
arguments of
[`eyethinkdata::pirateye()`](https://dcr-eyethink.github.io/eyethinkdata/reference/pirateye.html)
can be passed in to change the plot.

## Usage

``` r
bayes_mm_interact(
  bmm,
  interacts = NULL,
  title = NULL,
  pred_values = NULL,
  ribbon = T,
  ladder = F,
  spark = F,
  slope_mpes = T,
  mfx_sideplot = F,
  ylim = NULL,
  contrast_sparkline = F,
  show_mpe = T,
  show_preddist = T,
  ...
)
```

## Arguments

- bmm:

  A model from
  [`bayes_mm_run()`](https://dcr-eyethink.github.io/eyethinkbayes/reference/bayes_mm_run.md)
  (or the list returned by
  [`bayes_mm()`](https://dcr-eyethink.github.io/eyethinkbayes/reference/bayes_mm.md)).

- interacts:

  The interactions to plot, as a vector of text with the conditions
  separated by ":", eg `c("adcon:tv", "adcon:age:tv")`. If missing, all
  of the interactions in the model are plotted. Remember that
  punctuation is replaced by underscores in the names of conditions.

- title:

  Extra text for the title of the plot (and the file name).

- pred_values:

  For continuous variables, a vector of values at which to make
  predictions. Default is 10 equally spaced values.

- ribbon:

  For continuous variables, shade the region where there is no evidence
  that the lines differ?

- ladder:

  For continuous variables, write the MPE for the difference between
  lines at each predicted value?

- spark:

  For continuous variables, plot the MPEs at each predicted value in a
  panel below?

- slope_mpes:

  For continuous variables, write the MPE for each slope on the plot?

- mfx_sideplot:

  For continuous variables with a two level first factor, add a small
  plot on the left of the posterior distributions of the main effect of
  that factor.

- ylim:

  Limits for the y axis, as a vector of two numbers.

- contrast_sparkline:

  For factors, show the distribution of the contrast estimates below the
  plot?

- show_mpe:

  For factors, write the MPEs on the plot?

- show_preddist:

  For factors, show the posterior distributions of the means?

- ...:

  Further arguments for
  [`eyethinkdata::pirateye()`](https://dcr-eyethink.github.io/eyethinkdata/reference/pirateye.html),
  eg `bars = TRUE`, `violin = FALSE`, and `outp` to change where the pdf
  is saved (`outp = ""` to not save it).

## Value

A named list of plots, one for each interaction (named like "adcon:tv").

## Details

The order of the conditions in an interaction matters. The first is used
for the colour split, the second for the x axis, and the third, if there
is one, splits the plot into panels. The MPEs shown on the plot (the
probability that there is a difference) are for the effect of the first
condition at each level of the others.

**Factors only.** The estimated means are shown with the distributions
of the estimates as split violins, and with the MPE for the difference
between the colour conditions at each x position.

**One continuous variable.** Predictions are plotted as lines at
`pred_values` points along the x axis (by default 10 equally spaced
points), and the MPE that the slope of each line is not zero is shown.
If the first factor has two levels, the area where there is no evidence
that the lines differ (MPE below 90%) is shaded grey (`ribbon`), the MPE
for the difference at each point can be written on the plot (`ladder`),
or plotted below (`spark`). Only one continuous variable is allowed in
an interaction; if there are two, the function stops and returns a
message.

## See also

[`bayes_mm()`](https://dcr-eyethink.github.io/eyethinkbayes/reference/bayes_mm.md),
[`bayes_mm_mainFX()`](https://dcr-eyethink.github.io/eyethinkbayes/reference/bayes_mm_mainFX.md),
[`eyethinkdata::pirateye()`](https://dcr-eyethink.github.io/eyethinkdata/reference/pirateye.html)

## Examples

``` r
if (FALSE) { # \dontrun{
bayes_mm_interact(bmm, interacts = "adcon:tv", bars = TRUE, violin = FALSE)
# a continuous variable (age), split by two levels of adcon and tv
bayes_mm_interact(bmm, interacts = "adcon:age:tv", ladder = TRUE, ribbon = FALSE)
} # }
```
