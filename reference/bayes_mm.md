# Run, analyse and plot a Bayesian mixed model in one step

A wrapper that runs a Bayesian mixed model (or takes one you have
already run), prints a report of the model, and then plots all of the
main effects and all of the interactions. It is the quickest way to
analyse a data set; the functions it calls
([`bayes_mm_run()`](https://dcr-eyethink.github.io/eyethinkbayes/reference/bayes_mm_run.md),
[`bayes_mm_mainFX()`](https://dcr-eyethink.github.io/eyethinkbayes/reference/bayes_mm_mainFX.md)
and
[`bayes_mm_interact()`](https://dcr-eyethink.github.io/eyethinkbayes/reference/bayes_mm_interact.md))
can also be used on their own, for more control.

## Usage

``` r
bayes_mm(bmm = list(), outp = "analysis", title = NULL, plotcheck = F, ...)
```

## Arguments

- bmm:

  A model that you have already run, either the output of
  [`bayes_mm_run()`](https://dcr-eyethink.github.io/eyethinkbayes/reference/bayes_mm_run.md)
  or the list returned by this function. If it is not given (the
  default), a new model is run using the arguments in `...`.

- outp:

  The name of a folder in which to save the text report and the plots as
  pdfs (it is created if needed). Set to `NULL` or "" to not save
  anything.

- title:

  Extra text to add to the plot titles and the report file name.

- plotcheck:

  If `TRUE`, also return plots to check the model: the traces of the
  chains, a posterior check of the mean, and a posterior check of the
  density of the data.

- ...:

  Arguments for
  [`bayes_mm_run()`](https://dcr-eyethink.github.io/eyethinkbayes/reference/bayes_mm_run.md)
  (`data`, `dvname`, `conditions`, `groups`, `binom`, `iter` etc.) if a
  model is to be run, and/or for
  [`bayes_mm_mainFX()`](https://dcr-eyethink.github.io/eyethinkbayes/reference/bayes_mm_mainFX.md),
  [`bayes_mm_interact()`](https://dcr-eyethink.github.io/eyethinkbayes/reference/bayes_mm_interact.md)
  and
  [`eyethinkdata::pirateye()`](https://dcr-eyethink.github.io/eyethinkdata/reference/pirateye.html)
  for plotting.

## Value

A list with:

- bmm:

  the model (a stanreg object), which you can pass back to the plotting
  functions

- mainFX:

  output of
  [`bayes_mm_mainFX()`](https://dcr-eyethink.github.io/eyethinkbayes/reference/bayes_mm_mainFX.md)

- interactFX:

  the plots from
  [`bayes_mm_interact()`](https://dcr-eyethink.github.io/eyethinkbayes/reference/bayes_mm_interact.md),
  or `NULL` if there was only one condition

- plotcheck_plots:

  the check plots, if `plotcheck = TRUE`

- param:

  the table of model parameters

## Details

**The report.** The model, its priors, and a table of parameters (from
[`parameters::model_parameters`](https://easystats.github.io/parameters/reference/model_parameters.html))
are printed to the console, followed by the means and contrasts for the
main effects and interactions. If `outp` is a folder name, the same text
is also saved to a file in that folder called
`<dv> <conditions>BayesMM <title>.txt`.

**The plots.** All the main effects are plotted by
[`bayes_mm_mainFX()`](https://dcr-eyethink.github.io/eyethinkbayes/reference/bayes_mm_mainFX.md).
If there is more than one condition, the interactions are plotted by
[`bayes_mm_interact()`](https://dcr-eyethink.github.io/eyethinkbayes/reference/bayes_mm_interact.md):
the first condition with each of the others, and then all of them
together. The plots are drawn by
[`eyethinkdata::pirateye()`](https://dcr-eyethink.github.io/eyethinkdata/reference/pirateye.html),
and are saved as pdfs in the `outp` folder.

**Passing arguments.** If you supply `bmm`, everything in `...` goes to
the plotting functions, so you can use any argument of
[`bayes_mm_mainFX()`](https://dcr-eyethink.github.io/eyethinkbayes/reference/bayes_mm_mainFX.md),
[`bayes_mm_interact()`](https://dcr-eyethink.github.io/eyethinkbayes/reference/bayes_mm_interact.md)
or
[`eyethinkdata::pirateye()`](https://dcr-eyethink.github.io/eyethinkdata/reference/pirateye.html)
(eg `bars = TRUE`). If you want `bayes_mm()` to run the model for you,
then `...` must contain the arguments for
[`bayes_mm_run()`](https://dcr-eyethink.github.io/eyethinkbayes/reference/bayes_mm_run.md)
(`data`, `dvname`, `conditions`, `groups` and so on); these are also
passed to the plotting functions, which ignore them. Because
[`bayes_mm_run()`](https://dcr-eyethink.github.io/eyethinkbayes/reference/bayes_mm_run.md)
does not accept plotting arguments, it is best to run the model first
with
[`bayes_mm_run()`](https://dcr-eyethink.github.io/eyethinkbayes/reference/bayes_mm_run.md)
if you want to customise the plots.

## See also

[`bayes_mm_run()`](https://dcr-eyethink.github.io/eyethinkbayes/reference/bayes_mm_run.md),
[`bayes_mm_mainFX()`](https://dcr-eyethink.github.io/eyethinkbayes/reference/bayes_mm_mainFX.md),
[`bayes_mm_interact()`](https://dcr-eyethink.github.io/eyethinkbayes/reference/bayes_mm_interact.md)

## Examples

``` r
if (FALSE) { # \dontrun{
res <- bayes_mm(data = ad_data, dvname = "rating",
                conditions = c("adcon", "dim", "tv"), groups = "pid",
                plotcheck = TRUE)
# replot, using the model that was returned
bayes_mm_interact(res, interacts = "adcon:dim:tv", violin = FALSE)
} # }
```
