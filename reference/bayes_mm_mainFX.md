# Plot and report the main effects from a Bayesian mixed model

For each main effect (condition) in the model, prints a table of the
estimated means and contrasts, and draws a plot showing the observed
data and the posterior distributions of the estimated means. Any
arguments of
[`eyethinkdata::pirateye()`](https://dcr-eyethink.github.io/eyethinkdata/reference/pirateye.html)
can be passed in to change the plot.

## Usage

``` r
bayes_mm_mainFX(
  bmm,
  mainfxs = NULL,
  title = NULL,
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

- mainfxs:

  A vector of the names of the conditions to plot. By default all of the
  main effects in the model are plotted.

- title:

  Extra text for the title of the plot (and the file name).

- contrast_sparkline:

  Show the distribution of the contrast estimate below the plot?

- show_mpe:

  Write the MPEs on the plot?

- show_preddist:

  Show the posterior distributions of the means?

- ...:

  Further arguments for
  [`eyethinkdata::pirateye()`](https://dcr-eyethink.github.io/eyethinkdata/reference/pirateye.html),
  eg `bars = TRUE`, `violin = FALSE`, `w` and `h` (the width and height
  of the saved pdf).

## Value

A list with `plot_mainfx`, the plot (a cowplot grid, so you can also
draw it with [`print()`](https://rdrr.io/r/base/print.html)), and
`data_mainfx`, a list of the data behind it: `observed`, `predicted`
(the posterior samples), `m` (the estimated means), `mpe` and
`bcontrasts`.

## Details

For conditions that are factors, the plot shows the observed data
(violins), the estimated mean for each level, and the posterior
distribution of each estimate (the smaller violins). When a condition
has two levels, the MPE (maximum probability of effect, ie the
probability that the difference between the two is not zero) is written
between the two levels. The MPE is the `pd` (probability of direction)
from the `modelbased` package.

With `contrast_sparkline = TRUE`, the distribution of the difference
between conditions is drawn underneath, with the central 95% shaded in
grey and zero marked by a black line. If the line is outside of the grey
area, there is more than a 95% probability that the difference is not
zero.

For conditions that are continuous variables, a separate plot shows the
predicted line, with the MPE for the slope.

The text output is printed to the console. (When the function is called
by
[`bayes_mm()`](https://dcr-eyethink.github.io/eyethinkbayes/reference/bayes_mm.md)
it is saved to a file.) Plots are saved as pdfs by
[`eyethinkdata::pirateye()`](https://dcr-eyethink.github.io/eyethinkdata/reference/pirateye.html):
to change where, pass `outp` in `...` (use `outp = ""` to not save
them).

For binomial models the means are on the probability scale.

## See also

[`bayes_mm()`](https://dcr-eyethink.github.io/eyethinkbayes/reference/bayes_mm.md),
[`bayes_mm_interact()`](https://dcr-eyethink.github.io/eyethinkbayes/reference/bayes_mm_interact.md),
[`eyethinkdata::pirateye()`](https://dcr-eyethink.github.io/eyethinkdata/reference/pirateye.html)

## Examples

``` r
if (FALSE) { # \dontrun{
mfx <- bayes_mm_mainFX(bmm, mainfxs = "adcon")
mfx$plot_mainfx
bayes_mm_mainFX(bmm, mainfxs = c("adcon", "tv"), bars = TRUE, violin = FALSE,
                contrast_sparkline = TRUE, show_mpe = FALSE)
} # }
```
