# Plot the distribution of contrast estimates

Draws a small plot, with a panel for each condition, of the posterior
distribution of the difference between conditions. The central 95% (the
highest density interval) is shaded and zero is marked with a vertical
line. It is designed to be put underneath a means plot. Used by
[`bayes_mm_mainFX()`](https://dcr-eyethink.github.io/eyethinkbayes/reference/bayes_mm_mainFX.md)
and
[`bayes_mm_interact()`](https://dcr-eyethink.github.io/eyethinkbayes/reference/bayes_mm_interact.md).

## Usage

``` r
bayes_contrastsparkline(bcontrasts)
```

## Arguments

- bcontrasts:

  A data.table with columns `cond` (the name of the condition) and
  `diff_est` (samples from the posterior distribution of the
  difference).

## Value

A ggplot.
