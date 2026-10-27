# Make a model formula

Combines the names of the dv, the conditions and the grouping factors
into a model formula, as text. Used by
[`bayes_mm_run()`](https://dcr-eyethink.github.io/eyethinkbayes/reference/bayes_mm_run.md).

## Usage

``` r
bayes_formula_gen(dvname, conditions = NULL, groups, mmtype = "intercept")
```

## Arguments

- dvname:

  The name of the dependent variable.

- conditions:

  The names of the conditions, which are crossed (`a * b`).

- groups:

  The names of the grouping factors, each of which gets a random effect.

- mmtype:

  "intercept" for random intercepts, "slopes" for random slopes by the
  conditions as well.

## Value

The formula as text, eg "rating ~ adcon \* tv + (1 \| pid)".

## Examples

``` r
eyethinkbayes:::bayes_formula_gen("rating", c("adcon", "tv"), "pid")
#> [1] "rating ~ adcon * tv + (1 | pid )"
```
