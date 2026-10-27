# Remove punctuation from column names and levels

Replaces punctuation and spaces with underscores, in the names of the
conditions and the levels of any that are factors or text, which makes
the models and plots simpler to work with. Used by
[`bayes_mm_run()`](https://dcr-eyethink.github.io/eyethinkbayes/reference/bayes_mm_run.md).

## Usage

``` r
bayes_punctuationclean(dt = NULL, cvec, subpat = "[[:punct:]|[:space:]]")
```

## Arguments

- dt:

  A data.table. If it is missing, then `cvec` is cleaned and returned.

- cvec:

  The names of the columns to clean.

- subpat:

  The pattern (a regular expression) to replace.

## Value

The cleaned data.table (with text columns turned into factors), or if
`dt` is missing the cleaned `cvec`.

## Examples

``` r
eyethinkbayes:::bayes_punctuationclean(cvec = c("Ad condition", "tv-choice"))
#> [1] "Ad_condition" "tv_choice"   
```
