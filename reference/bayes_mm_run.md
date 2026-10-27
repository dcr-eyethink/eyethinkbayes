# Run a Bayesian hierarchical mixed model

Runs a Bayesian mixed model with up to three conditions as fixed effects
and any number of grouping factors (such as participants) as random
effects, using the `rstanarm` package. You give it the names of the
columns and it writes the model formula for you. This can take a few
minutes.

## Usage

``` r
bayes_mm_run(
  data,
  dvname,
  conditions,
  groups,
  f = NULL,
  mmtype = "intercept",
  binom = F,
  adapt_delta = 0.8,
  explore_browser = F,
  iter = 2000
)
```

## Arguments

- data:

  The data in long format, with a row for each observation.

- dvname:

  The name of the column with the dependent variable.

- conditions:

  A vector of up to three names of columns, the fixed effects. Factors
  (or text) and numeric variables can be used.

- groups:

  A vector of names of columns with the grouping factors for the random
  effects, eg "pid".

- f:

  A model formula (as text) to use instead of making one from
  `conditions` and `groups`.

- mmtype:

  "intercept" (the default) for random intercepts, or "slopes" to have
  random slopes too.

- binom:

  If `TRUE`, the dv is modelled as binomial, with a logit link (for 0/1
  data). The default is a normal (gaussian) model.

- adapt_delta:

  Defaults to 0.8. Raise it towards 1 if the model has convergence
  problems (it is slower).

- explore_browser:

  If `TRUE`, open a browser window to explore the model with
  `shinystan`.

- iter:

  The number of iterations for each chain. Default 2000.

## Value

The model, a stanreg object from `rstanarm`. Pass it to
[`bayes_mm()`](https://dcr-eyethink.github.io/eyethinkbayes/reference/bayes_mm.md),
[`bayes_mm_mainFX()`](https://dcr-eyethink.github.io/eyethinkbayes/reference/bayes_mm_mainFX.md)
or
[`bayes_mm_interact()`](https://dcr-eyethink.github.io/eyethinkbayes/reference/bayes_mm_interact.md).

## Details

All of the conditions are crossed (`a * b * c`), and a random effect is
added for each of the groups. With the default `mmtype = "intercept"`
each group gets a random intercept (`(1 | pid)`); with "slopes" each
group also gets random slopes for the conditions (`(1 + a + b | pid)`).

To avoid problems later on, punctuation and spaces in the names of the
conditions, and in the levels of any condition that is a factor or text,
are replaced by underscores (so "Not addressable" becomes
"Not_addressable"), and text columns are turned into factors. This is
done to a copy of your data, which is stored in the model (`bmm$data`).
The `conditions` are cleaned in the same way, so when you ask for plots,
use the cleaned names.

The model is run with the default priors of
[`rstanarm::stan_glmer`](https://mc-stan.org/rstanarm/reference/stan_glmer.html).
The function also sets the R option `mc.cores` to the number of cores on
your computer, so that the chains run in parallel.

## See also

[`bayes_mm()`](https://dcr-eyethink.github.io/eyethinkbayes/reference/bayes_mm.md)

## Examples

``` r
if (FALSE) { # \dontrun{
bmm <- bayes_mm_run(data = ad_data, dvname = "dprime",
                    conditions = c("adcon", "age", "tv"),
                    groups = c("pid", "dim"))
} # }
```
