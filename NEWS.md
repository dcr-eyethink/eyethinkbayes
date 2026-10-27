# eyethinkbayes 1.0.0

## Documentation
* Rewrote the help pages for every function, with descriptions, full parameter
  lists, return values and examples.

## Changes that may affect existing scripts
* `bayes_mm()` now passes `outp` on to the plotting functions, so the plot pdfs
  are saved in the `outp` folder along with the text report. Before, the plots
  were always saved in a folder called "analysis". With `outp = NULL` or `""`
  nothing is saved. (With the default `outp = "analysis"` nothing changes.)
* `bayes_mm_mainFX()` now also returns `data_mainfx`, the data behind the plot,
  alongside `plot_mainfx`.

## Bug fixes
* `bayes_mm_interact(mfx_sideplot = TRUE)` now works. It used to fail.
* `bayes_mm_interact(pred_values = ...)` now works for continuous variables
  with any name; it was hard coded to a variable called "age".
* `bayes_mm_run(explore_browser = TRUE)` now finds `shinystan`.
* `bayes_mm()` no longer leaves output diverted to its report file if there is an
  error.
* Declared the packages that were used but not listed as imports (`emmeans`,
  `insight`, `psych`, `cowplot`), and fixed the author field.
* Listed `marginaleffects`, `collapse` and `reformulas` in Suggests. Recent versions
  of `modelbased` and `insight` need them to estimate means, and without them
  the vignette (and the pkgdown site) could not be built.
* The vignette is now pre-rendered (`vignettes/eyethinkbayes-vignette.Rmd.orig`
  is the source). The models are no longer refitted when the package or the
  pkgdown site is built, so a newer `modelbased` on the build machine cannot
  break the build. The package code is unchanged.
