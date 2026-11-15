# mlpwr 1.2.0

## In short

* **Results change:** GPR designs (the default for two or more design parameters) now reach the target power. Before, they were often slightly too small, so designs can now cost a little more.
* **Old results stay reproducible** with `noise_fun = "bernoulli", optimizer = "genoud"`.
* **Faster:** searches take about 4 times less CPU time.
* **Parallel simulations, opt-in:** `parallel = TRUE` runs the simulations on several cores. This pays off for slow simulation functions only.
* **Many bug fixes**, and `continue` now keeps the settings of the earlier search.

## Results change

* **New default noise model for the GPR surrogate** (`noise_fun = "logistic"`). Designs found with GPR (the default for two or more design parameters) now reach the target power more reliably. Before, they were often slightly too small.
* **Exact search over integer designs** (`optimizer = "auto"`, the new default). The next design is found by checking every integer design within the boundaries, whenever that is faster than `rgenoud::genoud`. This finds equally good or cheaper designs, mainly with two or more design parameters.
* As a result, GPR designs can cost slightly more than in mlpwr 1.1.1, because they now reach the target. In the two-group example of the t-test vignette (power .8), the mean cost rose from 120.4 to 126.8.
* To get the old results back, use:

  ```r
  find.design(..., noise_fun = "bernoulli", optimizer = "genoud")
  ```

* `noise_fun = "logistic"` needs 0/1 (or `TRUE`/`FALSE`) simulation results. If `simfun` returns other values, e.g. proportions, the search uses `"bernoulli"` instead and says so in a message.
* `noise_fun = "logistic"` affects only the GPR surrogate. Other surrogates, a user-supplied `noise_fun` and `use_noise = FALSE` give the same results as before.
* One-parameter searches with the default surrogate (`"logreg"`) almost always give the same results as before, apart from the `"logreg"` fix below.

## New

* `find.design(parallel = TRUE)` runs the simulations on all CPU cores but one; `parallel = 4` uses 4 cores. This pays off only for slow simulation functions (e.g. lme4, mirt). It needs the suggested packages future, future.apply and parallelly, and works on Windows too. Results are reproducible with `set.seed()` and do not depend on the number of cores, but differ from a sequential run. The default `parallel = FALSE` gives the same results as before. See the new vignette `vignette("parallel", package = "mlpwr")`.
* `find.design(optimizer = )` chooses how the next design is found: `"auto"` (default, see above) or `"genoud"` (as in mlpwr 1.1.1).
* `find.design(continue = ds)` now reuses the settings of the previous search unless they are given explicitly: `setsize`, `ci`, `ci_perc`, `time`, `control`, `aggregate_fun`, `noise_fun`, `integer`, `use_noise`, `autosave_dir`, `optimizer` and `parallel`. It also starts from the previous fit. Results store these settings in `$settings`.
* Continuing a result of mlpwr 1.1.1 uses `noise_fun = "bernoulli"` and `optimizer = "genoud"`, the methods it was made with, unless given explicitly.

## Faster

* Searches take about 4 times less CPU time on average. The main speed-up is in the GPR surrogate: predictions from `DiceKriging::predict.km()` are cached and computed for many points at once.
* Smaller speed-ups: the logistic and linear surrogates, the overhead per simulation, SVR tuning, the check for flat fits, and the 2-D heat plot.
* These speed-ups do not change results.

## Bug fixes

* Clear errors at the start of a search, before any simulation, for invalid input:
  * `power` not strictly between 0 and 1 (except for a custom criterion: a custom `aggregate_fun`, or `noise_fun` given as a function), or invalid `cost`;
  * an unknown `surrogate` (it must be `"gpr"`, `"svr"`, `"logreg"` or `"reg"`, in lower case) or an unknown `noise_fun`;
  * `surrogate = "logreg"` or `"reg"` with more than one design parameter (documented as one-parameter only; such searches ran before but gave poor designs);
  * no termination rule (`evaluations`, `ci` or `time`); such a search ran forever;
  * `n.startsets` times the number of design parameters below 3.
* `autosave_dir`: the file is now saved inside the directory, as documented (1.1.1 pasted the name in front of the file name, so `"results"` saved `resultsdat_autosave.RData`). A missing directory is created.
* GPR: a failed `DiceKriging::km()` fit no longer discards the last good model ("no applicable method for `@` applied to an object of class NULL"). If every attempt fails, there is a clear error.
* In 1-D, nearly linear GPR fits are no longer rejected as flat ("plane") fits.
* `surrogate = "logreg"`: a failed logistic fit is retried from an intercept-only start. Before, all later fits could fail too, and the search ended at the lower boundary (typical for steep power curves). If the final fit still fails, the result is `NA` with a warning.
* `surrogate = "logreg"`: `glm.fit()` warnings from these retried fits are no longer shown. Warnings from regular fits are still shown.
* `find.design(evaluations = NULL, time = ...)` failed with "invalid 'length' argument", so a search could not be limited by time alone. This works now, and such searches can be continued. The same holds when `ci` is the only termination rule.
* `find.design(continue = ds)` now uses a newly given `power` or `cost` instead of always keeping the previous target.
* The extra GPR that supplies a standard error for the other surrogates (and for the `ci` rule) now uses the search's `aggregate_fun`, `noise_fun` and, for `surrogate = "gpr"`, `control`. Results change only when these options are set.
* A design proposed twice in one batch is now stored once with all its evaluations. Before, narrow integer boundaries such as `c(1, 3)` could fail with "$ operator is invalid for atomic vectors".
* A bad final prediction at the edge of the design space now gives the "No good design found" warning instead of an `NA` design or an error.
* The `ci` termination rule no longer errors after a bad prediction.
* Fewer than 4 initial designs no longer crash the optimiser.
* `find.design()` no longer picks up user variables called `n_updates`, `n.bad.fits`, `n.bad.predictions`, `n.edgepredictions` or `fit` from the global environment.
* `find.design()` works in a fresh session in which no random number has been drawn yet.
* Simfuns and costfuns that take a vector and have extra arguments with defaults, e.g. `function(x, verbose = FALSE)`, now work with several design parameters.
* Simulation output is silenced without removing your own `sink()`, so e.g. `capture.output()` works. `silent = TRUE` prints nothing.
* `simulations_data()` works without an SD function, uses the search's `aggregate_fun`, and with a `noise_fun` function takes the SE from it (it gave NaN for results other than 0/1).
* `plot()`:
  * `type = "line"` (the documented name) works; `"hline"` still does.
  * The 2-D iso-power line is found reliably, so `type = "line"` no longer errors with the default `trim`.
  * `addribbon = TRUE` works when there is no SD.
  * Continuous 1-D designs are drawn on a fine grid, and 1-D slices of 2-D designs show the observed designs.
* `goodvals` is documented as unused. Values other than `"high"` give a warning.

## Other

* DESCRIPTION now declares `R (>= 4.1.0)`, which mlpwr already needed (it uses the native pipe `|>`).
* A harmless warning that R-devel raises inside DiceKriging::km() ("object length is not a multiple of subscript length") is muffled.

## Documentation

* New vignette "Parallel computation": when and how to use several cores.
* The ANOVA and GLM vignettes now show the computed design in their result sentences (they showed empty values).
* The multilevel vignette states the budget its search uses (2000, not 3000).
* `?example.simfun` and the multilevel vignette no longer refer to a non-existent `"multi2"` example.

# mlpwr 1.1.1

Added 5 new vignettes contributed by Marc Egli with walkthroughs through various applications: ANOVA, GLM, IRT, multilevel model, t-test. 

# mlpwr 1.1.0

Introducing some new features:

* Added an option to specify custom aggregate functions and noise functions in `find.design()`, allowing for more flexibility in handling Monte Carlo simulations.
* Added a new function, `simulated_data()`, to investigate data from the evaluations.

Also added a vignette "extensions" showcasing the new features, including:

* Sensitivity analysis and compromise analysis.
* Inhomogeneous cost functions.
* Search using three-dimensional study designs.


# mlpwr 1.0.1

* Hotfix for an error that prevented terminating in some cases
* Hotfix for some fitting issues for logistic regression and SVR


# mlpwr 1.0.0

