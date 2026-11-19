# ProcEx 0.0.0.9001

## Breaking changes

* `mc_auc_compare()` results now use the same names as `mc_auc_ci()`:

  | Before | Now |
  |---|---|
  | class `mc_compare` | class `mc_auc_compare` |
  | `lower`, `upper` | `lower_ci`, `upper_ci` |
  | `se` | `standard_error` |
  | `replicates` | `bootstrap_values` |
  | `n_boot` (successful replicates) | `n_boot` (requested) and `n_successful` |

  Code that reads the old names, such as `res$lower`, needs updating.

* `mc_auc_compare()` now applies the same input checks as `mc_roc()`, so it
  rejects inputs it used to accept silently: scores that are not
  probabilities, probability columns not named after the classes, and rows
  that do not sum to 1.

## New features

* `plot()` for `mc_roc` objects: ROC curves for every pair of classes
  (`type = "pairs"`), or a heatmap of the pairwise AUCs
  (`type = "heatmap"`). pROC has no plot method for multiclass objects.
* `plot()` for `mc_auc_compare()` results: the bootstrap distribution of the
  AUC difference (`type = "bootstrap"`), or each class pair's AUC under both
  models (`type = "pairs"`).
* `mc_auc_compare()` gains a `labels` argument and returns `pairwise`
  (per-pair AUCs of both models), `labels`, `n` and `n_used`.
* New vignette, "Getting started with ProcEx": `vignette("ProcEx")`.

## Improvements to `mc_auc_compare()`

* The two AUCs are computed with `mc_roc()`, so `mc_auc_compare()` and
  `mc_roc()` always agree.
* A row with a missing label, or a missing probability in either model, is
  left out of both models with a warning, so the comparison stays paired.
  Previously missing values were passed to pROC, which dropped them
  separately for each pair of classes without a warning.
* The bootstrap uses the package's rank-based Hand & Till calculation, which
  gives the same values as pROC and is about 6 times faster (Dry Bean test
  set, 1000 replicates: 68 s before, 11 s now).
* `seed` no longer changes the caller's random-number stream.
* Two `mc_roc` objects fitted on different labels are now rejected.
* Warnings about small or imbalanced classes are shown once, not once per
  model.

## Documentation and packaging

* README: plotting section, updated comparison section, input requirements
  and test results.
* The `mc_roc()` help page no longer says that `mc_auc_ci()` receives or
  fills the `mc_roc` object (it takes labels and probabilities directly).
* The Dry Bean zip in `inst/extdata` (4.6 MB, not used by any code) is
  excluded from the built package. The previous `.Rbuildignore` entry did
  not match its location.
* DESCRIPTION: `URL`, `BugReports`, vignette support, and `graphics` and
  `grDevices` in Imports (used by the plots).

## Known issues (not changed in this version)

* `handle_missing()` changes the data but does not recalculate the AUC, so
  `$auc`, `$n` and `$n_used` describe the data before handling. Ending it
  with the internal `mc_refit()` would fix this.
* `profile_outliers()` applies the IQR rule to each probability column across
  all rows. With four or more classes this flags most confident, correct
  predictions (77% of Dry Bean test rows), and `profile_auc_influence()` and
  `profile_auc_combinations()` start from these flags.
* `mc_auc_ci()` gives a wrong interval when a class has a single row
  (`sample(i)` with one index draws from `1:i`); the internal
  `mc_boot_index()` avoids this. Its argument is named `probabilities`,
  where the other functions use `probs`.
* `validate_classification_data()` requires rows to sum to 1 within 1e-6, so
  probabilities rounded to 3 decimals (for example, read from a CSV) are
  rejected.
* Pairwise confidence intervals are not implemented yet.

# ProcEx 0.0.0.9000

* First development version.
