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
* Every exported function now has examples, and the help pages describe
  what each function checks, how it works and what it returns.
* The `profile_outliers()` help page, README and vignette explain how to
  read IQR flags: with four or more classes, confident correct predictions
  are often flagged, because each column's limits are calculated over all
  rows.
* The vignette now covers `handle_missing()`, `profile_outliers()` and
  `profile_auc_influence()`.
* Decorative divider lines (`# ---- ... ----`) were removed from code and
  test comments.

## Bug fixes

* `handle_missing()` now rebuilds the object after removing or filling rows,
  so `$auc`, `$pairwise`, `$n`, `$n_used` and `$complete` describe the
  cleaned data. Before, they still described the data before handling, and
  after `"remove"` the object was internally inconsistent. The cleaning
  itself is unchanged. If removing rows would leave a class empty, it now
  stops with a clear error.
* `mc_auc_ci()` now keeps a class with a single row in every bootstrap
  sample. Before, `sample()` on one row number `k` drew from `1:k`, so the
  class usually disappeared and the interval was wrong. For classes with two
  or more rows the resampled rows are exactly the same as before.
* `mc_auc_ci()` with `seed` no longer changes the caller's random-number
  stream.
* `profile_outliers()`, `profile_auc_influence()` and
  `profile_auc_combinations()` accept integer class labels, like the other
  functions.
* `profile_outliers()` warns when the probability columns are not named
  after the class labels, instead of silently skipping the high-confidence
  wrong-prediction check.
* `validate_classification_data()`: the "must sum to 1" error now gives the
  largest difference and how to fix rounded probabilities
  (`probs / rowSums(probs)`).

## Known issues

* `profile_outliers()` applies the IQR rule to each probability column over
  all rows, so with four or more classes it flags many confident correct
  predictions (77% of Dry Bean test rows). This is how the method is
  designed and is now documented; changing the rule would change the
  method.
* `validate_classification_data()` requires rows to sum to 1 within 1e-6,
  so rounded probabilities are rejected until they are renormalised.
* `mc_auc_ci()` names its probability argument `probabilities`, where the
  other functions use `probs`.
* Pairwise confidence intervals are not implemented yet.

# ProcEx 0.0.0.9000

* First development version.
