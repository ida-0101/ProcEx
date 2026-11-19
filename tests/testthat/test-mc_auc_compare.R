# Tests for mc_auc_compare()
# Argument names: truth, probs1, probs2, levels, n_boot, conf_level, seed,
# labels. sim_mc() and hand_till() come from helper-sim.R.

# Two iris models: "strong" uses the petals, "weak" uses the sepals.
make_iris_pair <- function() {
  set.seed(1)
  strong <- nnet::multinom(Species ~ Petal.Length + Petal.Width,
                           data = iris, trace = FALSE)
  weak   <- nnet::multinom(Species ~ Sepal.Length + Sepal.Width,
                           data = iris, trace = FALSE)
  list(
    truth  = iris$Species,
    strong = stats::predict(strong, type = "probs"),
    weak   = stats::predict(weak,   type = "probs")
  )
}



test_that("returns a well-formed mc_auc_compare object", {
  d <- make_iris_pair()
  res <- mc_auc_compare(d$truth, d$strong, d$weak, n_boot = 100, seed = 42)

  expect_s3_class(res, "mc_auc_compare")
  expect_named(res, c("auc1", "auc2", "difference", "lower_ci", "upper_ci",
                      "standard_error", "p_value", "conf_level", "n_boot",
                      "n_successful", "n_failed", "bootstrap_values",
                      "pairwise", "labels", "n", "n_used"))
  expect_equal(res$n_boot, 100)
  expect_equal(res$n_successful + res$n_failed, 100)
  expect_length(res$bootstrap_values, res$n_successful)
  expect_lte(res$lower_ci, res$upper_ci)
  expect_true(res$auc1 >= 0 && res$auc1 <= 1)
  expect_true(res$auc2 >= 0 && res$auc2 <= 1)
  expect_equal(res$difference, res$auc1 - res$auc2)
  expect_equal(res$standard_error, stats::sd(res$bootstrap_values))
  expect_true(res$p_value >= 0 && res$p_value <= 1)
  expect_equal(res$conf_level, 0.95)
})

test_that("interval, error and replicate names match mc_auc_ci()", {
  d <- make_iris_pair()
  cmp <- mc_auc_compare(d$truth, d$strong, d$weak, n_boot = 20, seed = 1)
  one <- mc_auc_ci(d$truth, d$strong, n_boot = 20, seed = 1)
  shared <- c("lower_ci", "upper_ci", "standard_error", "conf_level",
              "n_boot", "n_successful", "n_failed", "bootstrap_values")

  expect_true(all(shared %in% names(cmp)))
  expect_true(all(shared %in% names(one)))
  # n_boot means the number requested in both
  expect_equal(cmp$n_boot, one$n_boot)
})

test_that("each AUC matches pROC and an independent Hand & Till calculation", {
  d <- sim_mc(n = 200, k = 4, signal = 1.5, seed = 3)
  e <- sim_mc(n = 200, k = 4, signal = 0.5, seed = 4)
  res <- mc_auc_compare(d$y, d$p, e$p, n_boot = 20, seed = 1)

  expect_equal(res$auc1,
               as.numeric(suppressMessages(pROC::multiclass.roc(d$y, d$p))$auc))
  expect_equal(res$auc2,
               as.numeric(suppressMessages(pROC::multiclass.roc(d$y, e$p))$auc))
  expect_equal(res$auc1, hand_till(d$y, d$p))
  expect_equal(res$auc2, hand_till(d$y, e$p))
})


test_that("comparing a model with itself gives exactly zero", {
  d <- make_iris_pair()
  res <- mc_auc_compare(d$truth, d$strong, d$strong, n_boot = 100, seed = 1)

  expect_true(all(res$bootstrap_values == 0))
  expect_equal(res$difference, 0)
  expect_equal(res$lower_ci, 0)
  expect_equal(res$upper_ci, 0)
  expect_equal(res$standard_error, 0)
  expect_equal(res$p_value, 1)
})

test_that("swapping the models flips the sign (the bootstrap is paired)", {
  d <- make_iris_pair()
  a <- mc_auc_compare(d$truth, d$strong, d$weak, n_boot = 100, seed = 5)
  b <- mc_auc_compare(d$truth, d$weak, d$strong, n_boot = 100, seed = 5)

  expect_equal(b$difference, -a$difference)
  expect_equal(b$bootstrap_values, -a$bootstrap_values)
  expect_equal(b$lower_ci, -a$upper_ci)
  expect_equal(b$upper_ci, -a$lower_ci)
  expect_equal(b$p_value, a$p_value)
})

test_that("the same seed reproduces the result; a different seed does not", {
  d <- make_iris_pair()
  a <- mc_auc_compare(d$truth, d$strong, d$weak, n_boot = 100, seed = 99)
  b <- mc_auc_compare(d$truth, d$strong, d$weak, n_boot = 100, seed = 99)
  c <- mc_auc_compare(d$truth, d$strong, d$weak, n_boot = 100, seed = 100)

  expect_identical(a$bootstrap_values, b$bootstrap_values)
  expect_identical(a$lower_ci, b$lower_ci)
  expect_false(identical(a$bootstrap_values, c$bootstrap_values))
})

test_that("a better model is detected", {
  d <- make_iris_pair()
  res <- mc_auc_compare(d$truth, d$strong, d$weak, n_boot = 200, seed = 42)

  expect_gt(res$difference, 0)
  expect_gt(res$lower_ci, 0)
  expect_lt(res$p_value, 0.05)
})

test_that("two equally useless models are not called different", {
  y  <- sim_mc(n = 300, k = 4, signal = 2, seed = 1)$y
  p1 <- sim_mc(n = 300, k = 4, signal = 0, seed = 5)$p   # pure noise
  p2 <- sim_mc(n = 300, k = 4, signal = 0, seed = 6)$p   # pure noise
  res <- mc_auc_compare(y, p1, p2, n_boot = 200, seed = 11)

  expect_lt(res$lower_ci, 0)
  expect_gt(res$upper_ci, 0)
  expect_gt(res$p_value, 0.05)
})

test_that("a higher conf_level gives a wider interval", {
  d <- make_iris_pair()
  narrow <- mc_auc_compare(d$truth, d$strong, d$weak,
                           n_boot = 200, conf_level = 0.80, seed = 7)
  wide   <- mc_auc_compare(d$truth, d$strong, d$weak,
                           n_boot = 200, conf_level = 0.99, seed = 7)

  expect_gt(wide$upper_ci - wide$lower_ci, narrow$upper_ci - narrow$lower_ci)
  expect_equal(wide$conf_level, 0.99)
})

test_that("a class with a single row stays in every bootstrap sample", {
  # Regression test for the sample() pitfall: sample(i, 1) with a single
  # index i draws from 1:i, so the one-row class vanished from the replicate
  # and pROC warned "No observation for response level(s): d".
  d <- sim_mc(n = 120, k = 4, signal = 2, seed = 1)
  e <- sim_mc(n = 120, k = 4, signal = 1, seed = 1)
  y <- d$y
  y[y == "d"] <- "c"
  y[60] <- "d"                                   # class d has exactly 1 row

  warnings <- character()
  res <- withCallingHandlers(
    mc_auc_compare(y, d$p, e$p, n_boot = 50, seed = 1),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  # Only the shared class-size warnings, once each; nothing from pROC about
  # a class missing from a replicate.
  expect_identical(warnings, unique(warnings))
  expect_true(all(grepl("fewer than 10 observations|imbalance ratio",
                        warnings)))
  expect_false(any(grepl("No observation for response level", warnings)))
  expect_equal(res$n_failed, 0)
})


# Accepted input formats

test_that("data frames, character labels and column order give the same answer", {
  d <- make_iris_pair()
  ref <- mc_auc_compare(d$truth, d$strong, d$weak, n_boot = 50, seed = 3)

  as_df <- mc_auc_compare(d$truth, as.data.frame(d$strong),
                          as.data.frame(d$weak), n_boot = 50, seed = 3)
  as_chr <- mc_auc_compare(as.character(d$truth), d$strong, d$weak,
                           n_boot = 50, seed = 3)
  shuffled <- mc_auc_compare(d$truth, d$strong, d$weak[, c(3, 1, 2)],
                             n_boot = 50, seed = 3)
  given_levels <- mc_auc_compare(d$truth, d$strong, d$weak,
                                 levels = levels(d$truth),
                                 n_boot = 50, seed = 3)

  expect_equal(as_df$bootstrap_values, ref$bootstrap_values)
  expect_equal(as_chr$bootstrap_values, ref$bootstrap_values)
  expect_equal(shuffled$auc2, ref$auc2)
  expect_equal(shuffled$bootstrap_values, ref$bootstrap_values)
  expect_identical(given_levels$bootstrap_values, ref$bootstrap_values)
})

test_that("mc_roc objects can be passed in place of truth and probabilities", {
  d <- make_iris_pair()
  fit_strong <- mc_roc(d$truth, d$strong)
  fit_weak   <- mc_roc(d$truth, d$weak)
  ref <- mc_auc_compare(d$truth, d$strong, d$weak, n_boot = 50, seed = 8)

  two_fits   <- mc_auc_compare(fit_strong, fit_weak, n_boot = 50, seed = 8)
  fit_matrix <- mc_auc_compare(fit_strong, d$weak, n_boot = 50, seed = 8)
  named_arg  <- mc_auc_compare(fit_strong, probs2 = fit_weak,
                               n_boot = 50, seed = 8)
  fit_second <- mc_auc_compare(d$truth, d$strong, fit_weak,
                               n_boot = 50, seed = 8)

  for (res in list(two_fits, fit_matrix, named_arg, fit_second)) {
    expect_equal(res$auc1, ref$auc1)
    expect_equal(res$auc2, ref$auc2)
    expect_identical(res$bootstrap_values, ref$bootstrap_values)
  }
  expect_equal(two_fits$auc1, fit_strong$auc)
  expect_equal(two_fits$auc2, fit_weak$auc)
})


# Clear message for bad input

test_that("fewer than three classes is rejected", {
  two   <- iris$Species != "virginica"
  truth <- droplevels(iris$Species[two])
  p     <- stats::runif(sum(two))
  probs <- cbind(p, 1 - p)              # valid probabilities, two classes
  colnames(probs) <- levels(truth)

  expect_error(mc_auc_compare(truth, probs, probs), "at least 3 classes",
               ignore.case = TRUE)
})

test_that("mismatched row counts are rejected", {
  d <- make_iris_pair()

  expect_error(mc_auc_compare(d$truth, d$strong[1:140, ], d$weak),
               "probs1 has 140 rows but truth has 150 values")
  expect_error(mc_auc_compare(d$truth, d$strong, d$weak[1:140, ]),
               "probs2 has 140 rows but truth has 150 values")
})

test_that("probabilities that are not a matrix or data frame are rejected", {
  d <- make_iris_pair()

  expect_error(mc_auc_compare(d$truth, "not a matrix", d$weak),
               "probs1 must be a matrix or data frame")
  expect_error(mc_auc_compare(d$truth, d$strong, 1:150),
               "probs2 must be a matrix or data frame")
})

test_that("non-numeric probabilities are rejected", {
  d <- make_iris_pair()
  text <- matrix(as.character(d$weak), ncol = 3,
                 dimnames = list(NULL, colnames(d$weak)))

  expect_error(mc_auc_compare(d$truth, d$strong, text),
               "must contain numeric values")
})

test_that("invalid n_boot values are rejected", {
  d <- make_iris_pair()
  for (bad in list(0, -5, 2.5, "100", c(10, 20), NA_real_)) {
    expect_error(
      mc_auc_compare(d$truth, d$strong, d$weak, n_boot = bad),
      "n_boot must be a single positive integer",
      info = paste("n_boot =", deparse(bad))
    )
  }
})

test_that("invalid conf_level values are rejected", {
  d <- make_iris_pair()
  for (bad in list(0, 1, 1.5, -0.1, NA_real_, c(0.9, 0.95), "0.95")) {
    expect_error(
      mc_auc_compare(d$truth, d$strong, d$weak, conf_level = bad),
      "conf_level must be a single value between 0 and 1",
      info = paste("conf_level =", deparse(bad))
    )
  }
})


#  Failed bootstrap replicates

test_that("failed replicates are counted and warned about", {
  d <- make_iris_pair()
  real <- .mc_auc_fast
  calls <- 0
  # The bootstrap computes two AUCs per replicate; fail every 4th one.
  testthat::local_mocked_bindings(
    .mc_auc_fast = function(truth, probs) {
      calls <<- calls + 1
      if (calls %% 4 == 0) stop("simulated failure")
      real(truth, probs)
    }
  )

  expect_warning(
    res <- mc_auc_compare(d$truth, d$strong, d$weak, n_boot = 40, seed = 1),
    "bootstrap replicates failed"
  )
  expect_gt(res$n_failed, 0)
  expect_equal(res$n_boot, 40)
  expect_equal(res$n_successful + res$n_failed, 40)
  expect_length(res$bootstrap_values, res$n_successful)
})

test_that("it stops when almost every replicate fails", {
  d <- make_iris_pair()
  testthat::local_mocked_bindings(
    .mc_auc_fast = function(truth, probs) stop("simulated failure")
  )

  expect_error(mc_auc_compare(d$truth, d$strong, d$weak, n_boot = 10, seed = 1),
               "Too few successful bootstrap replicates")
})



test_that("print() shows the results and returns its input invisibly", {
  d <- make_iris_pair()
  res <- mc_auc_compare(d$truth, d$strong, d$weak, n_boot = 100, seed = 42)

  expect_output(print(res), "Multiclass AUC comparison")
  expect_output(print(res), "Model 1 AUC")
  expect_output(print(res), "95% CI", fixed = TRUE)
  expect_output(print(res), "100 successful, 0 failed", fixed = TRUE)

  utils::capture.output(shown <- withVisible(print(res)))
  expect_false(shown$visible)
  expect_identical(shown$value, res)
})

test_that("print() shows a tiny p-value as '< 1/n_boot', not 0.0000", {
  d <- make_iris_pair()
  res <- mc_auc_compare(d$truth, d$strong, d$weak, n_boot = 100, seed = 42)
  expect_equal(res$p_value, 0)
  expect_output(print(res), "p-value      : < 0.0100", fixed = TRUE)

  same <- mc_auc_compare(d$truth, d$strong, d$strong, n_boot = 100, seed = 1)
  expect_output(print(same), "p-value      : 1.0000", fixed = TRUE)
})


# Same checks and AUC as mc_roc()

test_that("both AUCs and the per-pair AUCs equal mc_roc()", {
  d <- make_iris_pair()
  res <- mc_auc_compare(d$truth, d$strong, d$weak, n_boot = 20, seed = 1)

  expect_equal(res$auc1, mc_roc(d$truth, d$strong)$auc)
  expect_equal(res$auc2, mc_roc(d$truth, d$weak)$auc)
  expect_equal(res$pairwise$auc1, mc_roc(d$truth, d$strong)$pairwise$auc)
  expect_equal(res$pairwise$auc2, mc_roc(d$truth, d$weak)$pairwise$auc)
})

test_that("each bootstrap difference equals pROC on the same resampled rows", {
  d <- make_iris_pair()
  res <- mc_auc_compare(d$truth, d$strong, d$weak, n_boot = 5, seed = 21)

  set.seed(21)
  for (b in 1:5) {
    i <- mc_boot_index(d$truth)
    a1 <- as.numeric(pROC::multiclass.roc(d$truth[i], d$strong[i, ],
                                          direction = ">")$auc)
    a2 <- as.numeric(pROC::multiclass.roc(d$truth[i], d$weak[i, ],
                                          direction = ">")$auc)
    expect_equal(res$bootstrap_values[b], a1 - a2)
  }
})

test_that("scores that are not probabilities are rejected, naming the model", {
  d <- make_iris_pair()
  logits <- log(d$weak) + 2

  expect_error(mc_auc_compare(d$truth, d$strong, logits, n_boot = 10),
               "probs2: Data checks failed")
  expect_error(mc_auc_compare(d$truth, logits, d$weak, n_boot = 10),
               "probs1: Data checks failed")
})

test_that("column names that do not match the classes are rejected", {
  d <- make_iris_pair()
  wrong <- d$weak
  colnames(wrong) <- c("A", "B", "C")

  expect_error(mc_auc_compare(d$truth, d$strong, wrong, n_boot = 10),
               "probs2: .*column names")
})

test_that("class-size warnings appear once, not once per model", {
  d <- sim_mc(n = 105, k = 3, signal = 2, seed = 4,
              weights = c(0.48, 0.48, 0.04))
  warnings <- character()
  withCallingHandlers(
    mc_auc_compare(d$y, d$p, d$p[, 3:1], n_boot = 10, seed = 1),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  expect_identical(warnings, unique(warnings))
  expect_true(any(grepl("fewer than 10 observations", warnings)))
})


# Missing values

test_that("a row missing in either model is left out of both, with a warning", {
  d <- make_iris_pair()
  weak <- d$weak
  weak[c(3, 60, 120), "versicolor"] <- NA
  keep <- !seq_len(150) %in% c(3, 60, 120)

  expect_warning(
    res <- mc_auc_compare(d$truth, d$strong, weak, n_boot = 20, seed = 1),
    "3 of 150 rows \\(2.0%\\).*left out of both"
  )
  expect_equal(res$n, 150)
  expect_equal(res$n_used, 147)
  # Model 1 had no missing values, but uses the same 147 rows as model 2.
  expect_equal(res$auc1, mc_roc(d$truth[keep], d$strong[keep, ])$auc)
  expect_equal(res$auc2, mc_roc(d$truth[keep], weak[keep, ])$auc)
  expect_output(print(res), "Rows used\\s+: 147 of 150")
})

test_that("a missing label is left out too", {
  d <- make_iris_pair()
  y <- d$truth
  y[10] <- NA

  expect_warning(res <- mc_auc_compare(y, d$strong, d$weak, n_boot = 10,
                                       seed = 1),
                 "1 of 150 rows")
  expect_equal(res$n_used, 149)
})

test_that("a class with no complete rows is a clear error", {
  d <- make_iris_pair()
  weak <- d$weak
  weak[d$truth == "setosa", "setosa"] <- NA

  expect_error(suppressWarnings(
    mc_auc_compare(d$truth, d$strong, weak, n_boot = 10)),
    "No rows complete in both models for class\\(es\\): setosa")
})


# mc_roc objects, labels and levels

test_that("mc_roc objects fitted on different labels are rejected", {
  d <- make_iris_pair()
  fit1 <- mc_roc(d$truth, d$strong)
  fit2 <- mc_roc(rev(d$truth), d$weak)

  expect_error(mc_auc_compare(fit1, fit2, n_boot = 10),
               "fitted on different class labels")
  expect_error(mc_auc_compare(d$truth, d$strong, fit2, n_boot = 10),
               "fitted on different class labels")
})

test_that("unclear model arguments are rejected", {
  d <- make_iris_pair()
  fit1 <- mc_roc(d$truth, d$strong)

  expect_error(mc_auc_compare(fit1, d$weak, d$weak, n_boot = 10),
               "give only the second model")
  expect_error(mc_auc_compare(fit1, n_boot = 10), "Supply a second model")
  expect_error(mc_auc_compare(d$truth, d$strong, n_boot = 10),
               "Supply two models")
})

test_that("labels come from mc_roc objects, variable names or the labels argument", {
  d <- make_iris_pair()
  strong <- d$strong
  weak <- d$weak

  by_name <- mc_auc_compare(d$truth, strong, weak, n_boot = 10, seed = 1)
  expect_identical(by_name$labels, c("strong", "weak"))

  unnamed <- mc_auc_compare(d$truth, d$strong, d$weak, n_boot = 10, seed = 1)
  expect_identical(unnamed$labels, c("Model 1", "Model 2"))

  fits <- mc_auc_compare(mc_roc(d$truth, strong, label = "Petals"),
                         mc_roc(d$truth, weak, label = "Sepals"),
                         n_boot = 10, seed = 1)
  expect_identical(fits$labels, c("Petals", "Sepals"))

  given <- mc_auc_compare(d$truth, strong, weak, n_boot = 10, seed = 1,
                          labels = c("A", "B"))
  expect_identical(given$labels, c("A", "B"))
  expect_output(print(given), "A AUC\\s+: 0\\.99")

  expect_error(mc_auc_compare(d$truth, strong, weak, labels = "one"),
               "labels must be NULL or a character vector of length 2")
})

test_that("levels must contain every label in truth", {
  d <- make_iris_pair()

  expect_error(mc_auc_compare(d$truth, d$strong, d$weak, n_boot = 10,
                              levels = c("setosa", "versicolor")),
               "truth contains labels not in levels: virginica")
  expect_error(mc_auc_compare(d$truth, d$strong, d$weak, n_boot = 10,
                              levels = c("setosa", "setosa", "virginica")),
               "levels must be a vector of distinct class labels")
})


# Random numbers

test_that("seed gives a reproducible result and leaves the user's random stream alone", {
  d <- make_iris_pair()

  set.seed(100)
  expected_next <- stats::runif(1)

  set.seed(100)
  a <- mc_auc_compare(d$truth, d$strong, d$weak, n_boot = 20, seed = 5)
  expect_identical(stats::runif(1), expected_next)

  b <- mc_auc_compare(d$truth, d$strong, d$weak, n_boot = 20, seed = 5)
  expect_identical(a$bootstrap_values, b$bootstrap_values)
})

test_that("invalid seeds are rejected", {
  d <- make_iris_pair()
  for (bad in list("1", c(1, 2), NA_real_, Inf)) {
    expect_error(mc_auc_compare(d$truth, d$strong, d$weak, n_boot = 5,
                                seed = bad),
                 "seed must be NULL or a single number",
                 info = paste("seed =", deparse(bad)))
  }
})
