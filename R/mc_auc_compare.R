#' Compare the multiclass AUC of two models
#'
#' Compares the Hand & Till (2001) multiclass AUC of two classification
#' models scored on the same observations, using a paired stratified
#' bootstrap to measure the uncertainty in their AUC difference.
#'
#' @details
#' **Same checks as [mc_roc()].** Each model's labels and probabilities go
#' through the same checks as [mc_roc()] (column names matching the class
#' labels, values between 0 and 1, rows summing to 1, at least three
#' classes), and the observed AUCs and per-pair AUCs are taken from
#' `mc_roc()`, so `mc_auc_compare()` and `mc_roc()` always report the same
#' AUC for the same data. Warnings about small or imbalanced classes are
#' shown once, not once per model.
#'
#' **Missing values.** A row with a missing label, or a missing probability
#' in either model, is left out of both models, with a warning giving the
#' count and percentage. Dropping it from both keeps the comparison paired:
#' the two AUCs are always computed on exactly the same rows.
#'
#' **Bootstrap.** Each replicate resamples rows within each class, so class
#' sizes never change and no class can disappear (including a class with a
#' single row), and applies the same rows to both models. The AUCs inside
#' the loop use an exact rank-based Hand & Till calculation that gives the
#' same value as pROC but is several times faster.
#'
#' The interval is the percentile interval of the bootstrap differences. The
#' p-value is two-sided: twice the smaller of the shares of replicates at or
#' above zero and at or below zero, capped at 1. Small p-values are limited
#' by the number of replicates; `print()` shows a value of zero as
#' `< 1 / n_successful`.
#'
#' **Random numbers.** With `seed`, the result is reproducible and your own
#' random-number stream is left exactly as it was.
#'
#' @param truth True class labels (factor, character or integer vector), or
#'   an `mc_roc` object for the first model.
#' @param probs1 The first model's predicted probabilities: a numeric matrix
#'   or data frame, one row per observation and one column per class, named
#'   exactly as the class labels (any column order). May also be an `mc_roc`
#'   object. When `truth` is an `mc_roc` object, give the second model here.
#' @param probs2 The second model's predicted probabilities, in the same form
#'   and the same row order as `probs1`, or an `mc_roc` object.
#' @param levels Optional set of class labels. Every label in `truth` must
#'   be one of them; classes with no observations are dropped, as in
#'   [mc_roc()]. Defaults to the labels in `truth`.
#' @param n_boot Number of bootstrap replicates. A positive whole number.
#'   Default 1000.
#' @param conf_level Confidence level for the interval, strictly between 0
#'   and 1. Default 0.95.
#' @param seed Optional single number, for a reproducible bootstrap.
#' @param labels Optional names for the two models, used by `print()` and
#'   `plot()`. Defaults to the labels of `mc_roc` objects, or the variable
#'   names passed as `probs1` and `probs2`, or "Model 1" and "Model 2".
#'
#' @return An object of class `mc_auc_compare`, a list containing:
#'   \item{auc1, auc2}{the two multiclass AUCs}
#'   \item{difference}{auc1 - auc2 on the observed data}
#'   \item{lower_ci, upper_ci}{confidence interval for the difference}
#'   \item{standard_error}{bootstrap standard error of the difference}
#'   \item{p_value}{two-sided bootstrap p-value}
#'   \item{conf_level}{the confidence level used}
#'   \item{n_boot}{bootstrap replicates requested}
#'   \item{n_successful, n_failed}{successful and failed replicates}
#'   \item{bootstrap_values}{the successful bootstrap differences}
#'   \item{pairwise}{data frame, one row per class pair: pair, class1,
#'     class2, auc1, auc2 and difference, from the observed data (no
#'     intervals). Used by `plot(type = "pairs")`.}
#'   \item{labels}{the two model names}
#'   \item{n, n_used}{rows supplied, and rows used (complete in both
#'     models)}
#'
#'   The interval, standard error and replicate components have the same
#'   names as in [mc_auc_ci()], so both results are read the same way.
#'
#' @references Hand, D. J. & Till, R. J. (2001). A simple generalisation of
#'   the area under the ROC curve for multiple class classification problems.
#'   Machine Learning, 45, 171-186.
#'
#' @examples
#' \donttest{
#' petals <- nnet::multinom(Species ~ Petal.Length + Petal.Width,
#'                          data = iris, trace = FALSE)
#' sepals <- nnet::multinom(Species ~ Sepal.Length + Sepal.Width,
#'                          data = iris, trace = FALSE)
#' res <- mc_auc_compare(iris$Species,
#'                       predict(petals, type = "probs"),
#'                       predict(sepals, type = "probs"),
#'                       n_boot = 200, seed = 42,
#'                       labels = c("Petals", "Sepals"))
#' res
#' plot(res)
#' plot(res, type = "pairs")
#'
#' # mc_roc objects work too, and keep their labels
#' fit_p <- mc_roc(iris$Species, predict(petals, type = "probs"),
#'                 label = "Petals")
#' fit_s <- mc_roc(iris$Species, predict(sepals, type = "probs"),
#'                 label = "Sepals")
#' mc_auc_compare(fit_p, fit_s, n_boot = 200, seed = 42)
#' }
#'
#' @seealso [mc_auc_ci()] for one model, [mc_roc()] for one model's AUC,
#'   [plot.mc_auc_compare()] to plot the result.
#' @export
mc_auc_compare <- function(truth,
                           probs1,
                           probs2,
                           levels     = NULL,
                           n_boot     = 1000,
                           conf_level = 0.95,
                           seed       = NULL,
                           labels     = NULL) {

  # Settings
  if (!is.numeric(n_boot) || length(n_boot) != 1 || is.na(n_boot) ||
      n_boot < 1 || n_boot != round(n_boot))
    stop("n_boot must be a single positive integer.", call. = FALSE)

  if (!is.numeric(conf_level) || length(conf_level) != 1 ||
      is.na(conf_level) || conf_level <= 0 || conf_level >= 1)
    stop("conf_level must be a single value between 0 and 1.", call. = FALSE)

  if (!is.null(seed) &&
      (!is.numeric(seed) || length(seed) != 1 || !is.finite(seed)))
    stop("seed must be NULL or a single number.", call. = FALSE)

  if (!is.null(labels) &&
      (!is.character(labels) || length(labels) != 2 || anyNA(labels)))
    stop("labels must be NULL or a character vector of length 2.",
         call. = FALSE)

  # The two models
  # Default names are the variable names passed, as mc_roc() does.
  name_of <- function(expr) {
    l <- .mc_default_label(expr)
    if (is.null(l) || !nzchar(l)) NA_character_ else l
  }
  arg_names <- c(name_of(substitute(probs1)), name_of(substitute(probs2)))

  # An mc_roc object may stand in for truth (then it is model 1, and the
  # second model is whichever of probs1 / probs2 was given), probs1 or probs2.
  if (inherits(truth, "mc_roc")) {
    has1 <- !missing(probs1)
    has2 <- !missing(probs2)
    if (has1 && has2) {
      stop("When truth is an mc_roc object, give only the second model ",
           "(as probs1 or probs2).", call. = FALSE)
    }
    if (!has1 && !has2) {
      stop("Supply a second model to compare with.", call. = FALSE)
    }
    second <- if (has1) probs1 else probs2
    arg_names <- c(name_of(substitute(truth)),
                   if (has1) arg_names[1] else arg_names[2])
    probs1 <- truth
    probs2 <- second
    truth  <- truth$truth
  } else if (missing(probs1) || missing(probs2)) {
    stop("Supply two models: probs1 and probs2.", call. = FALSE)
  }

  models <- list(probs1, probs2)
  object_labels <- c(NA_character_, NA_character_)
  for (m in 1:2) {
    p <- models[[m]]
    if (inherits(p, "mc_roc")) {
      if (!identical(as.character(p$truth), as.character(truth))) {
        stop("The mc_roc object for model ", m, " was fitted on different ",
             "class labels. Both models must be scored on the same ",
             "observations, in the same order.", call. = FALSE)
      }
      if (!is.null(p$label) && nzchar(p$label)) object_labels[m] <- p$label
      models[[m]] <- p$probs
    }
  }
  if (is.null(labels)) {
    labels <- vapply(1:2, function(m) {
      if (!is.na(object_labels[m])) object_labels[m]
      else if (!is.na(arg_names[m])) arg_names[m]
      else paste("Model", m)
    }, character(1))
  }

  # Read the inputs
  if (!is.null(levels)) {
    if (!is.atomic(levels) || anyNA(levels) || anyDuplicated(levels)) {
      stop("levels must be a vector of distinct class labels.", call. = FALSE)
    }
    unknown <- setdiff(unique(as.character(truth[!is.na(truth)])),
                       as.character(levels))
    if (length(unknown)) {
      stop("truth contains labels not in levels: ",
           paste(unknown, collapse = ", "), ".", call. = FALSE)
    }
    truth <- factor(truth, levels = levels)
  }

  for (m in 1:2) {
    p <- models[[m]]
    nm <- paste0("probs", m)
    if (!is.matrix(p) && !is.data.frame(p))
      stop(nm, " must be a matrix or data frame.", call. = FALSE)
    if (!all(vapply(as.data.frame(p), is.numeric, logical(1))))
      stop(nm, " must contain numeric values.", call. = FALSE)
    if (nrow(p) != length(truth))
      stop(nm, " has ", nrow(p), " rows but truth has ",
           length(truth), " values.", call. = FALSE)
  }

  # mc_roc()'s own reader: shape, type and column names.
  inputs <- lapply(1:2, function(m) {
    .mc_with_model_name(m, .mc_prepare(truth, models[[m]]))
  })
  truth <- inputs[[1]]$truth
  n <- length(truth)

  # Rows complete in BOTH models, so the two AUCs use the same rows.
  keep <- .mc_complete(truth, inputs[[1]]$probs) &
    .mc_complete(truth, inputs[[2]]$probs)
  n_used <- sum(keep)
  if (n_used == 0) {
    stop("Every row has a missing label or probability in at least one ",
         "model, so there is nothing to compare.", call. = FALSE)
  }
  lost <- setdiff(base::levels(truth), unique(as.character(truth[keep])))
  if (length(lost)) {
    stop("No rows complete in both models for class(es): ",
         paste(lost, collapse = ", "), ".", call. = FALSE)
  }
  if (n_used < n) {
    warning(sprintf(paste0(
      "%d of %d rows (%.1f%%) have a missing label or probability in at ",
      "least one model and were left out of both, so the comparison stays ",
      "paired."), n - n_used, n, 100 * (n - n_used) / n), call. = FALSE)
  }

  # Fit both models with mc_roc() on the same rows
  truth_k <- droplevels(truth[keep])
  fits <- .mc_collect_warnings(lapply(1:2, function(m) {
    .mc_with_model_name(m, mc_roc(truth_k,
                                  inputs[[m]]$probs[keep, , drop = FALSE],
                                  label = labels[m]))
  }))

  auc1 <- fits[[1]]$auc
  auc2 <- fits[[2]]$auc
  pw1 <- fits[[1]]$pairwise
  pw2 <- fits[[2]]$pairwise
  pairwise <- data.frame(pair = pw1$pair, class1 = pw1$class1,
                         class2 = pw1$class2, auc1 = pw1$auc,
                         auc2 = pw2$auc, difference = pw1$auc - pw2$auc,
                         stringsAsFactors = FALSE)

  # Paired stratified bootstrap
  boot <- .mc_with_seed(seed, mc_boot_auc(fits, n_boot))
  boot_differences <- boot[, 1] - boot[, 2]

  ok       <- boot_differences[!is.na(boot_differences)]
  n_ok     <- length(ok)
  n_failed <- n_boot - n_ok

  if (n_ok < 2)
    stop("Too few successful bootstrap replicates to summarise.", call. = FALSE)
  if (n_ok < n_boot * 0.9)
    warning(n_failed, " of ", n_boot, " bootstrap replicates failed.",
            call. = FALSE)

  alpha <- 1 - conf_level
  ci    <- stats::quantile(ok, c(alpha / 2, 1 - alpha / 2), names = FALSE)

  structure(
    list(auc1             = auc1,
         auc2             = auc2,
         difference       = auc1 - auc2,
         lower_ci         = ci[1],
         upper_ci         = ci[2],
         standard_error   = stats::sd(ok),
         p_value          = min(2 * min(mean(ok >= 0), mean(ok <= 0)), 1),
         conf_level       = conf_level,
         n_boot           = n_boot,
         n_successful     = n_ok,
         n_failed         = n_failed,
         bootstrap_values = ok,
         pairwise         = pairwise,
         labels           = labels,
         n                = n,
         n_used           = n_used),
    class = "mc_auc_compare"
  )
}

# Prefix errors with the model they came from ("probs2: ...").
.mc_with_model_name <- function(m, expr) {
  withCallingHandlers(expr, error = function(e) {
    stop("probs", m, ": ", conditionMessage(e), call. = FALSE)
  })
}

# Run expr, then re-issue its warnings once each (both models share the
# same class-size warnings).
.mc_collect_warnings <- function(expr) {
  seen <- character()
  value <- withCallingHandlers(expr, warning = function(w) {
    seen <<- c(seen, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  for (msg in unique(seen)) warning(msg, call. = FALSE)
  value
}


#' @param x An `mc_auc_compare` object.
#' @param ... Ignored.
#' @rdname mc_auc_compare
#' @export
print.mc_auc_compare <- function(x, ...) {
  lab <- if (is.null(x$labels)) c("Model 1", "Model 2") else x$labels
  w <- max(nchar(c(paste(lab, "AUC"), "Difference", "Bootstrap SE")))
  row <- function(name, value) {
    cat(sprintf("  %-*s : %s\n", w, name, value))
  }

  cat("\nMulticlass AUC comparison (paired stratified bootstrap)\n")
  row(paste(lab[1], "AUC"), sprintf("%.4f", x$auc1))
  row(paste(lab[2], "AUC"), sprintf("%.4f", x$auc2))
  row("Difference", sprintf("%.4f", x$difference))
  row(sprintf("%.0f%% CI", x$conf_level * 100),
      sprintf("%.4f to %.4f", x$lower_ci, x$upper_ci))
  row("Bootstrap SE", sprintf("%.4f", x$standard_error))

  # For display, an empirical p-value of zero is reported as
  # < 1 / n_successful.
  if (x$p_value < 1 / x$n_successful) {
    row("p-value", sprintf("< %.4f", 1 / x$n_successful))
  } else {
    row("p-value", sprintf("%.4f", x$p_value))
  }

  row("Replicates", sprintf("%d successful, %d failed",
                            x$n_successful, x$n_failed))
  if (!is.null(x$n_used) && !is.null(x$n) && x$n_used < x$n) {
    row("Rows used", sprintf("%d of %d (rows with missing values left out)",
                             x$n_used, x$n))
  }
  cat("\n")
  invisible(x)
}
