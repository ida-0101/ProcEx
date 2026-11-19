#' Profile multiclass prediction outliers
#'
#' Identifies unusual prediction probabilities using the IQR method
#' and detects high-confidence incorrect predictions.
#' Users can choose whether to flag, remove, or keep flagged observations.
#'
#' @details
#' Each flagged score is given one of three reasons:
#'
#' * `"IQR outlier"`: the score is below Q1 - `iqr_multiplier` x IQR or above
#'   Q3 + `iqr_multiplier` x IQR, where the quartiles are calculated for each
#'   probability column over all observations.
#' * `"Outside custom limits"`: used instead of the IQR rule when `lower` and
#'   `upper` are given; the score is below `lower` or above `upper`.
#' * `"High-confidence wrong prediction"`: the most likely class is wrong and
#'   its probability is at least 0.90. This check needs the probability
#'   columns to be named after the class labels; otherwise it is skipped with
#'   a warning.
#'
#' How to read IQR flags: the quartiles of a column are taken over all
#' observations, most of which belong to other classes and have a low
#' probability for that column. With four or more classes the upper limit is
#' therefore usually close to 0, and confident correct predictions are
#' flagged as `"IQR outlier"`. These flags mark scores that are unusual within
#' their column, not wrong predictions; the `"High-confidence wrong
#' prediction"` flags are the ones that point to errors.
#'
#' Missing values are not allowed; remove or fill them first.
#'
#' @param truth True class labels: a factor, character or integer vector.
#' @param probs Predicted class probabilities (matrix or data frame), one
#'   column per class.
#' @param iqr_multiplier Multiplier for IQR-based outlier detection
#'   (default 1.5).
#' @param lower Optional custom lower probability limit. Give both `lower`
#'   and `upper` to replace the IQR rule.
#' @param upper Optional custom upper probability limit.
#' @param action What to do with flagged observations:
#'   "flag" (default), "remove", or "keep".
#'
#' @return For `action = "flag"`, a data frame with one row per flagged
#'   score: `observation` (row number), `actual_class`, `score_column`,
#'   `score` and `reason`. An observation can appear more than once. For
#'   `"remove"` or `"keep"`, a list with `action`, `outliers` (that data
#'   frame), `truth` and `probs` (all rows for `"keep"`, the rows that were
#'   not flagged for `"remove"`) and `removed_observations`.
#'
#' @examples
#' fit <- nnet::multinom(Species ~ Sepal.Length + Sepal.Width,
#'                       data = iris, trace = FALSE)
#' probs <- predict(fit, type = "probs")
#'
#' flags <- profile_outliers(iris$Species, probs)
#' table(flags$reason)
#'
#' # Custom limits instead of the IQR rule
#' profile_outliers(iris$Species, probs, lower = 0.01, upper = 0.99)
#'
#' # Remove flagged rows and keep the rest
#' cleaned <- profile_outliers(iris$Species, probs, action = "remove")
#' length(cleaned$removed_observations)
#'
#' @seealso [profile_auc_influence()] for how much each flagged observation
#'   changes the AUC.
#' @export
profile_outliers <- function(truth, probs,
                             iqr_multiplier = 1.5,
                             lower = NULL,
                             upper = NULL,
                             action = "flag") {

  action <- match.arg(action, c("flag", "remove", "keep"))

  # Validate actual class labels
  if (!is.null(dim(truth)) ||
      !(is.factor(truth) || is.character(truth) || is.numeric(truth))) {
    stop("truth must be a factor, character or integer vector.")
  }

  # Validate prediction scores
  if (!is.matrix(probs) && !is.data.frame(probs)) {
    stop("probs must be a numeric matrix or data frame.")
  }

  probs <- as.data.frame(probs)

  if (nrow(probs) != length(truth)) {
    stop("truth and probs must have the same number of observations.")
  }

  if (ncol(probs) < 3) {
    stop("At least three prediction-score columns are required.")
  }

  if (!all(vapply(probs, is.numeric, logical(1)))) {
    stop("All prediction scores must be numeric.")
  }

  if (anyNA(truth) || anyNA(probs)) {
    stop("Missing values detected. Please handle them first.")
  }

  if (any(!is.finite(as.matrix(probs)))) {
    stop("Prediction scores must be finite.")
  }

  if (any(as.matrix(probs) < 0 | as.matrix(probs) > 1)) {
    stop("Prediction probabilities must be between 0 and 1.")
  }

  if (!is.numeric(iqr_multiplier) ||
      length(iqr_multiplier) != 1 ||
      !is.finite(iqr_multiplier) ||
      iqr_multiplier < 0) {
    stop("iqr_multiplier must be a non-negative number.")
  }

  if (xor(is.null(lower), is.null(upper))) {
    stop("Provide both lower and upper, or neither.")
  }

  if (!is.null(lower)) {
    if (!is.numeric(lower) || !is.numeric(upper) ||
        length(lower) != 1 || length(upper) != 1 ||
        !is.finite(lower) || !is.finite(upper) ||
        lower < 0 || upper > 1 || lower >= upper) {
      stop("Custom limits must satisfy 0 <= lower < upper <= 1.")
    }
  }

  # Create an empty results table
  results <- data.frame(
    observation = integer(),
    actual_class = character(),
    score_column = character(),
    score = numeric(),
    reason = character()
  )

  # Detect IQR-based outliers
  for (column in names(probs)) {

    values <- probs[[column]]

    if (is.null(lower)) {
      q1 <- as.numeric(stats::quantile(values, 0.25))
      q3 <- as.numeric(stats::quantile(values, 0.75))

      iqr_value <- q3 - q1

      lower_limit <- q1 - iqr_multiplier * iqr_value
      upper_limit <- q3 + iqr_multiplier * iqr_value

    } else {
      lower_limit <- lower
      upper_limit <- upper
    }

    outliers <- which(
      values < lower_limit | values > upper_limit
    )

    if (length(outliers) > 0) {
      results <- rbind(
        results,
        data.frame(
          observation = as.integer(outliers),
          actual_class = as.character(truth[outliers]),
          score_column = column,
          score = as.numeric(values[outliers]),
          reason = if (is.null(lower)) {
            "IQR outlier"
          } else {
            "Outside custom limits"
          }
        )
      )
    }
  }

  # Detect high-confidence wrong predictions
  confidence_threshold <- 0.90

  if (!is.null(names(probs)) &&
      setequal(names(probs), unique(as.character(truth)))) {

    predicted_class <- names(probs)[
      max.col(probs, ties.method = "first")
    ]

    confidence <- apply(probs, 1, max)

    wrong_high_confidence <- which(
      predicted_class != as.character(truth) &
        confidence >= confidence_threshold
    )

    if (length(wrong_high_confidence) > 0) {
      results <- rbind(
        results,
        data.frame(
          observation = as.integer(wrong_high_confidence),
          actual_class = as.character(truth[wrong_high_confidence]),
          score_column = predicted_class[wrong_high_confidence],
          score = as.numeric(confidence[wrong_high_confidence]),
          reason = "High-confidence wrong prediction"
        )
      )
    }
  } else {
    warning(
      paste(
        "Probability column names do not match the class labels, so",
        "high-confidence wrong predictions were not checked."
      )
    )
  }

  # Default action: only display flagged observations
  if (action == "flag") {
    return(results)
  }

  # Get unique observation numbers
  flagged_rows <- sort(unique(results$observation))

  # Remove or retain flagged observations
  if (action == "remove") {
    selected_rows <- setdiff(
      seq_len(nrow(probs)),
      flagged_rows
    )
  } else {
    selected_rows <- seq_len(nrow(probs))
  }

  # Return selected data without changing the original input
  return(list(
    action = action,
    outliers = results,
    truth = truth[selected_rows],
    probs = probs[selected_rows, , drop = FALSE],
    removed_observations = if (action == "remove") {
      flagged_rows
    } else {
      integer()
    }
  ))
}


#' Calculate the influence of flagged observations on multiclass AUC
#'
#' Identifies flagged observations and calculates how much the
#' Hand-Till multiclass AUC changes when each observation is
#' temporarily excluded, one at a time.
#'
#' @details
#' Observations are flagged with [profile_outliers()] using the same
#' settings; see its help page for how the flags are chosen. For each flagged
#' observation the AUC is recalculated without it. An observation whose
#' removal would leave a class empty is skipped. The original data are not
#' modified.
#'
#' The AUC is recalculated once per flagged observation, so the time taken
#' grows with both the number of flags and the number of rows.
#'
#' @param truth True class labels: a factor, character or integer vector.
#' @param probs Predicted class probabilities (matrix or data frame), one
#'   column per class, named exactly as the class labels.
#' @param iqr_multiplier Multiplier for IQR-based outlier detection.
#' @param lower Optional custom lower probability limit.
#' @param upper Optional custom upper probability limit.
#'
#' @return A data frame with one row per evaluated observation, sorted by the
#'   size of the change: `observation`, `actual_class`, `original_auc`,
#'   `auc_without` (AUC without that observation) and `auc_change`
#'   (`auc_without - original_auc`).
#'
#' @examples
#' fit <- nnet::multinom(Species ~ Sepal.Length + Sepal.Width,
#'                       data = iris, trace = FALSE)
#' probs <- predict(fit, type = "probs")
#'
#' influence <- profile_auc_influence(iris$Species, probs)
#' head(influence)
#'
#' @seealso [profile_outliers()], [profile_auc_combinations()]
#' @export
profile_auc_influence <- function(truth, probs,
                                  iqr_multiplier = 1.5,
                                  lower = NULL,
                                  upper = NULL) {

  # Identify outliers without removing them
  flagged <- profile_outliers(
    truth = truth,
    probs = probs,
    iqr_multiplier = iqr_multiplier,
    lower = lower,
    upper = upper,
    action = "flag"
  )

  probs <- as.matrix(probs)
  truth <- factor(truth)

  if (nlevels(truth) < 3) {
    stop("At least three classes are required.")
  }

  if (is.null(colnames(probs)) ||
      !setequal(colnames(probs), levels(truth))) {
    stop("Prediction columns must match the class labels.")
  }

  probs <- probs[, levels(truth), drop = FALSE]

  # Calculate Hand-Till multiclass AUC
  auc_value <- function(y, p) {
    as.numeric(pROC::multiclass.roc(
      response = y,
      predictor = p,
      levels = levels(truth),
      direction = ">"
    )$auc)
  }

  original_auc <- auc_value(truth, probs)

  # Avoid evaluating the same observation multiple times
  observations <- unique(flagged$observation)

  results <- data.frame(
    observation = integer(),
    actual_class = character(),
    original_auc = numeric(),
    auc_without = numeric(),
    auc_change = numeric()
  )

  for (i in observations) {

    keep <- seq_along(truth) != i

    # Skip removal if it would eliminate a class
    if (any(table(truth[keep]) == 0)) {
      next
    }

    new_auc <- auc_value(
      truth[keep],
      probs[keep, , drop = FALSE]
    )

    results <- rbind(
      results,
      data.frame(
        observation = as.integer(i),
        actual_class = as.character(truth[i]),
        original_auc = original_auc,
        auc_without = new_auc,
        auc_change = new_auc - original_auc
      )
    )
  }

  # Rank observations by absolute AUC change
  results[
    order(-abs(results$auc_change)),
    ,
    drop = FALSE
  ]
}


#' Analyse combinations of influential multiclass outliers
#'
#' Selects the top influential observations and checks
#' all possible combinations using Hand-Till AUC.
#'
#' @details
#' The influence of each flagged observation is first calculated with
#' [profile_auc_influence()]. The `top_n` observations with the largest
#' absolute change are then removed in every possible combination (for
#' `top_n = 5`, 31 combinations) and the AUC is recalculated each time.
#' Combinations that would leave a class empty are skipped.
#'
#' @param truth True class labels: a factor, character or integer vector.
#' @param probs Predicted class probabilities (matrix or data frame), one
#'   column per class, named exactly as the class labels.
#' @param top_n Number of most influential observations to combine, a whole
#'   number from 1 to 10 (default 5).
#' @param warn_threshold Warn when more than this many observations are
#'   flagged, because the influence step may then be slow (default 100).
#' @param iqr_multiplier Multiplier for IQR-based outlier detection.
#' @param lower Optional custom lower probability limit.
#' @param upper Optional custom upper probability limit.
#'
#' @return A list with `individual` (the [profile_auc_influence()] result),
#'   `combinations` (a data frame with `observations`, `combination_size`,
#'   `original_auc`, `auc_without` and `auc_change`, sorted by the size of the
#'   change), `selected_observations`, `flagged_count` and `original_auc`.
#'
#' @examples
#' fit <- nnet::multinom(Species ~ Sepal.Length + Sepal.Width,
#'                       data = iris, trace = FALSE)
#' probs <- predict(fit, type = "probs")
#'
#' combos <- profile_auc_combinations(iris$Species, probs, top_n = 3)
#' combos$selected_observations
#' head(combos$combinations)
#'
#' @seealso [profile_auc_influence()], [profile_outliers()]
#' @export
profile_auc_combinations <- function(
    truth, probs,
    top_n = 5,
    warn_threshold = 100,
    iqr_multiplier = 1.5,
    lower = NULL,
    upper = NULL) {

  # Check settings
  if (!is.numeric(top_n) || length(top_n) != 1 ||
      !is.finite(top_n) || top_n != floor(top_n) ||
      top_n < 1 || top_n > 10) {
    stop("top_n must be a whole number between 1 and 10.")
  }

  if (!is.numeric(warn_threshold) ||
      length(warn_threshold) != 1 ||
      !is.finite(warn_threshold) ||
      warn_threshold != floor(warn_threshold) ||
      warn_threshold < 0) {
    stop("warn_threshold must be a non-negative whole number.")
  }

  # Identify flagged observations
  flagged <- profile_outliers(
    truth = truth,
    probs = probs,
    iqr_multiplier = iqr_multiplier,
    lower = lower,
    upper = upper,
    action = "flag"
  )

  flagged_count <- length(unique(flagged$observation))

  # Warn if there are too many flagged observations
  if (flagged_count > warn_threshold) {
    warning(
      paste(
        flagged_count, "unique observations were flagged.",
        "Influence analysis may take a long time.",
        "Consider using a smaller sample."
      )
    )
  }

  # Calculate individual influence
  individual <- profile_auc_influence(
    truth = truth,
    probs = probs,
    iqr_multiplier = iqr_multiplier,
    lower = lower,
    upper = upper
  )

  # Prepare empty results
  empty <- data.frame(
    observations = character(),
    combination_size = integer(),
    original_auc = numeric(),
    auc_without = numeric(),
    auc_change = numeric()
  )

  if (nrow(individual) == 0) {
    return(list(
      individual = individual,
      combinations = empty,
      selected_observations = integer(),
      flagged_count = flagged_count,
      original_auc = NA_real_
    ))
  }

  # Select the top_n most influential observations
  selected <- utils::head(individual$observation, top_n)
  selected <- as.integer(selected)

  original_auc <- individual$original_auc[1]

  truth <- factor(truth)
  probs <- as.matrix(probs)

  probs <- probs[, levels(truth), drop = FALSE]

  # Generate every possible combination
  combinations_list <- list()

  for (k in seq_along(selected)) {
    combinations_list <- c(
      combinations_list,
      utils::combn(seq_along(selected), k, simplify = FALSE)
    )
  }

  combinations_list <- lapply(
    combinations_list,
    function(i) selected[i]
  )

  results <- empty

  # Calculate AUC for each combination
  for (group in combinations_list) {

    keep <- !(seq_along(truth) %in% group)

    # Skip if removing observations eliminates a class
    if (any(table(truth[keep]) == 0)) {
      next
    }

    new_auc <- as.numeric(
      pROC::multiclass.roc(
        response = truth[keep],
        predictor = probs[keep, , drop = FALSE],
        levels = levels(truth),
        direction = ">"
      )$auc
    )

    results <- rbind(
      results,
      data.frame(
        observations = paste(group, collapse = ", "),
        combination_size = length(group),
        original_auc = original_auc,
        auc_without = new_auc,
        auc_change = new_auc - original_auc
      )
    )
  }

  # Rank combinations by absolute AUC change
  results <- results[
    order(-abs(results$auc_change)),
    ,
    drop = FALSE
  ]

  rownames(results) <- NULL

  return(list(
    individual = individual,
    combinations = results,
    selected_observations = selected,
    flagged_count = flagged_count,
    original_auc = original_auc
  ))
}
