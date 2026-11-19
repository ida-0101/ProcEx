#' Handle missing values in multiclass ROC data
#'
#' Detects observations affected by missing values in an `mc_roc` object,
#' then either removes them or fills the missing probabilities, and
#' recalculates the AUC on the cleaned data.
#'
#' @details
#' [mc_roc()] keeps rows with missing values in the object but leaves them
#' out of the AUC. `handle_missing()` decides what happens to those rows:
#'
#' * `"remove"` drops every row with a missing label or a missing
#'   probability.
#' * `"mean"` and `"median"` drop rows with a missing label (a true class
#'   cannot be guessed), then fill each missing probability with the mean or
#'   median of the observed values in its column. Each filled row is then
#'   divided by its total so that it sums to 1 again.
#'
#' After cleaning, the object is rebuilt: the AUC, the pairwise AUCs, the
#' class counts and the row counts (`n`, `n_used`) all describe the cleaned
#' data. A warning reports how many rows were removed or filled, and the
#' `missing_info` component records the details. If removing rows would
#' leave a class with no observations, the function stops with an error.
#'
#' @param x An object of class `mc_roc`.
#' @param method Method used to handle missing values. One of
#'   `"remove"` (default), `"mean"`, or `"median"`.
#'
#' @return An updated `mc_roc` object built from the cleaned data. Its
#'   `missing_info` component is a list with `n_affected` (rows with any
#'   missing value), `pct_affected` (their percentage), `n_removed`,
#'   `n_imputed` and `method`. If there are no missing values, `x` is
#'   returned unchanged apart from `missing_info`.
#'
#' @examples
#' set.seed(1)
#' truth  <- factor(sample(c("a", "b", "c"), 150, replace = TRUE))
#' scores <- matrix(stats::rnorm(450), 150, 3) +
#'   1.5 * outer(as.integer(truth), 1:3, "==")
#' probs  <- exp(scores) / rowSums(exp(scores))
#' colnames(probs) <- levels(truth)
#' probs[c(4, 20, 77), "b"] <- NA
#'
#' fit <- mc_roc(truth, probs)            # warns: 3 rows left out of the AUC
#' cleaned <- handle_missing(fit, "remove")
#' cleaned$auc
#' cleaned$missing_info
#'
#' filled <- handle_missing(fit, "median")
#' filled$n_used                            # all 150 rows are used
#'
#' @seealso [mc_roc()]
#' @export
handle_missing <- function(
    x,
    method = c("remove", "mean", "median")
) {

  method <- match.arg(method)

  # Check input object

  if (!inherits(x, "mc_roc")) {
    stop("x must be an object of class 'mc_roc'.")
  }

  # Check required components

  if (is.null(x$truth) || is.null(x$probs)) {
    stop("x must contain 'truth' and 'probs' components.")
  }

  # Check probability structure

  if (!is.data.frame(x$probs) && !is.matrix(x$probs)) {
    stop("x$probs must be a data frame or matrix.")
  }

  if (length(x$truth) != nrow(x$probs)) {
    stop(
      "x$truth and x$probs must contain the same number of observations."
    )
  }

  # Probability columns must be numeric for imputation

  numeric_cols <- vapply(
    as.data.frame(x$probs),
    is.numeric,
    logical(1)
  )

  if (!all(numeric_cols)) {
    stop("All probability columns must be numeric.")
  }

  # Detect observations affected by missing values

  truth_missing <- is.na(x$truth)

  probs_missing <- apply(
    is.na(x$probs),
    1,
    any
  )

  affected <- truth_missing | probs_missing

  n_affected <- sum(affected)

  pct_affected <- if (length(affected) > 0) {
    100 * n_affected / length(affected)
  } else {
    0
  }

  # Return unchanged object if there are no missing values

  if (n_affected == 0) {

    x$missing_info <- list(
      n_affected = 0L,
      pct_affected = 0,
      n_removed = 0L,
      n_imputed = 0L,
      method = method
    )

    return(x)
  }

  # Remove all incomplete observations

  if (method == "remove") {

    warning(
      paste0(
        n_affected,
        " observation(s) containing missing values were removed (",
        round(pct_affected, 2),
        "% of the data)."
      )
    )

    .mc_stop_if_class_lost(x$truth, !affected)

    x$truth <- x$truth[!affected]

    x$probs <- x$probs[
      !affected,
      ,
      drop = FALSE
    ]

    x$missing_info <- list(
      n_affected = n_affected,
      pct_affected = pct_affected,
      n_removed = n_affected,
      n_imputed = 0L,
      method = method
    )

    # Recalculate the AUC and counts for the cleaned data
    return(mc_refit(x))
  }

  # Mean or median imputation

  # Missing true labels cannot be safely imputed.
  # Remove those observations first.

  n_truth_removed <- sum(truth_missing)

  if (n_truth_removed > 0) {

    warning(
      paste0(
        n_truth_removed,
        " observation(s) with missing true class labels were removed."
      )
    )

    keep <- !truth_missing

    .mc_stop_if_class_lost(x$truth, keep)

    x$truth <- x$truth[keep]

    x$probs <- x$probs[
      keep,
      ,
      drop = FALSE
    ]
  }

  # Count rows that still contain missing probabilities

  probs_missing_after <- apply(
    is.na(x$probs),
    1,
    any
  )

  n_imputed <- sum(probs_missing_after)

  # Impute each probability column separately

  if (n_imputed > 0) {

    for (j in seq_len(ncol(x$probs))) {

      missing_j <- is.na(x$probs[, j])

      if (any(missing_j)) {

        observed_values <- x$probs[!missing_j, j]

        if (length(observed_values) == 0) {
          stop(
            paste0(
              "Cannot impute probability column ",
              j,
              " because all values are missing."
            )
          )
        }

        fill_value <- if (method == "mean") {
          mean(observed_values)
        } else {
          stats::median(observed_values)
        }

        x$probs[missing_j, j] <- fill_value
      }
    }

    # Renormalise affected probability rows so that probabilities sum to 1

    rows_to_normalise <- which(probs_missing_after)

    row_totals <- rowSums(
      x$probs[
        rows_to_normalise,
        ,
        drop = FALSE
      ]
    )

    if (any(!is.finite(row_totals)) || any(row_totals <= 0)) {
      stop(
        "Imputed probabilities could not be normalised."
      )
    }

    x$probs[
      rows_to_normalise,
    ] <-
      x$probs[
        rows_to_normalise,
        ,
        drop = FALSE
      ] / row_totals

    warning(
      paste0(
        n_imputed,
        " observation(s) had missing probability values filled using ",
        method,
        " imputation."
      )
    )
  }

  # Store missing-data information

  x$missing_info <- list(
    n_affected = n_affected,
    pct_affected = pct_affected,
    n_removed = n_truth_removed,
    n_imputed = n_imputed,
    method = method
  )

  # Recalculate the AUC and counts for the cleaned data
  return(mc_refit(x))
}

# Stop if keeping only the rows in `keep` would leave a class with no
# observations.
.mc_stop_if_class_lost <- function(truth, keep) {
  observed <- unique(as.character(truth[!is.na(truth)]))
  remaining <- unique(as.character(truth[keep & !is.na(truth)]))
  lost <- setdiff(observed, remaining)
  if (length(lost) > 0) {
    stop(
      paste0(
        "Removing rows with missing values would leave no observations ",
        "for class(es): ", paste(lost, collapse = ", "), "."
      ),
      call. = FALSE
    )
  }
  invisible(NULL)
}
