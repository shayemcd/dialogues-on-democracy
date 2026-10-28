#' Simulated example data
#'
#' A small simulated data set so the examples in the documentation and
#' vignettes run without any files. It has repeated measures (`k`
#' observations for each of `n_id` participants), a three-level
#' between-person `condition`, a continuous and a binary outcome, and a
#' random participant intercept, which makes it suitable for mixed models.
#' The effects are arbitrary; do not read anything into them.
#'
#' @param n_id Number of participants.
#' @param k Observations per participant.
#' @param seed Random seed. The global random-number state is left unchanged.
#' @return A data frame with `n_id * k` rows and columns `id` (factor),
#'   `condition` (factor: Control, Gain, Loss), `age`, `female` (logical),
#'   `trust` and `score` (continuous outcomes), and `share` and `bought`
#'   (binary outcomes).
#' @examples
#' d <- cr_example_data()
#' head(d)
#' res <- clean_regress(d, dvs = c(trust = "linear"), predictors = ~ condition)
#' res
#' @export
cr_example_data <- function(n_id = 80, k = 4, seed = 42) {
  old <- if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    get(".Random.seed", envir = globalenv())
  }
  on.exit({
    if (is.null(old)) {
      if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
        rm(".Random.seed", envir = globalenv())
      }
    } else {
      assign(".Random.seed", old, envir = globalenv())
    }
  }, add = TRUE)
  set.seed(seed)
  d <- data.frame(
    id = factor(rep(seq_len(n_id), each = k)),
    condition = factor(rep(sample(c("Control", "Gain", "Loss"), n_id, TRUE), each = k),
                       levels = c("Control", "Gain", "Loss")),
    age = rep(round(stats::rnorm(n_id, 40, 12)), each = k),
    female = rep(stats::rbinom(n_id, 1, .5) == 1, each = k)
  )
  u <- stats::rnorm(n_id)[d$id]
  d$trust <- 3 + 0.4 * (d$condition == "Gain") + 0.01 * d$age + u + stats::rnorm(nrow(d))
  d$share <- stats::rbinom(nrow(d), 1, stats::plogis(-0.3 + 0.7 * (d$condition == "Loss") + 0.5 * u))
  d$score <- 2 + 0.5 * (d$condition == "Loss") + stats::rnorm(nrow(d))
  d$bought <- stats::rbinom(nrow(d), 1, stats::plogis(-0.2 + 0.6 * (d$condition == "Gain")))
  d
}
