# ==============================================================================
# Learning R
# Part V: Statistical Inference and Hypothesis Testing
# Lesson 31: Confidence Intervals and Estimation
# Suggested Solutions
# ==============================================================================

set.seed(20260714)

student_data <- read.csv(
  "data/student_scores.csv",
  stringsAsFactors = FALSE
)

final_scores <- student_data$final_score

observed_scores <- final_scores[
  !is.na(final_scores)
]

# Exercise 1: Calculate a point estimate.
sample_mean <- mean(
  observed_scores
)

sample_mean

# Exercise 2: Calculate a standard error.
sample_size <- length(
  observed_scores
)

sample_sd <- sd(
  observed_scores
)

standard_error <-
  sample_sd /
  sqrt(sample_size)

standard_error

# Exercise 3: Construct a manual t interval.
confidence_level <- 0.95
alpha <- 1 -
  confidence_level

critical_t <- qt(
  1 -
    alpha / 2,
  df = sample_size - 1
)

margin_of_error <-
  critical_t *
  standard_error

manual_interval <- c(
  lower =
    sample_mean -
    margin_of_error,
  upper =
    sample_mean +
    margin_of_error
)

manual_interval

# Exercise 4: Verify with t.test().
mean_test <- t.test(
  observed_scores,
  conf.level = 0.95
)

mean_test$conf.int

all.equal(
  unname(
    manual_interval
  ),
  unname(
    mean_test$conf.int
  )
)

# Exercise 5: Compare confidence levels.
confidence_levels <- c(
  0.90,
  0.95,
  0.99
)

confidence_level_comparison <- do.call(
  rbind,
  lapply(
    confidence_levels,
    function(level) {
      result <- t.test(
        observed_scores,
        conf.level = level
      )

      data.frame(
        confidence_level = level,
        estimate =
          mean(observed_scores),
        lower =
          result$conf.int[1],
        upper =
          result$conf.int[2],
        width =
          diff(
            result$conf.int
          )
      )
    }
  )
)

confidence_level_comparison

# Exercise 6: Assess sample-size effects.
population <- rnorm(
  100000,
  mean = 70,
  sd = 12
)

sample_sizes <- c(
  20,
  50,
  200
)

sample_size_intervals <- do.call(
  rbind,
  lapply(
    sample_sizes,
    function(current_size) {
      selected_sample <- sample(
        population,
        size = current_size
      )

      result <- t.test(
        selected_sample
      )

      data.frame(
        sample_size = current_size,
        estimate =
          mean(selected_sample),
        lower =
          result$conf.int[1],
        upper =
          result$conf.int[2],
        width =
          diff(
            result$conf.int
          )
      )
    }
  )
)

sample_size_intervals

# Exercise 7: Construct proportion intervals.
pass_status <-
  student_data$final_score >= 50

number_passing <- sum(
  pass_status,
  na.rm = TRUE
)

number_observed <- sum(
  !is.na(
    pass_status
  )
)

sample_proportion <-
  number_passing /
  number_observed

critical_z <- qnorm(
  0.975
)

proportion_se <- sqrt(
  sample_proportion *
    (
      1 -
      sample_proportion
    ) /
    number_observed
)

wald_interval <- c(
  lower =
    sample_proportion -
    critical_z *
    proportion_se,
  upper =
    sample_proportion +
    critical_z *
    proportion_se
)

score_interval <- prop.test(
  number_passing,
  number_observed,
  correct = FALSE
)$conf.int

exact_interval <- binom.test(
  number_passing,
  number_observed
)$conf.int

data.frame(
  method = c(
    "Wald",
    "Score",
    "Exact"
  ),
  lower = c(
    wald_interval[1],
    score_interval[1],
    exact_interval[1]
  ),
  upper = c(
    wald_interval[2],
    score_interval[2],
    exact_interval[2]
  ),
  stringsAsFactors = FALSE
)

# Exercise 8: Compare prop.test() and binom.test().
proportion_method_comparison <- data.frame(
  method = c(
    "Score interval",
    "Exact binomial interval"
  ),
  estimate = c(
    sample_proportion,
    sample_proportion
  ),
  lower = c(
    score_interval[1],
    exact_interval[1]
  ),
  upper = c(
    score_interval[2],
    exact_interval[2]
  ),
  stringsAsFactors = FALSE
)

proportion_method_comparison

# Exercise 9: Difference-in-means interval.
selected_programmes <- student_data[
  student_data$programme %in%
    c(
      "Statistics",
      "Procurement"
    ),
]

difference_test <- t.test(
  final_score ~ programme,
  data = selected_programmes,
  var.equal = FALSE,
  conf.level = 0.95
)

difference_test

# Exercise 10: Paired confidence interval.
before_scores <- c(
  60,
  65,
  70,
  72,
  68,
  75
)

after_scores <- c(
  64,
  67,
  74,
  73,
  72,
  80
)

paired_test <- t.test(
  after_scores,
  before_scores,
  paired = TRUE,
  conf.level = 0.95
)

paired_test

# Exercise 11: Simulate interval coverage.
population_mean <- 70
population_sd <- 12
coverage_sample_size <- 30
replications <- 3000

coverage_status <- replicate(
  replications,
  {
    current_sample <- rnorm(
      coverage_sample_size,
      mean = population_mean,
      sd = population_sd
    )

    interval <- t.test(
      current_sample,
      conf.level = 0.95
    )$conf.int

    interval[1] <=
      population_mean &&
      population_mean <=
      interval[2]
  }
)

estimated_coverage <- mean(
  coverage_status
)

estimated_coverage

# Exercise 12: Bootstrap percentile interval.
bootstrap_means <- replicate(
  5000,
  mean(
    sample(
      observed_scores,
      size = length(
        observed_scores
      ),
      replace = TRUE
    )
  )
)

bootstrap_interval <- quantile(
  bootstrap_means,
  probs = c(
    0.025,
    0.975
  )
)

bootstrap_interval

# Exercise 13: Reusable interval functions.
mean_confidence_interval <- function(
  x,
  confidence_level = 0.95
) {
  if (!is.numeric(x)) {
    stop(
      "`x` must be numeric.",
      call. = FALSE
    )
  }

  observed <- x[
    !is.na(x)
  ]

  if (length(observed) < 2L) {
    stop(
      "At least two observed values are required.",
      call. = FALSE
    )
  }

  result <- t.test(
    observed,
    conf.level = confidence_level
  )

  data.frame(
    n = length(observed),
    estimate = mean(observed),
    standard_error =
      sd(observed) /
      sqrt(
        length(observed)
      ),
    lower =
      result$conf.int[1],
    upper =
      result$conf.int[2],
    confidence_level =
      confidence_level
  )
}

proportion_confidence_interval <- function(
  successes,
  trials,
  confidence_level = 0.95,
  method = c(
    "exact",
    "score"
  )
) {
  method <- match.arg(
    method
  )

  if (successes < 0 ||
      trials <= 0 ||
      successes > trials) {
    stop(
      "Counts are invalid.",
      call. = FALSE
    )
  }

  result <- if (
    method == "exact"
  ) {
    binom.test(
      successes,
      trials,
      conf.level = confidence_level
    )
  } else {
    prop.test(
      successes,
      trials,
      conf.level = confidence_level,
      correct = FALSE
    )
  }

  data.frame(
    successes = successes,
    trials = trials,
    estimate =
      successes /
      trials,
    lower =
      result$conf.int[1],
    upper =
      result$conf.int[2],
    confidence_level =
      confidence_level,
    method = method
  )
}

mean_confidence_interval(
  final_scores
)

proportion_confidence_interval(
  number_passing,
  number_observed,
  method = "exact"
)

# Exercise 14: Accurate interpretation.
interpretation <- paste(
  "The 95 per cent confidence interval estimates the range produced by a",
  "procedure that would contain the true population parameter in approximately",
  "95 per cent of repeated samples. It does not assign a 95 per cent probability",
  "to the fixed parameter after the interval has been calculated."
)

interpretation

# Practical assessment workflow.
mean_interval_report <- mean_confidence_interval(
  final_scores,
  0.95
)

proportion_interval_report <-
  proportion_confidence_interval(
    number_passing,
    number_observed,
    0.95,
    "score"
  )

method_comparison <- data.frame(
  method = c(
    "t interval",
    "Bootstrap percentile"
  ),
  lower = c(
    mean_test$conf.int[1],
    bootstrap_interval[1]
  ),
  upper = c(
    mean_test$conf.int[2],
    bootstrap_interval[2]
  ),
  stringsAsFactors = FALSE
)

mean_interval_report
proportion_interval_report
method_comparison
estimated_coverage

# ==============================================================================
# End of Lesson 31 Solutions
# ==============================================================================
