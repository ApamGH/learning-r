# ==============================================================================
# Learning R
# Part V: Statistical Inference and Hypothesis Testing
# Lesson 32: Hypothesis Testing and t-Tests
# Suggested Solutions
# ==============================================================================

student_data <- read.csv(
  "data/student_scores.csv",
  stringsAsFactors = FALSE
)

final_scores <- student_data$final_score

# Exercise 1: Formulate hypotheses.
hypotheses <- c(
  "H0: The population mean final score equals 70.",
  "H1: The population mean final score differs from 70."
)

hypotheses

# Exercise 2: One-sample t-test.
one_sample_test <- t.test(
  final_scores,
  mu = 70,
  alternative = "two.sided",
  conf.level = 0.95
)

one_sample_test

# Exercise 3: Calculate the test statistic manually.
observed_scores <- final_scores[
  !is.na(final_scores)
]

sample_mean <- mean(
  observed_scores
)

sample_sd <- sd(
  observed_scores
)

sample_size <- length(
  observed_scores
)

null_mean <- 70

manual_t <- (
  sample_mean -
    null_mean
) / (
  sample_sd /
    sqrt(sample_size)
)

manual_t

all.equal(
  unname(
    manual_t
  ),
  unname(
    one_sample_test$statistic
  )
)

# Exercise 4: Compare one-sided and two-sided tests.
two_sided_test <- t.test(
  observed_scores,
  mu = 70,
  alternative = "two.sided"
)

greater_test <- t.test(
  observed_scores,
  mu = 70,
  alternative = "greater"
)

less_test <- t.test(
  observed_scores,
  mu = 70,
  alternative = "less"
)

data.frame(
  alternative = c(
    "Two-sided",
    "Greater",
    "Less"
  ),
  t = c(
    unname(
      two_sided_test$statistic
    ),
    unname(
      greater_test$statistic
    ),
    unname(
      less_test$statistic
    )
  ),
  p_value = c(
    two_sided_test$p.value,
    greater_test$p.value,
    less_test$p.value
  ),
  stringsAsFactors = FALSE
)

# Exercise 5: Welch and pooled independent tests.
two_programmes <- student_data[
  student_data$programme %in%
    c(
      "Statistics",
      "Procurement"
    ),
]

welch_test <- t.test(
  final_score ~ programme,
  data = two_programmes,
  var.equal = FALSE
)

pooled_test <- t.test(
  final_score ~ programme,
  data = two_programmes,
  var.equal = TRUE
)

welch_test
pooled_test

# Exercise 6: Group summaries.
group_summary <- aggregate(
  final_score ~ programme,
  data = two_programmes,
  FUN = function(x) {
    c(
      n = length(x),
      mean = mean(x),
      sd = sd(x)
    )
  }
)

group_summary

# Exercise 7: Paired t-test.
before_scores <- c(
  60,
  65,
  70,
  72,
  68,
  75,
  66,
  71
)

after_scores <- c(
  64,
  67,
  74,
  73,
  72,
  80,
  70,
  74
)

paired_test <- t.test(
  after_scores,
  before_scores,
  paired = TRUE
)

paired_test

# Exercise 8: Analyse paired differences.
differences <-
  after_scores -
  before_scores

difference_summary <- data.frame(
  n = length(differences),
  mean_difference =
    mean(differences),
  standard_deviation =
    sd(differences),
  minimum =
    min(differences),
  maximum =
    max(differences)
)

difference_summary

difference_test <- t.test(
  differences,
  mu = 0
)

difference_test

# Exercise 9: Inspect assumptions.
qqnorm(
  observed_scores,
  main = "Q-Q Plot of Final Scores"
)

qqline(
  observed_scores
)

qqnorm(
  differences,
  main = "Q-Q Plot of Paired Differences"
)

qqline(
  differences
)

shapiro_one_sample <- shapiro.test(
  observed_scores
)

shapiro_paired <- shapiro.test(
  differences
)

shapiro_one_sample
shapiro_paired

boxplot(
  final_score ~ programme,
  data = two_programmes,
  main = "Final Scores by Programme",
  xlab = "Programme",
  ylab = "Final score"
)

# Exercise 10: Calculate Cohen's d.
one_sample_d <- (
  sample_mean -
    null_mean
) / sample_sd

group_split <- split(
  two_programmes$final_score,
  two_programmes$programme
)

group_names <- names(
  group_split
)

group_1 <- group_split[[
  group_names[1]
]]

group_2 <- group_split[[
  group_names[2]
]]

n_1 <- length(group_1)
n_2 <- length(group_2)

pooled_sd <- sqrt(
  (
    (
      n_1 - 1
    ) *
    var(group_1) +
    (
      n_2 - 1
    ) *
    var(group_2)
  ) /
  (
    n_1 +
    n_2 -
    2
  )
)

independent_d <- (
  mean(group_1) -
    mean(group_2)
) / pooled_sd

paired_d <-
  mean(differences) /
  sd(differences)

data.frame(
  design = c(
    "One-sample",
    "Independent groups",
    "Paired"
  ),
  cohens_d = c(
    one_sample_d,
    independent_d,
    paired_d
  ),
  stringsAsFactors = FALSE
)

# Exercise 11: Explain Type I and Type II errors.
error_types <- data.frame(
  error = c(
    "Type I",
    "Type II"
  ),
  meaning = c(
    "Rejecting a true null hypothesis",
    "Failing to reject a false null hypothesis"
  ),
  probability = c(
    "alpha",
    "beta"
  ),
  stringsAsFactors = FALSE
)

error_types

# Exercise 12: Conduct power calculations.
one_sample_power <- power.t.test(
  n = 30,
  delta = 5,
  sd = 12,
  sig.level = 0.05,
  type = "one.sample",
  alternative = "two.sided"
)

required_sample_size <- power.t.test(
  power = 0.80,
  delta = 5,
  sd = 12,
  sig.level = 0.05,
  type = "one.sample",
  alternative = "two.sided"
)

one_sample_power
required_sample_size

# Exercise 13: Reusable t-test functions.
report_one_sample_t <- function(
  x,
  null_mean,
  confidence_level = 0.95,
  alternative = c(
    "two.sided",
    "less",
    "greater"
  )
) {
  alternative <- match.arg(
    alternative
  )

  observed <- x[
    !is.na(x)
  ]

  result <- t.test(
    observed,
    mu = null_mean,
    alternative = alternative,
    conf.level = confidence_level
  )

  data.frame(
    n = length(observed),
    mean = mean(observed),
    standard_deviation =
      sd(observed),
    null_mean = null_mean,
    mean_difference =
      mean(observed) -
      null_mean,
    t =
      unname(
        result$statistic
      ),
    degrees_of_freedom =
      unname(
        result$parameter
      ),
    p_value =
      result$p.value,
    confidence_lower =
      result$conf.int[1],
    confidence_upper =
      result$conf.int[2],
    cohens_d =
      (
        mean(observed) -
        null_mean
      ) /
      sd(observed)
  )
}

report_independent_t <- function(
  data,
  outcome,
  group,
  confidence_level = 0.95
) {
  if (!all(
    c(
      outcome,
      group
    ) %in%
      names(data)
  )) {
    stop(
      "Required variables are missing.",
      call. = FALSE
    )
  }

  group_values <- unique(
    data[[
      group
    ]][
      !is.na(
        data[[
          group
        ]]
      )
    ]
  )

  if (length(group_values) != 2L) {
    stop(
      "Exactly two groups are required.",
      call. = FALSE
    )
  }

  formula_object <- reformulate(
    group,
    response = outcome
  )

  result <- t.test(
    formula_object,
    data = data,
    var.equal = FALSE,
    conf.level = confidence_level
  )

  data.frame(
    group_1 = names(
      result$estimate
    )[1],
    group_2 = names(
      result$estimate
    )[2],
    estimate_1 =
      unname(
        result$estimate[1]
      ),
    estimate_2 =
      unname(
        result$estimate[2]
      ),
    mean_difference =
      unname(
        result$estimate[1] -
        result$estimate[2]
      ),
    t =
      unname(
        result$statistic
      ),
    degrees_of_freedom =
      unname(
        result$parameter
      ),
    p_value =
      result$p.value,
    confidence_lower =
      result$conf.int[1],
    confidence_upper =
      result$conf.int[2]
  )
}

report_paired_t <- function(
  before,
  after,
  confidence_level = 0.95
) {
  complete_status <- complete.cases(
    before,
    after
  )

  before_complete <- before[
    complete_status
  ]

  after_complete <- after[
    complete_status
  ]

  differences <-
    after_complete -
    before_complete

  result <- t.test(
    after_complete,
    before_complete,
    paired = TRUE,
    conf.level = confidence_level
  )

  data.frame(
    n = length(differences),
    before_mean =
      mean(before_complete),
    after_mean =
      mean(after_complete),
    mean_difference =
      mean(differences),
    t =
      unname(
        result$statistic
      ),
    degrees_of_freedom =
      unname(
        result$parameter
      ),
    p_value =
      result$p.value,
    confidence_lower =
      result$conf.int[1],
    confidence_upper =
      result$conf.int[2],
    cohens_d =
      mean(differences) /
      sd(differences)
  )
}

report_one_sample_t(
  final_scores,
  null_mean = 70
)

report_independent_t(
  two_programmes,
  outcome = "final_score",
  group = "programme"
)

report_paired_t(
  before_scores,
  after_scores
)

# Exercise 14: Complete interpretation.
decision <- if (
  one_sample_test$p.value < 0.05
) {
  "reject"
} else {
  "fail to reject"
}

interpretation <- paste(
  "The one-sample t-test compared the mean final score with 70. The sample",
  "mean was",
  round(
    sample_mean,
    2
  ),
  "and the test statistic was",
  round(
    unname(
      one_sample_test$statistic
    ),
    3
  ),
  "with",
  round(
    unname(
      one_sample_test$parameter
    ),
    2
  ),
  "degrees of freedom. The p-value was",
  format.pval(
    one_sample_test$p.value,
    digits = 3,
    eps = 0.001
  ),
  ", so we",
  decision,
  "the null hypothesis at the 5 per cent level. The effect size and confidence",
  "interval should be considered alongside statistical significance."
)

interpretation

# Practical assessment workflow.
test_assessment <- list(
  one_sample =
    report_one_sample_t(
      final_scores,
      70
    ),
  independent =
    report_independent_t(
      two_programmes,
      "final_score",
      "programme"
    ),
  paired =
    report_paired_t(
      before_scores,
      after_scores
    ),
  power =
    one_sample_power
)

test_assessment

# ==============================================================================
# End of Lesson 32 Solutions
# ==============================================================================
