# ==============================================================================
# Learning R
# Part IV: Descriptive Analysis and Data Visualisation
# Lesson 28: Distribution Assessment and Exploratory Data Analysis
# Suggested Solutions
# ==============================================================================

student_data <- read.csv(
  "data/student_scores.csv",
  stringsAsFactors = FALSE
)

final_scores <- student_data$final_score

# Exercise 1: Complete numerical summary.
calculate_skewness <- function(
  x
) {
  observed <- x[
    !is.na(x)
  ]

  n <- length(observed)

  if (n < 3L) {
    return(NA_real_)
  }

  centre <- mean(observed)
  spread <- sd(observed)

  if (spread == 0) {
    return(0)
  }

  mean(
    (
      (observed - centre) /
        spread
    ) ^ 3
  )
}

calculate_excess_kurtosis <- function(
  x
) {
  observed <- x[
    !is.na(x)
  ]

  n <- length(observed)

  if (n < 4L) {
    return(NA_real_)
  }

  centre <- mean(observed)
  spread <- sd(observed)

  if (spread == 0) {
    return(NA_real_)
  }

  mean(
    (
      (observed - centre) /
        spread
    ) ^ 4
  ) - 3
}

create_eda_summary <- function(
  x,
  variable_name = "x"
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

  q <- quantile(
    observed,
    c(
      0.25,
      0.50,
      0.75
    )
  )

  data.frame(
    variable = variable_name,
    n = length(observed),
    missing = sum(
      is.na(x)
    ),
    mean = mean(observed),
    median = median(observed),
    standard_deviation = sd(observed),
    minimum = min(observed),
    first_quartile = q[1],
    third_quartile = q[3],
    maximum = max(observed),
    skewness =
      calculate_skewness(
        observed
      ),
    excess_kurtosis =
      calculate_excess_kurtosis(
        observed
      ),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

create_eda_summary(
  final_scores,
  "final_score"
)

# Exercise 2: Compare mean and median.
mean_score <- mean(
  final_scores,
  na.rm = TRUE
)

median_score <- median(
  final_scores,
  na.rm = TRUE
)

data.frame(
  mean = mean_score,
  median = median_score,
  difference =
    mean_score -
    median_score
)

# Exercise 3: Histograms with different breaks.
hist(
  final_scores,
  breaks = 5,
  main = "Final Scores: Five Bins",
  xlab = "Final score"
)

hist(
  final_scores,
  breaks = 12,
  main = "Final Scores: Twelve Bins",
  xlab = "Final score"
)

# Exercise 4: Density and box plots.
plot(
  density(
    final_scores,
    na.rm = TRUE
  ),
  main = "Density of Final Scores",
  xlab = "Final score",
  ylab = "Density"
)

boxplot(
  final_scores,
  main = "Box Plot of Final Scores",
  ylab = "Final score"
)

# Exercise 5: IQR and z-score outlier screening.
q1 <- quantile(
  final_scores,
  0.25,
  na.rm = TRUE
)

q3 <- quantile(
  final_scores,
  0.75,
  na.rm = TRUE
)

iqr_value <- q3 - q1

lower_fence <- q1 - 1.5 * iqr_value
upper_fence <- q3 + 1.5 * iqr_value

iqr_outlier <- final_scores < lower_fence |
  final_scores > upper_fence

z_scores <- as.numeric(
  scale(
    final_scores
  )
)

z_outlier <- abs(
  z_scores
) > 3

outlier_report <- student_data[
  iqr_outlier |
    z_outlier,
]

outlier_report

# Exercise 6: Skewness and kurtosis.
calculate_skewness(
  final_scores
)

calculate_excess_kurtosis(
  final_scores
)

# Exercise 7: Q-Q plot.
qqnorm(
  final_scores,
  main = "Normal Q-Q Plot of Final Scores"
)

qqline(
  final_scores
)

# Exercise 8: Shapiro-Wilk test.
shapiro_result <- shapiro.test(
  final_scores
)

shapiro_result

normality_interpretation <- if (
  shapiro_result$p.value < 0.05
) {
  paste(
    "There is evidence against normality at the 5 per cent level.",
    "The conclusion should be considered together with the Q-Q plot."
  )
} else {
  paste(
    "There is insufficient evidence against normality at the 5 per cent level.",
    "This does not prove that the distribution is normal."
  )
}

normality_interpretation

# Exercise 9: Compare distributions across groups.
boxplot(
  final_score ~ programme,
  data = student_data,
  main = "Final Scores by Programme",
  xlab = "Programme",
  ylab = "Final score",
  las = 2
)

programme_names <- unique(
  student_data$programme
)

group_summaries <- lapply(
  programme_names,
  function(programme_name) {
    selected_scores <- student_data$final_score[
      student_data$programme ==
        programme_name
    ]

    result <- create_eda_summary(
      selected_scores,
      "final_score"
    )

    result$programme <- programme_name

    result
  }
)

group_summary_table <- do.call(
  rbind,
  group_summaries
)

group_summary_table[
  c(
    "programme",
    setdiff(
      names(group_summary_table),
      "programme"
    )
  )
]

# Exercise 10: Assess transformations.
set.seed(20260714)

skewed_values <- rexp(
  n = 200,
  rate = 0.10
)

log_values <- log(
  skewed_values
)

square_root_values <- sqrt(
  skewed_values
)

transformation_summary <- data.frame(
  version = c(
    "Original",
    "Log",
    "Square root"
  ),
  skewness = c(
    calculate_skewness(
      skewed_values
    ),
    calculate_skewness(
      log_values
    ),
    calculate_skewness(
      square_root_values
    )
  ),
  stringsAsFactors = FALSE
)

transformation_summary

old_parameters <- par(
  mfrow = c(
    1,
    3
  )
)

hist(
  skewed_values,
  main = "Original",
  xlab = "Value"
)

hist(
  log_values,
  main = "Log",
  xlab = "Log value"
)

hist(
  square_root_values,
  main = "Square root",
  xlab = "Square-root value"
)

par(
  old_parameters
)

# Exercise 11: Audit missingness and duplicate identifiers.
missingness_report <- data.frame(
  variable = names(student_data),
  missing = vapply(
    student_data,
    function(x) {
      sum(
        is.na(x)
      )
    },
    FUN.VALUE = integer(1)
  ),
  missing_percentage = vapply(
    student_data,
    function(x) {
      mean(
        is.na(x)
      ) * 100
    },
    FUN.VALUE = numeric(1)
  ),
  stringsAsFactors = FALSE
)

duplicate_identifiers <- duplicated(
  student_data$student_id
) |
  duplicated(
    student_data$student_id,
    fromLast = TRUE
  )

missingness_report

student_data[
  duplicate_identifiers,
]

# Exercise 12: EDA report for every numerical variable.
numeric_variables <- names(
  student_data
)[
  vapply(
    student_data,
    is.numeric,
    FUN.VALUE = logical(1)
  )
]

eda_tables <- lapply(
  numeric_variables,
  function(variable_name) {
    create_eda_summary(
      student_data[[
        variable_name
      ]],
      variable_name
    )
  }
)

complete_eda_table <- do.call(
  rbind,
  eda_tables
)

complete_eda_table

# Exercise 13: Exploratory versus confirmatory findings.
analysis_stage_comparison <- data.frame(
  stage = c(
    "Exploratory",
    "Confirmatory"
  ),
  purpose = c(
    "Discover patterns, anomalies and possible questions",
    "Evaluate pre-specified hypotheses"
  ),
  interpretation = c(
    "Tentative and hypothesis-generating",
    "Based on planned inferential procedures"
  ),
  stringsAsFactors = FALSE
)

analysis_stage_comparison

# Practical assessment workflow.
range_checks <- list(
  coursework_valid = all(
    student_data$coursework_score >= 0 &
      student_data$coursework_score <= 40
  ),
  examination_valid = all(
    student_data$examination_score >= 0 &
      student_data$examination_score <= 60
  ),
  final_valid = all(
    student_data$final_score >= 0 &
      student_data$final_score <= 100
  ),
  arithmetic_valid = all(
    student_data$final_score ==
      student_data$coursework_score +
      student_data$examination_score
  )
)

eda_assessment <- list(
  numerical_summary =
    complete_eda_table,
  missingness =
    missingness_report,
  range_checks =
    range_checks,
  shapiro_wilk =
    shapiro_result,
  possible_outliers =
    outlier_report
)

eda_assessment

# ==============================================================================
# End of Lesson 28 Solutions
# ==============================================================================
