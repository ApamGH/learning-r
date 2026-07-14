# ==============================================================================
# Learning R
# Part IV: Descriptive Analysis and Data Visualisation
# Lesson 29: Correlation, Covariance and Visual Association
# Suggested Solutions
# ==============================================================================

student_data <- read.csv(
  "data/student_scores.csv",
  stringsAsFactors = FALSE
)

# Exercise 1: Covariance.
coursework_examination_covariance <- cov(
  student_data$coursework_score,
  student_data$examination_score,
  use = "complete.obs"
)

coursework_examination_covariance

# Exercise 2: Pearson correlation.
pearson_correlation <- cor(
  student_data$coursework_score,
  student_data$examination_score,
  method = "pearson",
  use = "complete.obs"
)

pearson_correlation

# Exercise 3: Spearman correlation.
spearman_correlation <- cor(
  student_data$coursework_score,
  student_data$examination_score,
  method = "spearman",
  use = "complete.obs"
)

spearman_correlation

# Exercise 4: Kendall correlation.
kendall_correlation <- cor(
  student_data$coursework_score,
  student_data$examination_score,
  method = "kendall",
  use = "complete.obs"
)

kendall_correlation

# Exercise 5: Compare methods.
correlation_comparison <- data.frame(
  method = c(
    "Pearson",
    "Spearman",
    "Kendall"
  ),
  coefficient = c(
    pearson_correlation,
    spearman_correlation,
    kendall_correlation
  ),
  stringsAsFactors = FALSE
)

correlation_comparison

# Exercise 6: Monotonic versus linear association.
x <- 1:20
y <- x ^ 2

monotonic_comparison <- data.frame(
  method = c(
    "Pearson",
    "Spearman"
  ),
  correlation = c(
    cor(
      x,
      y,
      method = "pearson"
    ),
    cor(
      x,
      y,
      method = "spearman"
    )
  ),
  stringsAsFactors = FALSE
)

monotonic_comparison

plot(
  x,
  y,
  main = "Monotonic but Non-linear Association",
  xlab = "x",
  ylab = "x squared",
  pch = 19
)

# Exercise 7: Non-linear association with near-zero correlation.
x_curve <- seq(
  -3,
  3,
  length.out = 100
)

y_curve <- x_curve ^ 2

curve_correlation <- cor(
  x_curve,
  y_curve
)

curve_correlation

plot(
  x_curve,
  y_curve,
  main = "Non-linear Association",
  xlab = "x",
  ylab = "x squared",
  pch = 19
)

# Exercise 8: Outlier influence.
x_regular <- 1:10
y_regular <- 2 * x_regular

x_with_outlier <- c(
  x_regular,
  20
)

y_with_outlier <- c(
  y_regular,
  -20
)

outlier_influence <- data.frame(
  statistic = c(
    "Pearson without outlier",
    "Pearson with outlier",
    "Spearman with outlier"
  ),
  value = c(
    cor(
      x_regular,
      y_regular
    ),
    cor(
      x_with_outlier,
      y_with_outlier
    ),
    cor(
      x_with_outlier,
      y_with_outlier,
      method = "spearman"
    )
  ),
  stringsAsFactors = FALSE
)

outlier_influence

# Exercise 9: Correlation and covariance matrices.
numeric_student_data <- student_data[
  vapply(
    student_data,
    is.numeric,
    FUN.VALUE = logical(1)
  )
]

correlation_matrix <- cor(
  numeric_student_data,
  use = "pairwise.complete.obs",
  method = "pearson"
)

covariance_matrix <- cov(
  numeric_student_data,
  use = "pairwise.complete.obs"
)

round(
  correlation_matrix,
  3
)

round(
  covariance_matrix,
  3
)

# Exercise 10: Correlation tests.
pearson_test <- cor.test(
  student_data$coursework_score,
  student_data$examination_score,
  method = "pearson"
)

spearman_test <- cor.test(
  student_data$coursework_score,
  student_data$examination_score,
  method = "spearman",
  exact = FALSE
)

kendall_test <- cor.test(
  student_data$coursework_score,
  student_data$examination_score,
  method = "kendall",
  exact = FALSE
)

pearson_test
spearman_test
kendall_test

# Exercise 11: Extract confidence interval.
pearson_report <- data.frame(
  method = "Pearson",
  estimate =
    unname(
      pearson_test$estimate
    ),
  statistic =
    unname(
      pearson_test$statistic
    ),
  degrees_of_freedom =
    unname(
      pearson_test$parameter
    ),
  p_value =
    pearson_test$p.value,
  confidence_lower =
    pearson_test$conf.int[1],
  confidence_upper =
    pearson_test$conf.int[2],
  stringsAsFactors = FALSE
)

pearson_report

# Exercise 12: Grouped correlations.
calculate_grouped_correlation <- function(
  data,
  x_variable,
  y_variable,
  group_variable,
  method = "pearson"
) {
  required_variables <- c(
    x_variable,
    y_variable,
    group_variable
  )

  if (!all(
    required_variables %in%
      names(data)
  )) {
    stop(
      "Required variables are missing.",
      call. = FALSE
    )
  }

  groups <- split(
    data,
    data[[
      group_variable
    ]]
  )

  results <- lapply(
    names(groups),
    function(group_name) {
      group_data <- groups[[
        group_name
      ]]

      complete_status <- complete.cases(
        group_data[
          c(
            x_variable,
            y_variable
          )
        ]
      )

      n_complete <- sum(
        complete_status
      )

      estimate <- if (
        n_complete >= 3L
      ) {
        cor(
          group_data[[
            x_variable
          ]][
            complete_status
          ],
          group_data[[
            y_variable
          ]][
            complete_status
          ],
          method = method
        )
      } else {
        NA_real_
      }

      data.frame(
        group = group_name,
        n = n_complete,
        correlation = estimate,
        stringsAsFactors = FALSE
      )
    }
  )

  result <- do.call(
    rbind,
    results
  )

  names(result)[1] <-
    group_variable

  result
}

grouped_correlations <- calculate_grouped_correlation(
  student_data,
  "coursework_score",
  "examination_score",
  "programme"
)

grouped_correlations

# Exercise 13: Reusable correlation-report functions.
create_correlation_report <- function(
  x,
  y,
  x_name = "x",
  y_name = "y",
  method = c(
    "pearson",
    "spearman",
    "kendall"
  )
) {
  method <- match.arg(
    method
  )

  complete_status <- complete.cases(
    x,
    y
  )

  x_complete <- x[
    complete_status
  ]

  y_complete <- y[
    complete_status
  ]

  test_result <- cor.test(
    x_complete,
    y_complete,
    method = method,
    exact = FALSE
  )

  report <- data.frame(
    variable_x = x_name,
    variable_y = y_name,
    method = method,
    n = length(
      x_complete
    ),
    estimate =
      unname(
        test_result$estimate
      ),
    p_value =
      test_result$p.value,
    stringsAsFactors = FALSE
  )

  if (!is.null(
    test_result$conf.int
  )) {
    report$confidence_lower <-
      test_result$conf.int[1]

    report$confidence_upper <-
      test_result$conf.int[2]
  }

  report
}

create_correlation_matrix <- function(
  data,
  method = c(
    "pearson",
    "spearman",
    "kendall"
  ),
  digits = 3
) {
  method <- match.arg(
    method
  )

  numeric_data <- data[
    vapply(
      data,
      is.numeric,
      FUN.VALUE = logical(1)
    )
  ]

  if (ncol(
    numeric_data
  ) < 2L) {
    stop(
      "At least two numeric variables are required.",
      call. = FALSE
    )
  }

  round(
    cor(
      numeric_data,
      use = "pairwise.complete.obs",
      method = method
    ),
    digits = digits
  )
}

create_correlation_report(
  student_data$coursework_score,
  student_data$examination_score,
  "coursework_score",
  "examination_score",
  "pearson"
)

create_correlation_matrix(
  student_data,
  "spearman"
)

# Exercise 14: Non-causal interpretation.
interpretation <- paste(
  "The Pearson correlation between coursework and examination scores was",
  round(
    pearson_correlation,
    3
  ),
  ". This describes the direction and strength of linear association in the",
  "observed sample. It does not establish that one assessment component causes",
  "the other."
)

interpretation

# Scatter plot with fitted line.
plot(
  student_data$coursework_score,
  student_data$examination_score,
  main = "Coursework and Examination Scores",
  xlab = "Coursework score",
  ylab = "Examination score",
  pch = 19
)

abline(
  lm(
    examination_score ~ coursework_score,
    data = student_data
  ),
  lwd = 2
)

# Scatter-plot matrix.
pairs(
  numeric_student_data,
  main = "Scatter-Plot Matrix"
)

# Practical assessment workflow.
all_method_reports <- do.call(
  rbind,
  lapply(
    c(
      "pearson",
      "spearman",
      "kendall"
    ),
    function(method_name) {
      create_correlation_report(
        student_data$coursework_score,
        student_data$examination_score,
        "coursework_score",
        "examination_score",
        method_name
      )
    }
  )
)

all_method_reports

correlation_assessment <- list(
  method_comparison =
    correlation_comparison,
  pearson_inference =
    pearson_report,
  grouped_correlations =
    grouped_correlations,
  correlation_matrix =
    round(
      correlation_matrix,
      3
    ),
  non_causal_interpretation =
    interpretation
)

correlation_assessment

# ==============================================================================
# End of Lesson 29 Solutions
# ==============================================================================
