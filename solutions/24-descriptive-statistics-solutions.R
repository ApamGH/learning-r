# ==============================================================================
# Learning R
# Part IV: Descriptive Analysis and Data Visualisation
# Lesson 24: Descriptive Statistics for Numerical Data
# Suggested Solutions
# ==============================================================================

student_data <- read.csv(
  "data/student_scores.csv",
  stringsAsFactors = FALSE
)

final_scores <- student_data$final_score

# Exercise 1: Observed and missing counts.
total_count <- length(final_scores)
observed_count <- sum(
  !is.na(final_scores)
)
missing_count <- sum(
  is.na(final_scores)
)

data.frame(
  total = total_count,
  observed = observed_count,
  missing = missing_count,
  missing_percentage =
    missing_count /
    total_count *
    100
)

# Exercise 2: Mean, median and mode.
mean_score <- mean(
  final_scores,
  na.rm = TRUE
)

median_score <- median(
  final_scores,
  na.rm = TRUE
)

statistical_mode <- function(
  x,
  remove_missing = TRUE
) {
  if (remove_missing) {
    x <- x[
      !is.na(x)
    ]
  }

  if (length(x) == 0L) {
    return(
      list(
        mode = NA,
        frequency = 0L
      )
    )
  }

  frequencies <- table(x)
  maximum_frequency <- max(frequencies)

  list(
    mode = names(
      frequencies
    )[
      frequencies ==
        maximum_frequency
    ],
    frequency =
      as.integer(
        maximum_frequency
      )
  )
}

mode_result <- statistical_mode(
  final_scores
)

mean_score
median_score
mode_result

# Exercise 3: Minimum, maximum and range.
minimum_score <- min(
  final_scores,
  na.rm = TRUE
)

maximum_score <- max(
  final_scores,
  na.rm = TRUE
)

score_range <-
  maximum_score -
  minimum_score

minimum_score
maximum_score
score_range

# Exercise 4: Quartiles and IQR.
quartiles <- quantile(
  final_scores,
  probs = c(
    0.25,
    0.50,
    0.75
  ),
  na.rm = TRUE
)

interquartile_range <- IQR(
  final_scores,
  na.rm = TRUE
)

quartiles
interquartile_range

# Exercise 5: Variance and standard deviation.
score_variance <- var(
  final_scores,
  na.rm = TRUE
)

score_standard_deviation <- sd(
  final_scores,
  na.rm = TRUE
)

score_variance
score_standard_deviation

# Exercise 6: Coefficient of variation.
coefficient_of_variation <-
  score_standard_deviation /
  mean_score *
  100

coefficient_of_variation

# Exercise 7: Selected percentiles.
selected_percentiles <- quantile(
  final_scores,
  probs = c(
    0.05,
    0.10,
    0.25,
    0.50,
    0.75,
    0.90,
    0.95
  ),
  na.rm = TRUE
)

selected_percentiles

# Exercise 8: Effect of an extreme value.
ordinary_values <- c(
  10,
  12,
  13,
  14,
  15
)

values_with_extreme <- c(
  ordinary_values,
  100
)

extreme_value_comparison <- data.frame(
  dataset = c(
    "Ordinary values",
    "With extreme value"
  ),
  mean = c(
    mean(ordinary_values),
    mean(values_with_extreme)
  ),
  median = c(
    median(ordinary_values),
    median(values_with_extreme)
  ),
  standard_deviation = c(
    sd(ordinary_values),
    sd(values_with_extreme)
  ),
  interquartile_range = c(
    IQR(ordinary_values),
    IQR(values_with_extreme)
  ),
  stringsAsFactors = FALSE
)

extreme_value_comparison

# Exercise 9: Possible outliers.
q1 <- quartiles[1]
q3 <- quartiles[3]

lower_fence <-
  q1 -
  1.5 *
  interquartile_range

upper_fence <-
  q3 +
  1.5 *
  interquartile_range

outlier_status <-
  final_scores <
  lower_fence |
  final_scores >
  upper_fence

student_data[
  outlier_status,
]

# Exercise 10: Grouped means and standard deviations.
programme_means <- tapply(
  student_data$final_score,
  student_data$programme,
  mean,
  na.rm = TRUE
)

programme_standard_deviations <- tapply(
  student_data$final_score,
  student_data$programme,
  sd,
  na.rm = TRUE
)

programme_means
programme_standard_deviations

# Exercise 11: Complete numerical summary function.
summarise_numeric <- function(
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

  if (length(observed) == 0L) {
    stop(
      "No observed values are available.",
      call. = FALSE
    )
  }

  q <- quantile(
    observed,
    probs = c(
      0.25,
      0.50,
      0.75
    )
  )

  data.frame(
    variable = variable_name,
    total = length(x),
    observed = length(observed),
    missing = sum(
      is.na(x)
    ),
    missing_percentage =
      mean(
        is.na(x)
      ) *
      100,
    mean = mean(observed),
    median = median(observed),
    variance = var(observed),
    standard_deviation = sd(observed),
    minimum = min(observed),
    first_quartile = q[1],
    third_quartile = q[3],
    maximum = max(observed),
    interquartile_range = IQR(observed),
    coefficient_of_variation =
      sd(observed) /
      mean(observed) *
      100,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

summarise_numeric(
  final_scores,
  "final_score"
)

# Exercise 12: Summarise every numerical variable.
numeric_variables <- names(
  student_data
)[
  vapply(
    student_data,
    is.numeric,
    FUN.VALUE = logical(1)
  )
]

numeric_summary_list <- lapply(
  numeric_variables,
  function(variable_name) {
    summarise_numeric(
      student_data[[
        variable_name
      ]],
      variable_name
    )
  }
)

numeric_summary_table <- do.call(
  rbind,
  numeric_summary_list
)

numeric_summary_table

# Exercise 13: Grouped descriptive table.
summarise_by_group <- function(
  data,
  numerical_variable,
  group_variable
) {
  required_variables <- c(
    numerical_variable,
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

  group_values <- unique(
    data[[
      group_variable
    ]]
  )

  results <- lapply(
    group_values,
    function(group_value) {
      selected_values <- data[
        data[[
          group_variable
        ]] == group_value &
          !is.na(
            data[[
              group_variable
            ]]
          ),
        numerical_variable
      ]

      result <- summarise_numeric(
        selected_values,
        numerical_variable
      )

      result[
        group_variable
      ] <- group_value

      result
    }
  )

  result <- do.call(
    rbind,
    results
  )

  result[
    c(
      group_variable,
      setdiff(
        names(result),
        group_variable
      )
    )
  ]
}

programme_summary <- summarise_by_group(
  student_data,
  "final_score",
  "programme"
)

programme_summary

# Exercise 14: Contextual interpretation.
rounded_summary <- programme_summary

numeric_columns <- vapply(
  rounded_summary,
  is.numeric,
  FUN.VALUE = logical(1)
)

rounded_summary[
  numeric_columns
] <- lapply(
  rounded_summary[
    numeric_columns
  ],
  round,
  digits = 2
)

rounded_summary

interpretation <- paste(
  "The overall mean final score was",
  round(
    mean_score,
    2
  ),
  "with a median of",
  round(
    median_score,
    2
  ),
  "and a standard deviation of",
  round(
    score_standard_deviation,
    2
  ),
  ". The observed scores ranged from",
  minimum_score,
  "to",
  maximum_score,
  "."
)

interpretation

# Practical assessment workflow.
assessment_summary <- summarise_numeric(
  student_data$final_score,
  "final_score"
)

assessment_percentiles <- quantile(
  student_data$final_score,
  probs = c(
    0.10,
    0.25,
    0.50,
    0.75,
    0.90
  ),
  na.rm = TRUE
)

assessment_groups <- summarise_by_group(
  student_data,
  "final_score",
  "programme"
)

assessment_summary
assessment_percentiles
assessment_groups

# ==============================================================================
# End of Lesson 24 Solutions
# ==============================================================================
