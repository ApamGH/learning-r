# ==============================================================================
# Learning R
# Part V: Statistical Inference and Hypothesis Testing
# Lesson 33: Analysis of Variance and Post Hoc Comparisons
# Suggested Solutions
# ==============================================================================

student_data <- read.csv(
  "data/student_scores.csv",
  stringsAsFactors = FALSE
)

student_data$programme <- factor(
  student_data$programme
)

# Exercise 1: State ANOVA hypotheses.
anova_hypotheses <- c(
  "H0: All programme population means are equal.",
  "H1: At least one programme population mean differs."
)

anova_hypotheses

# Exercise 2: Create group summaries.
create_anova_group_summary <- function(
  data,
  outcome,
  group
) {
  groups <- split(
    data[[outcome]],
    data[[group]]
  )

  results <- lapply(
    names(groups),
    function(group_name) {
      x <- groups[[group_name]]

      data.frame(
        group = group_name,
        n = sum(!is.na(x)),
        mean = mean(
          x,
          na.rm = TRUE
        ),
        standard_deviation = sd(
          x,
          na.rm = TRUE
        ),
        stringsAsFactors = FALSE
      )
    }
  )

  result <- do.call(
    rbind,
    results
  )

  names(result)[1] <- group

  result
}

group_summary <- create_anova_group_summary(
  student_data,
  "final_score",
  "programme"
)

group_summary

# Exercise 3: Fit a one-way ANOVA model.
anova_model <- aov(
  final_score ~ programme,
  data = student_data
)

anova_summary <- summary(
  anova_model
)

anova_summary

# Exercise 4: Extract the ANOVA table.
anova_table <- anova_summary[[1]]

anova_table

# Exercise 5: Calculate the F ratio manually.
between_mean_square <- anova_table[
  "programme",
  "Mean Sq"
]

within_mean_square <- anova_table[
  "Residuals",
  "Mean Sq"
]

manual_f <-
  between_mean_square /
  within_mean_square

data.frame(
  manual_f = manual_f,
  reported_f = anova_table[
    "programme",
    "F value"
  ]
)

# Exercise 6: Inspect residual assumptions.
anova_residuals <- residuals(
  anova_model
)

old_parameters <- par(
  mfrow = c(
    1,
    2
  )
)

hist(
  anova_residuals,
  main = "Distribution of ANOVA Residuals",
  xlab = "Residual"
)

qqnorm(
  anova_residuals,
  main = "Q-Q Plot of ANOVA Residuals"
)

qqline(
  anova_residuals
)

par(old_parameters)

shapiro_result <- shapiro.test(
  anova_residuals
)

shapiro_result

# Exercise 7: Test variance homogeneity.
bartlett_result <- bartlett.test(
  final_score ~ programme,
  data = student_data
)

fligner_result <- fligner.test(
  final_score ~ programme,
  data = student_data
)

bartlett_result
fligner_result

# Exercise 8: Calculate eta squared and omega squared.
between_sum_squares <- anova_table[
  "programme",
  "Sum Sq"
]

within_sum_squares <- anova_table[
  "Residuals",
  "Sum Sq"
]

total_sum_squares <-
  between_sum_squares +
  within_sum_squares

between_df <- anova_table[
  "programme",
  "Df"
]

eta_squared <-
  between_sum_squares /
  total_sum_squares

omega_squared <- (
  between_sum_squares -
    between_df *
    within_mean_square
) / (
  total_sum_squares +
    within_mean_square
)

data.frame(
  eta_squared = eta_squared,
  omega_squared = omega_squared
)

# Exercise 9: Tukey post hoc comparisons.
tukey_results <- TukeyHSD(
  anova_model
)

tukey_results

tukey_table <- as.data.frame(
  tukey_results$programme
)

tukey_table$comparison <- rownames(
  tukey_table
)

rownames(tukey_table) <- NULL

tukey_table[
  c(
    "comparison",
    setdiff(
      names(tukey_table),
      "comparison"
    )
  )
]

# Exercise 10: Adjusted pairwise t-tests.
pairwise_results <- pairwise.t.test(
  student_data$final_score,
  student_data$programme,
  p.adjust.method = "holm",
  pool.sd = FALSE
)

pairwise_results

# Exercise 11: Welch's ANOVA.
welch_result <- oneway.test(
  final_score ~ programme,
  data = student_data,
  var.equal = FALSE
)

welch_result

# Exercise 12: Planned contrast.
programme_levels <- levels(
  student_data$programme
)

contrast_weights <- c(
  3,
  -1,
  -1,
  -1
)

names(contrast_weights) <- programme_levels

contrast_matrix <- matrix(
  contrast_weights,
  ncol = 1
)

contrasts(
  student_data$programme
) <- contrast_matrix

planned_model <- lm(
  final_score ~ programme,
  data = student_data
)

summary(planned_model)

# Restore ordinary treatment coding.
contrasts(
  student_data$programme
) <- NULL

# Exercise 13: Reusable reporting function.
report_one_way_anova <- function(
  data,
  outcome,
  group
) {
  required <- c(
    outcome,
    group
  )

  if (!all(
    required %in% names(data)
  )) {
    stop(
      "Required variables are missing.",
      call. = FALSE
    )
  }

  formula_object <- reformulate(
    group,
    response = outcome
  )

  model <- aov(
    formula_object,
    data = data
  )

  table_result <- summary(
    model
  )[[1]]

  between_ss <- table_result[
    1,
    "Sum Sq"
  ]

  residual_ss <- table_result[
    2,
    "Sum Sq"
  ]

  total_ss <-
    between_ss +
    residual_ss

  within_ms <- table_result[
    2,
    "Mean Sq"
  ]

  df_between <- table_result[
    1,
    "Df"
  ]

  omega_squared <- (
    between_ss -
      df_between *
      within_ms
  ) / (
    total_ss +
      within_ms
  )

  data.frame(
    outcome = outcome,
    group = group,
    df_between = df_between,
    df_within = table_result[
      2,
      "Df"
    ],
    f = table_result[
      1,
      "F value"
    ],
    p_value = table_result[
      1,
      "Pr(>F)"
    ],
    eta_squared =
      between_ss /
      total_ss,
    omega_squared =
      omega_squared,
    stringsAsFactors = FALSE
  )
}

anova_report <- report_one_way_anova(
  student_data,
  "final_score",
  "programme"
)

anova_report

# Exercise 14: Complete interpretation.
decision <- if (
  anova_report$p_value < 0.05
) {
  "reject"
} else {
  "fail to reject"
}

interpretation <- paste(
  "A one-way ANOVA compared mean final scores across programmes. The test",
  "produced F(",
  anova_report$df_between,
  ", ",
  anova_report$df_within,
  ") = ",
  round(
    anova_report$f,
    3
  ),
  ", p = ",
  format.pval(
    anova_report$p_value,
    digits = 3,
    eps = 0.001
  ),
  ". We therefore ",
  decision,
  " the null hypothesis. Eta squared was ",
  round(
    anova_report$eta_squared,
    3
  ),
  ", and any post hoc interpretation should be based on adjusted pairwise",
  "comparisons.",
  sep = ""
)

interpretation

# Practical assessment workflow.
diagnostic_summary <- list(
  shapiro_wilk = shapiro_result,
  bartlett = bartlett_result,
  fligner_killeen = fligner_result
)

anova_assessment <- list(
  group_summary = group_summary,
  anova_report = anova_report,
  tukey_results = tukey_table,
  welch_anova = welch_result,
  diagnostics = diagnostic_summary
)

anova_assessment

# ==============================================================================
# End of Lesson 33 Solutions
# ==============================================================================
