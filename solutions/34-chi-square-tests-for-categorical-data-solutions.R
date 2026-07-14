# ==============================================================================
# Learning R
# Part V: Statistical Inference and Hypothesis Testing
# Lesson 34: Chi-Square Tests for Categorical Data
# Suggested Solutions
# ==============================================================================

student_data <- read.csv(
  "data/student_scores.csv",
  stringsAsFactors = FALSE
)

student_data$result <- ifelse(
  student_data$final_score >= 50,
  "Pass",
  "Fail"
)

# Exercise 1: Create a goodness-of-fit table.
programme_counts <- table(
  student_data$programme
)

programme_counts

# Exercise 2: Test equal expected proportions.
equal_proportions <- rep(
  1 /
    length(
      programme_counts
    ),
  length(
    programme_counts
  )
)

goodness_equal <- chisq.test(
  programme_counts,
  p = equal_proportions
)

goodness_equal

# Exercise 3: Test unequal expected proportions.
expected_proportions <- c(
  0.30,
  0.25,
  0.20,
  0.25
)

names(
  expected_proportions
) <- names(
  programme_counts
)

stopifnot(
  abs(
    sum(
      expected_proportions
    ) - 1
  ) < 1e-8
)

goodness_unequal <- chisq.test(
  programme_counts,
  p = expected_proportions
)

goodness_unequal

# Exercise 4: Create a contingency table.
programme_result_table <- table(
  student_data$programme,
  student_data$result
)

programme_result_table

# Exercise 5: Conduct chi-square independence test.
independence_test <- chisq.test(
  programme_result_table,
  correct = FALSE
)

independence_test

# Exercise 6: Calculate expected counts manually.
row_totals <- rowSums(
  programme_result_table
)

column_totals <- colSums(
  programme_result_table
)

grand_total <- sum(
  programme_result_table
)

manual_expected <- outer(
  row_totals,
  column_totals
) / grand_total

manual_expected

all.equal(
  unname(
    manual_expected
  ),
  unname(
    independence_test$expected
  )
)

# Exercise 7: Calculate the chi-square statistic manually.
manual_chi_square <- sum(
  (
    programme_result_table -
      manual_expected
  ) ^ 2 /
    manual_expected
)

data.frame(
  manual_chi_square =
    manual_chi_square,
  reported_chi_square =
    unname(
      independence_test$statistic
    )
)

# Exercise 8: Inspect assumptions.
expected_values <- as.vector(
  independence_test$expected
)

assumption_report <- data.frame(
  minimum_expected =
    min(
      expected_values
    ),
  cells_below_one =
    sum(
      expected_values < 1
    ),
  cells_below_five =
    sum(
      expected_values < 5
    ),
  percentage_below_five =
    mean(
      expected_values < 5
    ) *
    100
)

assumption_report

# Exercise 9: Conduct Fisher's exact test.
fisher_result <- fisher.test(
  programme_result_table
)

fisher_result

simulated_result <- chisq.test(
  programme_result_table,
  simulate.p.value = TRUE,
  B = 5000
)

simulated_result

# Exercise 10: Inspect residuals and cell contributions.
pearson_residuals <- independence_test$residuals
standardised_residuals <- independence_test$stdres

cell_contributions <- (
  programme_result_table -
    independence_test$expected
) ^ 2 /
  independence_test$expected

pearson_residuals
standardised_residuals
cell_contributions

largest_contribution <- which(
  cell_contributions ==
    max(
      cell_contributions
    ),
  arr.ind = TRUE
)

largest_contribution

# Exercise 11: Row and column percentages.
row_percentages <- prop.table(
  programme_result_table,
  margin = 1
) * 100

column_percentages <- prop.table(
  programme_result_table,
  margin = 2
) * 100

row_percentages
column_percentages

stopifnot(
  all(
    abs(
      rowSums(
        row_percentages
      ) - 100
    ) < 1e-8
  ),
  all(
    abs(
      colSums(
        column_percentages
      ) - 100
    ) < 1e-8
  )
)

# Exercise 12: Calculate Cramer's V.
chi_square_value <- unname(
  independence_test$statistic
)

n_total <- sum(
  programme_result_table
)

number_rows <- nrow(
  programme_result_table
)

number_columns <- ncol(
  programme_result_table
)

cramers_v <- sqrt(
  chi_square_value /
    (
      n_total *
      min(
        number_rows - 1,
        number_columns - 1
      )
    )
)

cramers_v

# Exercise 13: Reusable reporting functions.
report_goodness_of_fit <- function(
  observed_counts,
  expected_proportions = NULL
) {
  if (is.null(
    expected_proportions
  )) {
    expected_proportions <- rep(
      1 /
        length(
          observed_counts
        ),
      length(
        observed_counts
      )
    )
  }

  if (abs(
    sum(
      expected_proportions
    ) - 1
  ) > 1e-8) {
    stop(
      "Expected proportions must sum to one.",
      call. = FALSE
    )
  }

  result <- chisq.test(
    observed_counts,
    p = expected_proportions
  )

  data.frame(
    chi_square =
      unname(
        result$statistic
      ),
    degrees_of_freedom =
      unname(
        result$parameter
      ),
    p_value =
      result$p.value,
    minimum_expected =
      min(
        result$expected
      ),
    stringsAsFactors = FALSE
  )
}

report_chi_square_independence <- function(
  table_object
) {
  if (!is.matrix(
    table_object
  ) &&
      !inherits(
        table_object,
        "table"
      )) {
    stop(
      "`table_object` must be a table or matrix.",
      call. = FALSE
    )
  }

  result <- chisq.test(
    table_object,
    correct = FALSE
  )

  total_n <- sum(
    table_object
  )

  r <- nrow(
    table_object
  )

  c <- ncol(
    table_object
  )

  v <- sqrt(
    unname(
      result$statistic
    ) /
      (
        total_n *
        min(
          r - 1,
          c - 1
        )
      )
  )

  data.frame(
    chi_square =
      unname(
        result$statistic
      ),
    degrees_of_freedom =
      unname(
        result$parameter
      ),
    p_value =
      result$p.value,
    cramers_v = v,
    minimum_expected =
      min(
        result$expected
      ),
    cells_below_five =
      sum(
        result$expected < 5
      ),
    stringsAsFactors = FALSE
  )
}

goodness_report <- report_goodness_of_fit(
  programme_counts
)

independence_report <- report_chi_square_independence(
  programme_result_table
)

goodness_report
independence_report

# Exercise 14: Complete interpretation.
decision <- if (
  independence_report$p_value < 0.05
) {
  "reject"
} else {
  "fail to reject"
}

interpretation <- paste(
  "A chi-square test of independence examined the association between",
  "programme and result. The test produced chi-square(",
  independence_report$degrees_of_freedom,
  ") = ",
  round(
    independence_report$chi_square,
    3
  ),
  ", p = ",
  format.pval(
    independence_report$p_value,
    digits = 3,
    eps = 0.001
  ),
  ". We therefore ",
  decision,
  " the null hypothesis of independence. Cramer's V was ",
  round(
    independence_report$cramers_v,
    3
  ),
  ". This result describes association and does not establish causation.",
  sep = ""
)

interpretation

# Visualisation.
mosaicplot(
  programme_result_table,
  main = "Programme and Result",
  xlab = "Programme",
  ylab = "Result"
)

# Practical assessment workflow.
chi_square_assessment <- list(
  goodness_equal = goodness_report,
  goodness_unequal = report_goodness_of_fit(
    programme_counts,
    expected_proportions
  ),
  independence = independence_report,
  expected_counts =
    independence_test$expected,
  standardised_residuals =
    standardised_residuals,
  row_percentages =
    row_percentages,
  column_percentages =
    column_percentages
)

chi_square_assessment

# ==============================================================================
# End of Lesson 34 Solutions
# ==============================================================================
