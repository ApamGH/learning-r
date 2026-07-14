# ==============================================================================
# Learning R
# Part IV: Descriptive Analysis and Data Visualisation
# Lesson 25: Frequency Tables, Cross-Tabulation and Grouped Summaries
# Suggested Solutions
# ==============================================================================

student_data <- read.csv(
  "data/student_scores.csv",
  stringsAsFactors = FALSE
)

# Exercise 1: One-way frequency table.
programme_frequency <- table(
  student_data$programme
)

programme_frequency

# Exercise 2: Include missing categories.
programme_frequency_with_missing <- table(
  student_data$programme,
  useNA = "ifany"
)

programme_frequency_with_missing

# Exercise 3: Frequencies and percentages.
programme_percentage <- prop.table(
  programme_frequency
) * 100

programme_summary <- data.frame(
  programme = names(
    programme_frequency
  ),
  frequency =
    as.integer(
      programme_frequency
    ),
  percentage =
    as.numeric(
      programme_percentage
    ),
  stringsAsFactors = FALSE
)

programme_summary

# Exercise 4: Ordered cumulative table.
grade <- cut(
  student_data$final_score,
  breaks = c(
    -Inf,
    49,
    59,
    69,
    79,
    Inf
  ),
  labels = c(
    "F",
    "D",
    "C",
    "B",
    "A"
  ),
  ordered_result = TRUE
)

grade_frequency <- table(
  grade
)

grade_summary <- data.frame(
  grade = names(
    grade_frequency
  ),
  frequency =
    as.integer(
      grade_frequency
    ),
  percentage =
    as.numeric(
      prop.table(
        grade_frequency
      ) *
      100
    ),
  cumulative_frequency =
    as.integer(
      cumsum(
        grade_frequency
      )
    ),
  cumulative_percentage =
    as.numeric(
      cumsum(
        prop.table(
          grade_frequency
        ) *
        100
      )
    ),
  stringsAsFactors = FALSE
)

grade_summary

# Exercise 5: Two-way cross-tabulation.
student_data$result <- ifelse(
  student_data$final_score >= 50,
  "Pass",
  "Fail"
)

programme_result_table <- table(
  student_data$programme,
  student_data$result
)

programme_result_table

# Exercise 6: Add margins.
programme_result_with_margins <- addmargins(
  programme_result_table
)

programme_result_with_margins

# Exercise 7: Row, column and overall percentages.
row_percentages <- prop.table(
  programme_result_table,
  margin = 1
) * 100

column_percentages <- prop.table(
  programme_result_table,
  margin = 2
) * 100

overall_percentages <- prop.table(
  programme_result_table
) * 100

row_percentages
column_percentages
overall_percentages

# Exercise 8: Verify percentage totals.
rowSums(
  row_percentages
)

colSums(
  column_percentages
)

sum(
  overall_percentages
)

# Exercise 9: Grouped sample sizes.
programme_n <- table(
  student_data$programme
)

programme_n

# Exercise 10: Grouped means, medians and standard deviations.
programme_mean <- tapply(
  student_data$final_score,
  student_data$programme,
  mean,
  na.rm = TRUE
)

programme_median <- tapply(
  student_data$final_score,
  student_data$programme,
  median,
  na.rm = TRUE
)

programme_sd <- tapply(
  student_data$final_score,
  student_data$programme,
  sd,
  na.rm = TRUE
)

grouped_descriptive <- data.frame(
  programme = names(
    programme_n
  ),
  n =
    as.integer(
      programme_n
    ),
  mean =
    as.numeric(
      programme_mean[
        names(
          programme_n
        )
      ]
    ),
  median =
    as.numeric(
      programme_median[
        names(
          programme_n
        )
      ]
    ),
  standard_deviation =
    as.numeric(
      programme_sd[
        names(
          programme_n
        )
      ]
    ),
  stringsAsFactors = FALSE
)

grouped_descriptive

# Exercise 11: Pass and distinction percentages.
pass_percentage <- tapply(
  student_data$final_score >= 50,
  student_data$programme,
  mean,
  na.rm = TRUE
) * 100

distinction_percentage <- tapply(
  student_data$final_score >= 80,
  student_data$programme,
  mean,
  na.rm = TRUE
) * 100

grouped_performance <- data.frame(
  programme = names(
    pass_percentage
  ),
  pass_percentage =
    as.numeric(
      pass_percentage
    ),
  distinction_percentage =
    as.numeric(
      distinction_percentage[
        names(
          pass_percentage
        )
      ]
    ),
  stringsAsFactors = FALSE
)

grouped_performance

# Exercise 12: Reusable one-way and cross-tabulation functions.
create_one_way_table <- function(
  x,
  variable_name = "category",
  include_missing = TRUE
) {
  counts <- table(
    x,
    useNA = if (
      include_missing
    ) {
      "ifany"
    } else {
      "no"
    }
  )

  result <- data.frame(
    category = names(counts),
    frequency =
      as.integer(counts),
    percentage =
      as.numeric(
        counts /
          sum(counts) *
          100
      ),
    stringsAsFactors = FALSE
  )

  names(result)[1] <-
    variable_name

  result
}

create_cross_tabulation <- function(
  row_variable,
  column_variable,
  percentage = c(
    "count",
    "row",
    "column",
    "overall"
  )
) {
  percentage <- match.arg(
    percentage
  )

  tab <- table(
    row_variable,
    column_variable,
    useNA = "ifany"
  )

  switch(
    percentage,
    count = tab,
    row = prop.table(
      tab,
      margin = 1
    ) * 100,
    column = prop.table(
      tab,
      margin = 2
    ) * 100,
    overall = prop.table(
      tab
    ) * 100
  )
}

create_one_way_table(
  student_data$programme,
  "programme"
)

create_cross_tabulation(
  student_data$programme,
  student_data$result,
  percentage = "row"
)

# Exercise 13: Grouped summaries for clinic data.
clinic_data <- read.csv(
  "data/clinic_visits.csv",
  stringsAsFactors = FALSE
)

create_grouped_summary <- function(
  data,
  group_variable,
  numeric_variable
) {
  if (!all(
    c(
      group_variable,
      numeric_variable
    ) %in%
      names(data)
  )) {
    stop(
      "Required variables are missing.",
      call. = FALSE
    )
  }

  groups <- split(
    data[[
      numeric_variable
    ]],
    data[[
      group_variable
    ]]
  )

  results <- lapply(
    names(groups),
    function(group_name) {
      x <- groups[[
        group_name
      ]]

      data.frame(
        group = group_name,
        n = sum(
          !is.na(x)
        ),
        missing = sum(
          is.na(x)
        ),
        mean = mean(
          x,
          na.rm = TRUE
        ),
        median = median(
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

  names(result)[1] <-
    group_variable

  result
}

clinic_waiting_summary <- create_grouped_summary(
  clinic_data,
  "sex",
  "waiting_time"
)

clinic_waiting_summary

# Exercise 14: Interpretation.
largest_programme <- programme_summary$programme[
  which.max(
    programme_summary$frequency
  )
]

largest_programme_percentage <- programme_summary$percentage[
  which.max(
    programme_summary$frequency
  )
]

interpretation <- paste(
  largest_programme,
  "had the largest number of students, representing",
  round(
    largest_programme_percentage,
    2
  ),
  "per cent of the sample. Row percentages in the programme-by-result table",
  "show the pass-fail distribution within each programme."
)

interpretation

# Practical assessment workflow.
frequency_report <- create_one_way_table(
  student_data$programme,
  "programme"
)

cross_count <- create_cross_tabulation(
  student_data$programme,
  student_data$result,
  "count"
)

cross_row <- create_cross_tabulation(
  student_data$programme,
  student_data$result,
  "row"
)

cross_column <- create_cross_tabulation(
  student_data$programme,
  student_data$result,
  "column"
)

frequency_report
cross_count
cross_row
cross_column

stopifnot(
  all(
    abs(
      rowSums(
        cross_row
      ) - 100
    ) < 1e-8
  ),
  all(
    abs(
      colSums(
        cross_column
      ) - 100
    ) < 1e-8
  )
)

# ==============================================================================
# End of Lesson 25 Solutions
# ==============================================================================
