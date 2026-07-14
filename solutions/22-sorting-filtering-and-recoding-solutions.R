# ==============================================================================
# Learning R
# Part III: Importing, Cleaning and Transforming Data
# Lesson 22: Sorting, Filtering, Recoding and Creating Variables
# Suggested Solutions
# ==============================================================================

raw_student_data <- read.csv(
  "data/student_scores.csv",
  stringsAsFactors = FALSE
)

student_data <- raw_student_data

# Exercise 1: Sort a vector and data frame.
sorted_scores <- sort(
  student_data$final_score
)

students_high_to_low <- student_data[
  order(
    student_data$final_score,
    decreasing = TRUE
  ),
]

sorted_scores
students_high_to_low

# Exercise 2: Order by two variables.
students_by_programme <- student_data[
  order(
    student_data$programme,
    -student_data$final_score
  ),
]

students_by_programme

# Exercise 3: Filter using one and multiple conditions.
passing_students <- student_data[
  student_data$final_score >= 50,
]

selected_students <- student_data[
  student_data$final_score >= 70 &
    student_data$programme %in%
    c(
      "Statistics",
      "Procurement"
    ),
]

passing_students
selected_students

# Exercise 4: Select and exclude columns.
selected_columns <- student_data[
  c(
    "student_id",
    "programme",
    "final_score"
  )
]

excluded_components <- student_data[
  ,
  !names(student_data) %in%
    c(
      "coursework_score",
      "examination_score"
    )
]

selected_columns
excluded_components

# Exercise 5: Create arithmetic variables.
student_data$calculated_final_score <-
  student_data$coursework_score +
  student_data$examination_score

student_data$final_score_matches <-
  student_data$final_score ==
  student_data$calculated_final_score

table(
  student_data$final_score_matches,
  useNA = "ifany"
)

# Exercise 6: Create percentage variables.
student_data$coursework_percentage <-
  student_data$coursework_score /
  40 *
  100

student_data$examination_percentage <-
  student_data$examination_score /
  60 *
  100

head(
  student_data[
    c(
      "coursework_percentage",
      "examination_percentage"
    )
  ]
)

# Exercise 7: Create binary indicators.
student_data$passed <-
  student_data$final_score >= 50

student_data$distinction <-
  student_data$final_score >= 80

head(
  student_data[
    c(
      "final_score",
      "passed",
      "distinction"
    )
  ]
)

# Exercise 8: Recode categories with a named mapping.
programme_code <- c(
  Statistics = "STAT",
  Procurement = "PROC",
  Hospitality = "HOSP",
  Agriculture = "AGRIC"
)

student_data$programme_code <- unname(
  programme_code[
    student_data$programme
  ]
)

failed_recode <-
  is.na(
    student_data$programme_code
  ) &
  !is.na(
    student_data$programme
  )

student_data[
  c(
    "programme",
    "programme_code"
  )
]

student_data[
  failed_recode,
]

# Exercise 9: Group scores with cut().
student_data$grade <- cut(
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
  right = TRUE
)

table(
  student_data$grade,
  useNA = "ifany"
)

# Exercise 10: Test interval boundaries.
boundary_scores <- c(
  49,
  50,
  59,
  60,
  69,
  70,
  79,
  80
)

boundary_grades <- cut(
  boundary_scores,
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
  right = TRUE
)

data.frame(
  score = boundary_scores,
  grade = boundary_grades
)

# Exercise 11: Create centred and standardised variables.
mean_final_score <- mean(
  student_data$final_score,
  na.rm = TRUE
)

student_data$final_score_centred <-
  student_data$final_score -
  mean_final_score

student_data$final_score_z <- as.numeric(
  scale(
    student_data$final_score
  )
)

mean(
  student_data$final_score_centred,
  na.rm = TRUE
)

mean(
  student_data$final_score_z,
  na.rm = TRUE
)

sd(
  student_data$final_score_z,
  na.rm = TRUE
)

# Exercise 12: Rank observations.
student_data$score_rank <- rank(
  -student_data$final_score,
  ties.method = "min"
)

student_data[
  order(
    student_data$score_rank
  ),
  c(
    "student_id",
    "final_score",
    "score_rank"
  )
]

# Exercise 13: Within-group ranks.
student_data$programme_rank <- ave(
  -student_data$final_score,
  student_data$programme,
  FUN = function(x) {
    rank(
      x,
      ties.method = "min"
    )
  }
)

student_data[
  order(
    student_data$programme,
    student_data$programme_rank
  ),
  c(
    "programme",
    "student_id",
    "final_score",
    "programme_rank"
  )
]

# Exercise 14: Transformation log.
transformation_log <- data.frame(
  new_variable = c(
    "calculated_final_score",
    "coursework_percentage",
    "examination_percentage",
    "passed",
    "distinction",
    "grade",
    "final_score_centred",
    "final_score_z",
    "score_rank",
    "programme_rank"
  ),
  source_variables = c(
    "coursework_score + examination_score",
    "coursework_score",
    "examination_score",
    "final_score",
    "final_score",
    "final_score",
    "final_score",
    "final_score",
    "final_score",
    "final_score + programme"
  ),
  rule = c(
    "Arithmetic sum",
    "coursework_score / 40 * 100",
    "examination_score / 60 * 100",
    "final_score >= 50",
    "final_score >= 80",
    "Cut points at 49, 59, 69 and 79",
    "Subtract overall mean",
    "Standardise to mean 0 and standard deviation 1",
    "Descending overall rank",
    "Descending rank within programme"
  ),
  stringsAsFactors = FALSE
)

transformation_log

# Exercise 15: Validate derived variables.
validation_results <- list(
  calculated_score_matches = all(
    student_data$calculated_final_score ==
      student_data$final_score
  ),
  pass_indicator_matches = all(
    student_data$passed ==
      (
        student_data$final_score >= 50
      )
  ),
  distinction_indicator_matches = all(
    student_data$distinction ==
      (
        student_data$final_score >= 80
      )
  ),
  programme_recodes_complete = all(
    !is.na(
      student_data$programme_code
    )
  )
)

validation_results

# Practical assessment workflow.
analysis_data <- raw_student_data

analysis_data$programme_raw <-
  analysis_data$programme

analysis_data$programme_clean <- trimws(
  tools::toTitleCase(
    tolower(
      analysis_data$programme
    )
  )
)

analysis_data$total_score <-
  analysis_data$coursework_score +
  analysis_data$examination_score

analysis_data$result <- ifelse(
  analysis_data$total_score >= 50,
  "Pass",
  "Fail"
)

analysis_data$performance_band <- cut(
  analysis_data$total_score,
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
  )
)

analysis_data$total_score_z <- as.numeric(
  scale(
    analysis_data$total_score
  )
)

analysis_data$rank <- rank(
  -analysis_data$total_score,
  ties.method = "min"
)

analysis_validation <- list(
  raw_preserved = identical(
    raw_student_data,
    read.csv(
      "data/student_scores.csv",
      stringsAsFactors = FALSE
    )
  ),
  arithmetic_valid = all(
    analysis_data$total_score ==
      analysis_data$final_score
  ),
  no_failed_programme_cleaning = all(
    !is.na(
      analysis_data$programme_clean
    )
  )
)

analysis_validation

# ==============================================================================
# End of Lesson 22 Solutions
# ==============================================================================
