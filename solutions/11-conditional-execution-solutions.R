# ==============================================================================
# Learning R
# Part II: Accessing and Controlling Data
# Lesson 11: Conditional Execution with if, else and ifelse()
# Suggested Solutions
# ==============================================================================


# ==============================================================================
# Exercise 1: Basic if statement
# ==============================================================================

score <- 72

if (score >= 50) {
  print(
    "The student passed."
  )
}


# ==============================================================================
# Exercise 2: if-else pass rule
# ==============================================================================

score <- 42

if (score >= 50) {
  result <- "Pass"
} else {
  result <- "Fail"
}

result


# ==============================================================================
# Exercise 3: else-if grading system
# ==============================================================================

score <- 76

if (is.na(score)) {
  grade <- "Missing"
} else if (score >= 80) {
  grade <- "A"
} else if (score >= 70) {
  grade <- "B"
} else if (score >= 60) {
  grade <- "C"
} else if (score >= 50) {
  grade <- "D"
} else {
  grade <- "F"
}

grade


# ==============================================================================
# Exercise 4: Why branch order matters
# ==============================================================================

score <- 85

if (score >= 50) {
  incorrect_classification <- "Pass"
} else if (score >= 80) {
  incorrect_classification <- "Distinction"
} else {
  incorrect_classification <- "Fail"
}

incorrect_classification

if (score >= 80) {
  correct_classification <- "Distinction"
} else if (score >= 50) {
  correct_classification <- "Pass"
} else {
  correct_classification <- "Fail"
}

correct_classification


# ==============================================================================
# Exercise 5: Combine scalar conditions
# ==============================================================================

score <- 84
attendance <- 0.93

if (
  score >= 80 &&
  attendance >= 0.90
) {
  award_status <- "Eligible"
} else {
  award_status <- "Not eligible"
}

award_status


# ==============================================================================
# Exercise 6: Handle missing input
# ==============================================================================

score <- NA

if (is.na(score)) {
  result <- "Score missing"
} else if (score >= 50) {
  result <- "Pass"
} else {
  result <- "Fail"
}

result


# ==============================================================================
# Exercise 7: Vector classification with ifelse()
# ==============================================================================

scores <- c(
  45,
  58,
  72,
  84
)

pass_status <- ifelse(
  scores >= 50,
  "Pass",
  "Fail"
)

pass_status


# ==============================================================================
# Exercise 8: Type coercion in ifelse()
# ==============================================================================

mixed_result <- ifelse(
  c(
    TRUE,
    FALSE
  ),
  1,
  "No"
)

mixed_result
typeof(mixed_result)

# The result is character because numeric and character outputs require a
# common type.


# ==============================================================================
# Exercise 9: switch()
# ==============================================================================

output_format <- "pdf"

format_description <- switch(
  output_format,
  html = "Web document",
  word = "Microsoft Word document",
  pdf = "PDF document",
  "Unknown format"
)

format_description


# ==============================================================================
# Exercise 10: Stop when a file is missing
# ==============================================================================

student_path <- "data/student_scores.csv"

if (!file.exists(student_path)) {
  stop(
    paste(
      "Required file not found:",
      student_path
    ),
    call. = FALSE
  )
}

student_data <- read.csv(
  student_path,
  stringsAsFactors = FALSE
)


# ==============================================================================
# Exercise 11: Warning for invalid values
# ==============================================================================

scores_to_check <- c(
  65,
  72,
  105
)

if (any(
  scores_to_check > 100,
  na.rm = TRUE
)) {
  warning(
    "At least one score exceeds 100.",
    call. = FALSE
  )
}


# ==============================================================================
# Exercise 12: Contextual decision rules
# ==============================================================================

# Education rule.
student_score <- 68
attendance_rate <- 0.80

if (
  student_score >= 50 &&
  attendance_rate >= 0.75
) {
  progression_status <- "Progress"
} else {
  progression_status <- "Do not progress"
}

progression_status

# Health rule.
waiting_time <- 55
severity <- "Moderate"

if (
  severity == "High" ||
  waiting_time > 60
) {
  triage_status <- "Immediate attention"
} else if (waiting_time > 30) {
  triage_status <- "Priority queue"
} else {
  triage_status <- "Standard queue"
}

triage_status

# Procurement rule.
procurement_value <- 42000

if (procurement_value < 0) {
  approval_level <- "Invalid value"
} else if (procurement_value < 10000) {
  approval_level <- "Departmental approval"
} else if (procurement_value < 50000) {
  approval_level <- "Director approval"
} else {
  approval_level <- "Tender committee review"
}

approval_level


# ==============================================================================
# Exercise 13: Classify imported student data
# ==============================================================================

student_data$result <- ifelse(
  student_data$final_score >= 50,
  "Pass",
  "Fail"
)

student_data$distinction <- ifelse(
  student_data$final_score >= 80,
  "Yes",
  "No"
)

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

head(
  student_data[
    c(
      "student_id",
      "final_score",
      "result",
      "distinction",
      "grade"
    )
  ]
)


# ==============================================================================
# Exercise 14: Test every branch
# ==============================================================================

classify_one_score <- function(
  score
) {
  if (is.na(score)) {
    "Missing"
  } else if (score >= 80) {
    "Distinction"
  } else if (score >= 50) {
    "Pass"
  } else {
    "Fail"
  }
}

test_scores <- c(
  NA,
  45,
  65,
  85
)

branch_results <- vapply(
  test_scores,
  classify_one_score,
  FUN.VALUE = character(1)
)

branch_results


# ==============================================================================
# Practical assessment: Complete suggested workflow
# ==============================================================================

# Basic if.
value <- 10

if (value > 0) {
  positive_message <- "Value is positive."
}

positive_message

# if-else.
if (value %% 2 == 0) {
  parity <- "Even"
} else {
  parity <- "Odd"
}

parity

# else-if branches.
temperature <- 34

if (is.na(temperature)) {
  temperature_status <- "Missing"
} else if (temperature >= 40) {
  temperature_status <- "Extreme"
} else if (temperature >= 35) {
  temperature_status <- "Very high"
} else if (temperature >= 30) {
  temperature_status <- "High"
} else {
  temperature_status <- "Moderate"
}

temperature_status

# Missing condition.
missing_value <- NA

if (is.na(missing_value)) {
  missing_status <- "Input missing"
} else {
  missing_status <- "Input available"
}

missing_status

# if versus ifelse.
single_score <- 72

single_result <- if (
  single_score >= 50
) {
  "Pass"
} else {
  "Fail"
}

vector_scores <- c(
  45,
  58,
  72,
  84
)

vector_results <- ifelse(
  vector_scores >= 50,
  "Pass",
  "Fail"
)

single_result
vector_results

# switch.
analysis_choice <- "median"

selected_statistic <- switch(
  analysis_choice,
  mean = mean(
    vector_scores
  ),
  median = median(
    vector_scores
  ),
  sum = sum(
    vector_scores
  ),
  stop(
    "Unknown analysis choice.",
    call. = FALSE
  )
)

selected_statistic

# File and variable validation.
assessment_path <-
  "data/student_scores.csv"

if (!file.exists(assessment_path)) {
  stop(
    paste(
      "Required file not found:",
      assessment_path
    ),
    call. = FALSE
  )
}

assessment_data <- read.csv(
  assessment_path,
  stringsAsFactors = FALSE
)

required_variables <- c(
  "student_id",
  "programme",
  "final_score"
)

missing_variables <- setdiff(
  required_variables,
  names(assessment_data)
)

if (length(missing_variables) > 0L) {
  stop(
    paste(
      "Missing variables:",
      paste(
        missing_variables,
        collapse = ", "
      )
    ),
    call. = FALSE
  )
}

# Warning and stop conditions.
if (any(
  assessment_data$final_score > 100,
  na.rm = TRUE
)) {
  warning(
    "At least one score exceeds 100.",
    call. = FALSE
  )
}

if (any(
  assessment_data$final_score < 0,
  na.rm = TRUE
)) {
  stop(
    "Negative scores are not allowed.",
    call. = FALSE
  )
}

# Contextual vectorised rule.
assessment_data$performance <- ifelse(
  assessment_data$final_score >= 80,
  "Distinction",
  ifelse(
    assessment_data$final_score >= 50,
    "Pass",
    "Fail"
  )
)

head(
  assessment_data[
    c(
      "student_id",
      "final_score",
      "performance"
    )
  ]
)

# Branch test table.
branch_test_values <- c(
  NA,
  45,
  65,
  85
)

branch_test_results <- vapply(
  branch_test_values,
  classify_one_score,
  FUN.VALUE = character(1)
)

data.frame(
  score = branch_test_values,
  classification = branch_test_results,
  stringsAsFactors = FALSE
)


# ==============================================================================
# End of Lesson 11 Solutions
# ==============================================================================
