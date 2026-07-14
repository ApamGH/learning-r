# ==============================================================================
# Learning R
# Part II: Accessing and Controlling Data
# Lesson 13: Writing and Using Functions
# Suggested Solutions
# ==============================================================================


# ==============================================================================
# Exercise 1: Function with no arguments
# ==============================================================================

greet_learner <- function() {
  "Welcome to Learning R."
}

greet_learner()


# ==============================================================================
# Exercise 2: One-argument mathematical function
# ==============================================================================

square_value <- function(
  x
) {
  x ^ 2
}

square_value(5)
square_value(-3)


# ==============================================================================
# Exercise 3: Function with default arguments
# ==============================================================================

calculate_weighted_score <- function(
  coursework,
  examination,
  coursework_weight = 0.40,
  examination_weight = 0.60
) {
  coursework * coursework_weight +
    examination * examination_weight
}

calculate_weighted_score(
  coursework = 78,
  examination = 65
)

calculate_weighted_score(
  coursework = 78,
  examination = 65,
  coursework_weight = 0.30,
  examination_weight = 0.70
)


# ==============================================================================
# Exercise 4: Explicit and implicit returns
# ==============================================================================

add_values_implicit <- function(
  x,
  y
) {
  x + y
}

add_values_explicit <- function(
  x,
  y
) {
  result <- x + y

  return(result)
}

add_values_implicit(
  5,
  7
)

add_values_explicit(
  5,
  7
)


# ==============================================================================
# Exercise 5: Validate a numeric argument
# ==============================================================================

calculate_mean_score <- function(
  scores
) {
  if (!is.numeric(scores)) {
    stop(
      "`scores` must be numeric.",
      call. = FALSE
    )
  }

  if (length(scores) == 0L) {
    stop(
      "`scores` must not be empty.",
      call. = FALSE
    )
  }

  mean(
    scores,
    na.rm = TRUE
  )
}

calculate_mean_score(
  c(
    65,
    72,
    81
  )
)


# ==============================================================================
# Exercise 6: Return several results in a list
# ==============================================================================

summarise_scores <- function(
  scores
) {
  if (!is.numeric(scores)) {
    stop(
      "`scores` must be numeric.",
      call. = FALSE
    )
  }

  list(
    number = length(scores),
    observed = sum(
      !is.na(scores)
    ),
    missing = sum(
      is.na(scores)
    ),
    mean = mean(
      scores,
      na.rm = TRUE
    ),
    median = median(
      scores,
      na.rm = TRUE
    ),
    minimum = min(
      scores,
      na.rm = TRUE
    ),
    maximum = max(
      scores,
      na.rm = TRUE
    )
  )
}

score_summary <- summarise_scores(
  c(
    65,
    72,
    NA,
    81
  )
)

score_summary


# ==============================================================================
# Exercise 7: Return a summary data frame
# ==============================================================================

summarise_numeric_vector <- function(
  x,
  variable_name = "x"
) {
  if (!is.numeric(x)) {
    stop(
      "`x` must be numeric.",
      call. = FALSE
    )
  }

  data.frame(
    variable = variable_name,
    observed = sum(
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
    minimum = min(
      x,
      na.rm = TRUE
    ),
    maximum = max(
      x,
      na.rm = TRUE
    ),
    stringsAsFactors = FALSE
  )
}

summarise_numeric_vector(
  c(
    10,
    20,
    NA,
    30
  ),
  variable_name = "waiting_time"
)


# ==============================================================================
# Exercise 8: Vectorised classification function
# ==============================================================================

assign_grade <- function(
  score
) {
  ifelse(
    is.na(score),
    NA_character_,
    ifelse(
      score >= 80,
      "A",
      ifelse(
        score >= 70,
        "B",
        ifelse(
          score >= 60,
          "C",
          ifelse(
            score >= 50,
            "D",
            "F"
          )
        )
      )
    )
  )
}

assign_grade(
  c(
    45,
    58,
    72,
    84,
    NA
  )
)


# ==============================================================================
# Exercise 9: Function that modifies a data-frame copy
# ==============================================================================

add_result_columns <- function(
  data,
  score_variable = "final_score",
  pass_mark = 50,
  distinction_mark = 80
) {
  if (!is.data.frame(data)) {
    stop(
      "`data` must be a data frame.",
      call. = FALSE
    )
  }

  if (!score_variable %in%
      names(data)) {
    stop(
      paste(
        "Variable not found:",
        score_variable
      ),
      call. = FALSE
    )
  }

  score_values <- data[[
    score_variable
  ]]

  if (!is.numeric(score_values)) {
    stop(
      paste(
        score_variable,
        "must be numeric."
      ),
      call. = FALSE
    )
  }

  data$pass_status <- ifelse(
    score_values >= pass_mark,
    "Pass",
    "Fail"
  )

  data$distinction_status <- ifelse(
    score_values >= distinction_mark,
    "Yes",
    "No"
  )

  data
}

student_data <- read.csv(
  "data/student_scores.csv",
  stringsAsFactors = FALSE
)

student_results <- add_result_columns(
  student_data
)

head(student_results)


# ==============================================================================
# Exercise 10: Dynamic variable selection
# ==============================================================================

calculate_variable_mean <- function(
  data,
  variable
) {
  if (!is.data.frame(data)) {
    stop(
      "`data` must be a data frame.",
      call. = FALSE
    )
  }

  if (!variable %in%
      names(data)) {
    stop(
      paste(
        "Variable not found:",
        variable
      ),
      call. = FALSE
    )
  }

  values <- data[[
    variable
  ]]

  if (!is.numeric(values)) {
    stop(
      paste(
        variable,
        "must be numeric."
      ),
      call. = FALSE
    )
  }

  mean(
    values,
    na.rm = TRUE
  )
}

calculate_variable_mean(
  student_data,
  "final_score"
)


# ==============================================================================
# Exercise 11: Pass arguments through ...
# ==============================================================================

calculate_mean <- function(
  x,
  ...
) {
  mean(
    x,
    ...
  )
}

calculate_mean(
  c(
    10,
    NA,
    20
  ),
  na.rm = TRUE
)


# ==============================================================================
# Exercise 12: Function calling another function
# ==============================================================================

validate_scores <- function(
  scores
) {
  if (!is.numeric(scores)) {
    stop(
      "`scores` must be numeric.",
      call. = FALSE
    )
  }

  if (length(scores) == 0L) {
    stop(
      "`scores` must not be empty.",
      call. = FALSE
    )
  }

  if (!all(
    scores >= 0 &
      scores <= 100,
    na.rm = TRUE
  )) {
    stop(
      "Scores must lie between 0 and 100.",
      call. = FALSE
    )
  }

  TRUE
}

calculate_pass_count <- function(
  scores,
  pass_mark = 50
) {
  validate_scores(scores)

  sum(
    scores >= pass_mark,
    na.rm = TRUE
  )
}

calculate_pass_percentage <- function(
  scores,
  pass_mark = 50
) {
  pass_count <- calculate_pass_count(
    scores,
    pass_mark
  )

  observed_count <- sum(
    !is.na(scores)
  )

  pass_count /
    observed_count *
    100
}

calculate_pass_percentage(
  c(
    45,
    58,
    72,
    84
  )
)


# ==============================================================================
# Exercise 13: Function documentation
# ==============================================================================

# Calculate the percentage of observed scores meeting a pass mark.
#
# Arguments:
# scores:
#   Numeric vector of scores. Missing values are allowed.
#
# pass_mark:
#   Numeric threshold used to define passing. Default is 50.
#
# Returns:
#   A single numeric percentage based on observed scores.
#
# Errors:
#   Stops if scores are not numeric, empty or outside 0 through 100.
#
# Example:
#   calculate_pass_percentage(c(45, 58, 72, 84), pass_mark = 50)


# ==============================================================================
# Exercise 14: Test typical, boundary, missing and invalid inputs
# ==============================================================================

# Typical.
calculate_pass_percentage(
  c(
    45,
    58,
    72,
    84
  )
)

# Boundary.
calculate_pass_percentage(
  c(
    49,
    50,
    51
  )
)

# Missing.
calculate_pass_percentage(
  c(
    45,
    NA,
    72
  )
)

# Invalid examples are shown but not executed:
#
# calculate_pass_percentage(c("Pass", "Fail"))
# calculate_pass_percentage(numeric(0))
# calculate_pass_percentage(c(45, 120))


# ==============================================================================
# Practical assessment: Five documented custom functions
# ==============================================================================

# 1. Percentage conversion.
convert_to_percentage <- function(
  proportion
) {
  if (!is.numeric(proportion)) {
    stop(
      "`proportion` must be numeric.",
      call. = FALSE
    )
  }

  proportion * 100
}

# 2. Weighted score.
calculate_weighted_result <- function(
  coursework,
  examination,
  coursework_weight = 0.40,
  examination_weight = 0.60
) {
  if (!all(
    is.numeric(coursework),
    is.numeric(examination),
    is.numeric(coursework_weight),
    is.numeric(examination_weight)
  )) {
    stop(
      "All arguments must be numeric.",
      call. = FALSE
    )
  }

  if (!isTRUE(
    all.equal(
      coursework_weight +
        examination_weight,
      1
    )
  )) {
    stop(
      "Weights must sum to 1.",
      call. = FALSE
    )
  }

  coursework * coursework_weight +
    examination * examination_weight
}

# 3. Vector summary list.
create_summary_list <- function(
  x
) {
  if (!is.numeric(x)) {
    stop(
      "`x` must be numeric.",
      call. = FALSE
    )
  }

  list(
    n = length(x),
    observed = sum(
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
    )
  )
}

# 4. Summary data frame.
create_summary_table <- function(
  x,
  variable_name = "x"
) {
  if (!is.numeric(x)) {
    stop(
      "`x` must be numeric.",
      call. = FALSE
    )
  }

  data.frame(
    variable = variable_name,
    observed = sum(
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
    stringsAsFactors = FALSE
  )
}

# 5. Data-frame classifier.
classify_data_scores <- function(
  data,
  variable = "final_score",
  pass_mark = 50
) {
  if (!is.data.frame(data)) {
    stop(
      "`data` must be a data frame.",
      call. = FALSE
    )
  }

  if (!variable %in%
      names(data)) {
    stop(
      paste(
        "Variable not found:",
        variable
      ),
      call. = FALSE
    )
  }

  score_values <- data[[
    variable
  ]]

  data$result <- ifelse(
    score_values >= pass_mark,
    "Pass",
    "Fail"
  )

  data
}

# Use the functions.
convert_to_percentage(
  c(
    0.45,
    0.72
  )
)

calculate_weighted_result(
  coursework = 75,
  examination = 68
)

create_summary_list(
  student_data$final_score
)

create_summary_table(
  student_data$final_score,
  variable_name = "final_score"
)

classified_students <- classify_data_scores(
  student_data
)

head(classified_students)

# Anonymous application.
numeric_variables <- names(
  student_data
)[
  vapply(
    student_data,
    is.numeric,
    FUN.VALUE = logical(1)
  )
]

numeric_summaries <- lapply(
  numeric_variables,
  function(variable_name) {
    create_summary_table(
      student_data[[
        variable_name
      ]],
      variable_name
    )
  }
)

do.call(
  rbind,
  numeric_summaries
)


# ==============================================================================
# End of Lesson 13 Solutions
# ==============================================================================
