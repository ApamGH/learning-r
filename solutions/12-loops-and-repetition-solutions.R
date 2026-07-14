# ==============================================================================
# Learning R
# Part II: Accessing and Controlling Data
# Lesson 12: Repetition with for, while and repeat Loops
# Suggested Solutions
# ==============================================================================

# Run this file from the root of the learning-r project.


# ==============================================================================
# Exercise 1: Print 1 through 10 with a for loop
# ==============================================================================

for (number in 1:10) {
  print(number)
}


# ==============================================================================
# Exercise 2: Iterate over programme names
# ==============================================================================

programmes <- c(
  "Statistics",
  "Procurement",
  "Hospitality",
  "Agriculture"
)

for (programme in programmes) {
  message(
    "Current programme: ",
    programme
  )
}


# ==============================================================================
# Exercise 3: Square a vector using preallocation
# ==============================================================================

values <- c(
  2,
  4,
  6,
  8,
  10
)

squared_values <- numeric(
  length(values)
)

for (i in seq_along(values)) {
  squared_values[i] <-
    values[i] ^ 2
}

squared_values


# ==============================================================================
# Exercise 4: Classify scores inside a loop
# ==============================================================================

scores <- c(
  45,
  58,
  72,
  84,
  NA
)

classification <- character(
  length(scores)
)

for (i in seq_along(scores)) {
  if (is.na(scores[i])) {
    classification[i] <- "Missing"
  } else if (scores[i] >= 80) {
    classification[i] <- "Distinction"
  } else if (scores[i] >= 50) {
    classification[i] <- "Pass"
  } else {
    classification[i] <- "Fail"
  }
}

classification


# ==============================================================================
# Exercise 5: Skip missing values with next
# ==============================================================================

values_with_missing <- c(
  10,
  NA,
  20,
  30,
  NA,
  40
)

doubled_observed_values <- numeric(0)

for (value in values_with_missing) {
  if (is.na(value)) {
    next
  }

  doubled_observed_values <- c(
    doubled_observed_values,
    value * 2
  )
}

doubled_observed_values


# ==============================================================================
# Exercise 6: Stop when an invalid value is found
# ==============================================================================

values_to_check <- c(
  10,
  20,
  30,
  -5,
  40
)

for (value in values_to_check) {
  if (value < 0) {
    message(
      "Negative value found: ",
      value,
      ". Loop stopped."
    )

    break
  }

  print(value)
}


# ==============================================================================
# Exercise 7: Multiplication table with nested loops
# ==============================================================================

multiplication_table <- matrix(
  0,
  nrow = 10,
  ncol = 10
)

for (i in 1:10) {
  for (j in 1:10) {
    multiplication_table[
      i,
      j
    ] <- i * j
  }
}

rownames(
  multiplication_table
) <- paste0(
  "Row_",
  1:10
)

colnames(
  multiplication_table
) <- paste0(
  "Column_",
  1:10
)

multiplication_table


# ==============================================================================
# Exercise 8: Safe while loop
# ==============================================================================

counter <- 1

while (counter <= 10) {
  print(counter)

  counter <- counter + 1
}


# ==============================================================================
# Exercise 9: repeat loop with stopping condition
# ==============================================================================

counter <- 1

repeat {
  print(counter)

  counter <- counter + 1

  if (counter > 10) {
    break
  }
}


# ==============================================================================
# Exercise 10: Means for all numeric columns
# ==============================================================================

student_data <- read.csv(
  "data/student_scores.csv",
  stringsAsFactors = FALSE
)

numeric_columns <- vapply(
  student_data,
  is.numeric,
  FUN.VALUE = logical(1)
)

numeric_names <- names(
  student_data
)[
  numeric_columns
]

column_means <- numeric(
  length(numeric_names)
)

names(column_means) <-
  numeric_names

for (variable_name in numeric_names) {
  column_means[
    variable_name
  ] <- mean(
    student_data[[
      variable_name
    ]],
    na.rm = TRUE
  )
}

column_means


# ==============================================================================
# Exercise 11: Process all CSV files
# ==============================================================================

csv_files <- list.files(
  path = "data",
  pattern = "\\.csv$",
  full.names = TRUE
)

file_summary <- data.frame(
  file_name = basename(csv_files),
  rows = integer(
    length(csv_files)
  ),
  columns = integer(
    length(csv_files)
  ),
  stringsAsFactors = FALSE
)

for (i in seq_along(csv_files)) {
  current_data <- read.csv(
    csv_files[i],
    stringsAsFactors = FALSE
  )

  file_summary$rows[i] <-
    nrow(current_data)

  file_summary$columns[i] <-
    ncol(current_data)
}

file_summary


# ==============================================================================
# Exercise 12: Programme summaries using a loop
# ==============================================================================

programme_names <- unique(
  student_data$programme
)

programme_summary <- data.frame(
  programme = programme_names,
  number_of_students = integer(
    length(programme_names)
  ),
  mean_final_score = numeric(
    length(programme_names)
  ),
  pass_percentage = numeric(
    length(programme_names)
  ),
  stringsAsFactors = FALSE
)

for (i in seq_along(
  programme_names
)) {
  current_programme <-
    programme_names[i]

  current_subset <- student_data[
    student_data$programme ==
      current_programme,
  ]

  programme_summary$number_of_students[i] <-
    nrow(current_subset)

  programme_summary$mean_final_score[i] <-
    mean(
      current_subset$final_score,
      na.rm = TRUE
    )

  programme_summary$pass_percentage[i] <-
    mean(
      current_subset$final_score >= 50,
      na.rm = TRUE
    ) * 100
}

programme_summary


# ==============================================================================
# Exercise 13: Compare loop and vectorised code
# ==============================================================================

coursework <- student_data$coursework_score
examination <- student_data$examination_score

vectorised_total <-
  coursework + examination

loop_total <- numeric(
  nrow(student_data)
)

for (i in seq_len(
  nrow(student_data)
)) {
  loop_total[i] <-
    coursework[i] +
    examination[i]
}

vectorised_total
loop_total

identical(
  vectorised_total,
  loop_total
)


# ==============================================================================
# Exercise 14: Simulate 500 sample means
# ==============================================================================

set.seed(20260714)

number_of_simulations <- 500

simulated_means <- numeric(
  number_of_simulations
)

for (i in seq_len(
  number_of_simulations
)) {
  simulated_sample <- rnorm(
    n = 30,
    mean = 70,
    sd = 10
  )

  simulated_means[i] <-
    mean(simulated_sample)
}

summary(simulated_means)

hist(
  simulated_means,
  main = "Distribution of 500 Simulated Sample Means",
  xlab = "Sample mean",
  ylab = "Frequency"
)


# ==============================================================================
# Practical assessment: Complete suggested workflow
# ==============================================================================

# Basic for loop.
for (i in 1:5) {
  print(
    paste(
      "Iteration",
      i
    )
  )
}

# Iterate over names.
named_counts <- c(
  Statistics = 35,
  Procurement = 42,
  Hospitality = 28
)

for (programme_name in names(
  named_counts
)) {
  print(
    paste(
      programme_name,
      named_counts[
        programme_name
      ]
    )
  )
}

# Preallocation.
assessment_scores <- c(
  45,
  58,
  72,
  84,
  91
)

assessment_labels <- character(
  length(assessment_scores)
)

for (i in seq_along(
  assessment_scores
)) {
  if (assessment_scores[i] >= 80) {
    assessment_labels[i] <-
      "Distinction"
  } else if (
    assessment_scores[i] >= 50
  ) {
    assessment_labels[i] <-
      "Pass"
  } else {
    assessment_labels[i] <-
      "Fail"
  }
}

assessment_labels

# next.
observed_scores <- numeric(0)

for (score in c(
  45,
  NA,
  72,
  84
)) {
  if (is.na(score)) {
    next
  }

  observed_scores <- c(
    observed_scores,
    score
  )
}

observed_scores

# break.
for (score in c(
  45,
  72,
  105,
  84
)) {
  if (score > 100) {
    message(
      "Invalid score found. Stopping."
    )

    break
  }

  print(score)
}

# Nested loop.
small_table <- matrix(
  0,
  nrow = 5,
  ncol = 5
)

for (i in 1:5) {
  for (j in 1:5) {
    small_table[i, j] <- i * j
  }
}

small_table

# Safe while loop.
counter <- 1
running_total <- 0

while (counter <= 10) {
  running_total <-
    running_total + counter

  counter <-
    counter + 1
}

running_total

# repeat.
counter <- 1
repeat_values <- numeric(0)

repeat {
  repeat_values <- c(
    repeat_values,
    counter
  )

  counter <-
    counter + 1

  if (counter > 5) {
    break
  }
}

repeat_values

# Process all CSV files.
assessment_csv_files <- list.files(
  path = "data",
  pattern = "\\.csv$",
  full.names = TRUE
)

assessment_file_results <- vector(
  mode = "list",
  length = length(
    assessment_csv_files
  )
)

names(assessment_file_results) <-
  basename(
    assessment_csv_files
  )

for (i in seq_along(
  assessment_csv_files
)) {
  assessment_file_results[[i]] <- tryCatch(
    {
      current_data <- read.csv(
        assessment_csv_files[i],
        stringsAsFactors = FALSE
      )

      list(
        success = TRUE,
        rows = nrow(current_data),
        columns = ncol(current_data)
      )
    },
    error = function(error_object) {
      list(
        success = FALSE,
        message = conditionMessage(
          error_object
        )
      )
    }
  )
}

assessment_file_results

# Grouped summary.
assessment_programmes <- unique(
  student_data$programme
)

assessment_programme_summary <-
  data.frame(
    programme = assessment_programmes,
    n = integer(
      length(
        assessment_programmes
      )
    ),
    mean_score = numeric(
      length(
        assessment_programmes
      )
    ),
    stringsAsFactors = FALSE
  )

for (i in seq_along(
  assessment_programmes
)) {
  current_programme <-
    assessment_programmes[i]

  current_data <- student_data[
    student_data$programme ==
      current_programme,
  ]

  assessment_programme_summary$n[i] <-
    nrow(current_data)

  assessment_programme_summary$mean_score[i] <-
    mean(
      current_data$final_score
    )
}

assessment_programme_summary

# Simulation loop.
set.seed(123)

simulation_means <- numeric(
  100
)

for (i in seq_len(100)) {
  simulation_means[i] <-
    mean(
      rnorm(
        30,
        mean = 70,
        sd = 10
      )
    )
}

summary(simulation_means)


# ==============================================================================
# End of Lesson 12 Solutions
# ==============================================================================
