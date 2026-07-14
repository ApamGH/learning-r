# ==============================================================================
# Learning R
# Part I: R Foundations
# Lesson 8: Lists, Data Frames and Tibbles
# Suggested Solutions
# ==============================================================================


# ==============================================================================
# Exercise 1: Create a mixed list
# ==============================================================================

student_record <- list(
  name = "Ama",
  age = 22L,
  programme = "Statistics",
  scores = c(
    68,
    74,
    81
  ),
  registered = TRUE
)

student_record
str(student_record)


# ==============================================================================
# Exercise 2: Extract with $, [[ and [
# ==============================================================================

student_record$name

student_record[[
  "programme"
]]

student_record[
  "scores"
]

# $ and [[ return the component itself.
# [ returns a smaller list.


# ==============================================================================
# Exercise 3: Add and remove list components
# ==============================================================================

student_record$email <-
  "ama@example.com"

student_record

student_record$email <- NULL

student_record


# ==============================================================================
# Exercise 4: Nested list
# ==============================================================================

course_record <- list(
  title = "Learning R",
  lecturer = list(
    name = "Dr. Apam",
    role = "Lecturer"
  ),
  lessons = c(
    "Foundations",
    "Data structures",
    "Analysis"
  )
)

course_record

course_record$lecturer$name

course_record[[
  "lecturer"
]][[
  "role"
]]


# ==============================================================================
# Exercise 5: Create a data frame
# ==============================================================================

students <- data.frame(
  student_id = c(
    "S001",
    "S002",
    "S003",
    "S004"
  ),
  programme = c(
    "Statistics",
    "Procurement",
    "Hospitality",
    "Agriculture"
  ),
  age = c(
    21L,
    23L,
    22L,
    24L
  ),
  final_score = c(
    72,
    68,
    81,
    75
  ),
  passed = c(
    TRUE,
    TRUE,
    TRUE,
    TRUE
  ),
  stringsAsFactors = FALSE
)

students


# ==============================================================================
# Exercise 6: Inspect data frame
# ==============================================================================

class(students)
typeof(students)
dim(students)
nrow(students)
ncol(students)
names(students)
str(students)
summary(students)


# ==============================================================================
# Exercise 7: Add calculated columns
# ==============================================================================

students$distinction <-
  students$final_score >= 80

students$score_band <- ifelse(
  students$final_score >= 80,
  "High",
  ifelse(
    students$final_score >= 70,
    "Moderate",
    "Lower"
  )
)

students


# ==============================================================================
# Exercise 8: Remove columns
# ==============================================================================

students$score_band <- NULL

students

students_without_distinction <-
  students[
    ,
    names(students) !=
      "distinction"
  ]

students_without_distinction


# ==============================================================================
# Exercise 9: Add a compatible row
# ==============================================================================

new_student <- data.frame(
  student_id = "S005",
  programme = "Statistics",
  age = 20L,
  final_score = 77,
  passed = TRUE,
  distinction = FALSE,
  stringsAsFactors = FALSE
)

students <- rbind(
  students,
  new_student
)

students


# ==============================================================================
# Exercise 10: Matrix and data-frame coercion
# ==============================================================================

mixed_matrix <- matrix(
  c(
    1,
    2,
    "A",
    "B"
  ),
  nrow = 2
)

mixed_data_frame <- data.frame(
  number = c(
    1,
    2
  ),
  category = c(
    "A",
    "B"
  ),
  stringsAsFactors = FALSE
)

mixed_matrix
typeof(mixed_matrix)

mixed_data_frame
str(mixed_data_frame)

# A matrix has one atomic type, so all values become character.
# A data frame preserves independent column types.


# ==============================================================================
# Exercise 11: Import and validate student data
# ==============================================================================

student_data <- read.csv(
  "data/student_scores.csv",
  stringsAsFactors = FALSE
)

head(student_data)
str(student_data)

required_columns <- c(
  "student_id",
  "programme",
  "coursework_score",
  "examination_score",
  "final_score"
)

required_columns_exist <- all(
  required_columns %in%
    names(student_data)
)

identifiers_are_unique <-
  !anyDuplicated(
    student_data$student_id
  )

score_arithmetic_is_valid <- all(
  student_data$final_score ==
    student_data$coursework_score +
    student_data$examination_score
)

required_columns_exist
identifiers_are_unique
score_arithmetic_is_valid


# ==============================================================================
# Exercise 12: lapply, sapply and vapply
# ==============================================================================

list_classes <- lapply(
  student_data,
  class
)

simplified_classes <- sapply(
  student_data,
  class
)

verified_classes <- vapply(
  student_data,
  class,
  FUN.VALUE = character(1)
)

list_classes
simplified_classes
verified_classes


# ==============================================================================
# Exercise 13: Create a tibble if installed
# ==============================================================================

tibble_available <- requireNamespace(
  "tibble",
  quietly = TRUE
)

tibble_available

if (tibble_available) {
  student_tibble <-
    tibble::as_tibble(
      student_data
    )

  print(
    student_tibble
  )

  class(
    student_tibble
  )
}


# ==============================================================================
# Exercise 14: List of programme-specific data frames
# ==============================================================================

programme_data <- split(
  student_data,
  student_data$programme
)

names(programme_data)

lapply(
  programme_data,
  dim
)

# Example extraction.
programme_data[[
  1
]]


# ==============================================================================
# Practical assessment: Complete list and data-frame workflow
# ==============================================================================

# Create and modify a list.
analysis_record <- list(
  title = "Student Performance Analysis",
  dataset = "student_scores.csv",
  analyst = "Student",
  completed = FALSE
)

analysis_record

analysis_record$completed <- TRUE
analysis_record$notes <-
  "Initial analysis completed."

analysis_record

# Extraction methods.
analysis_record$title

analysis_record[[
  "dataset"
]]

analysis_record[
  "analyst"
]

# Create a data frame.
assessment_students <- data.frame(
  student_id = c(
    "S101",
    "S102",
    "S103"
  ),
  programme = c(
    "Statistics",
    "Procurement",
    "Hospitality"
  ),
  coursework = c(
    30,
    28,
    35
  ),
  examination = c(
    48,
    42,
    51
  ),
  stringsAsFactors = FALSE
)

# Add columns.
assessment_students$final_score <-
  assessment_students$coursework +
  assessment_students$examination

assessment_students$passed <-
  assessment_students$final_score >= 50

assessment_students

# Remove a column from a copy.
assessment_copy <- assessment_students
assessment_copy$passed <- NULL

assessment_copy

# Append a row.
additional_student <- data.frame(
  student_id = "S104",
  programme = "Agriculture",
  coursework = 24,
  examination = 39,
  final_score = 63,
  passed = TRUE,
  stringsAsFactors = FALSE
)

assessment_students <- rbind(
  assessment_students,
  additional_student
)

assessment_students

# Validation.
assessment_required <- c(
  "student_id",
  "programme",
  "coursework",
  "examination",
  "final_score",
  "passed"
)

all(
  assessment_required %in%
    names(assessment_students)
)

!anyDuplicated(
  assessment_students$student_id
)

all(
  assessment_students$final_score ==
    assessment_students$coursework +
    assessment_students$examination
)

# Matrix comparison.
comparison_matrix <- as.matrix(
  assessment_students[
    c(
      "coursework",
      "examination",
      "final_score"
    )
  ]
)

comparison_matrix
typeof(comparison_matrix)

# Numeric-column summaries.
numeric_columns <- vapply(
  assessment_students,
  is.numeric,
  FUN.VALUE = logical(1)
)

numeric_summary <- vapply(
  assessment_students[
    numeric_columns
  ],
  mean,
  na.rm = TRUE,
  FUN.VALUE = numeric(1)
)

numeric_summary

# Apply-family comparison.
lapply(
  assessment_students,
  class
)

sapply(
  assessment_students,
  class
)

vapply(
  assessment_students,
  class,
  FUN.VALUE = character(1)
)

# Optional tibble.
if (requireNamespace(
  "tibble",
  quietly = TRUE
)) {
  assessment_tibble <-
    tibble::as_tibble(
      assessment_students
    )

  print(
    assessment_tibble
  )
}


# ==============================================================================
# End of Lesson 8 Solutions
# ==============================================================================
