# ==============================================================================
# Learning R
# Part II: Accessing and Controlling Data
# Lesson 9: Indexing, Subsetting and Extracting Values
# Suggested Solutions
# ==============================================================================

# Run this file from the root of the learning-r project.


# ==============================================================================
# Exercise 1: Positive positional indexing
# ==============================================================================

scores <- c(
  68,
  74,
  81,
  59,
  90
)

scores[1]
scores[3]
scores[c(1, 3, 5)]
scores[c(5, 1, 3)]


# ==============================================================================
# Exercise 2: Negative indexing
# ==============================================================================

scores[-2]

scores[
  -c(
    1,
    5
  )
]


# ==============================================================================
# Exercise 3: Logical indexing
# ==============================================================================

scores[
  scores >= 70
]

scores[
  scores >= 70 &
    scores <= 85
]

scores[
  scores < 60 |
    scores > 85
]


# ==============================================================================
# Exercise 4: Named-vector indexing
# ==============================================================================

programme_counts <- c(
  Statistics = 35,
  Procurement = 42,
  Hospitality = 28,
  Agriculture = 31
)

programme_counts

programme_counts[
  "Statistics"
]

programme_counts[
  c(
    "Statistics",
    "Agriculture"
  )
]


# ==============================================================================
# Exercise 5: Minimum, maximum and missing positions
# ==============================================================================

which.min(scores)
which.max(scores)

scores[
  which.min(scores)
]

scores[
  which.max(scores)
]

rainfall <- c(
  115.2,
  98.4,
  NA,
  121.7,
  110.5
)

which(
  is.na(rainfall)
)

rainfall[
  !is.na(rainfall)
]


# ==============================================================================
# Exercise 6: Duplicate values
# ==============================================================================

student_ids <- c(
  "S001",
  "S002",
  "S003",
  "S002",
  "S004",
  "S003"
)

duplicate_status <- duplicated(
  student_ids
)

duplicate_status

student_ids[
  duplicate_status
]

all_duplicate_members <- student_ids[
  duplicated(student_ids) |
    duplicated(
      student_ids,
      fromLast = TRUE
    )
]

all_duplicate_members


# ==============================================================================
# Exercise 7: Modify selected vector values
# ==============================================================================

corrected_scores <- scores

corrected_scores[4] <- 62

corrected_scores[
  c(
    1,
    2
  )
] <- c(
  70,
  76
)

corrected_scores

capped_scores <- c(
  45,
  72,
  108,
  95
)

capped_scores[
  capped_scores > 100
] <- 100

capped_scores


# ==============================================================================
# Exercise 8: Matrix indexing
# ==============================================================================

score_matrix <- matrix(
  c(
    30,
    48,
    28,
    42,
    35,
    51
  ),
  nrow = 3,
  byrow = TRUE,
  dimnames = list(
    c(
      "Ama",
      "Kojo",
      "Esi"
    ),
    c(
      "Coursework",
      "Examination"
    )
  )
)

score_matrix

score_matrix[
  2,
  1
]

score_matrix[
  1,
]

score_matrix[
  ,
  2
]

score_matrix[
  "Esi",
  "Examination"
]


# ==============================================================================
# Exercise 9: Preserve dimensions
# ==============================================================================

single_row_matrix <- score_matrix[
  1,
  ,
  drop = FALSE
]

single_column_matrix <- score_matrix[
  ,
  2,
  drop = FALSE
]

single_row_matrix
single_column_matrix

class(single_row_matrix)
class(single_column_matrix)


# ==============================================================================
# Exercise 10: List extraction
# ==============================================================================

student_record <- list(
  name = "Ama",
  age = 22L,
  programme = "Statistics",
  scores = c(
    68,
    74,
    81
  )
)

student_record[
  "programme"
]

student_record[[
  "programme"
]]

student_record$programme

# [ returns a smaller list.
# [[ and $ return the component itself.


# ==============================================================================
# Exercise 11: Passing students
# ==============================================================================

student_data <- read.csv(
  "data/student_scores.csv",
  stringsAsFactors = FALSE
)

passing_students <- student_data[
  student_data$final_score >= 50,
]

passing_students


# ==============================================================================
# Exercise 12: Distinction students in selected programmes
# ==============================================================================

selected_distinction_students <- student_data[
  student_data$final_score >= 80 &
    student_data$programme %in%
    c(
      "Statistics",
      "Procurement"
    ),
  c(
    "student_id",
    "programme",
    "final_score"
  )
]

selected_distinction_students


# ==============================================================================
# Exercise 13: Complete cases
# ==============================================================================

rainfall_data <- read.csv(
  "data/rainfall_records.csv",
  stringsAsFactors = FALSE
)

complete_rainfall_records <- rainfall_data[
  complete.cases(
    rainfall_data
  ),
]

complete_rainfall_records

clinic_data <- read.csv(
  "data/clinic_visits.csv",
  stringsAsFactors = FALSE
)

complete_waiting_records <- clinic_data[
  !is.na(
    clinic_data$waiting_time
  ),
]

complete_waiting_records


# ==============================================================================
# Exercise 14: Ordering records
# ==============================================================================

highest_to_lowest <- student_data[
  order(
    student_data$final_score,
    decreasing = TRUE
  ),
]

highest_to_lowest

programme_then_score <- student_data[
  order(
    student_data$programme,
    -student_data$final_score
  ),
]

programme_then_score


# ==============================================================================
# Exercise 15: Modify and validate a copied dataset
# ==============================================================================

corrected_student_data <- student_data

corrected_student_data[
  1,
  "coursework_score"
] <- 35

corrected_student_data[
  1,
  "final_score"
] <-
  corrected_student_data[
    1,
    "coursework_score"
  ] +
  corrected_student_data[
    1,
    "examination_score"
  ]

corrected_student_data[
  1,
]

score_arithmetic_is_valid <- all(
  corrected_student_data$final_score ==
    corrected_student_data$coursework_score +
    corrected_student_data$examination_score
)

score_arithmetic_is_valid


# ==============================================================================
# Practical assessment: Complete suggested workflow
# ==============================================================================

# Positive, negative and logical indexing.
assessment_values <- c(
  10,
  20,
  30,
  40,
  50
)

assessment_values[
  c(
    1,
    3,
    5
  )
]

assessment_values[
  -c(
    2,
    4
  )
]

assessment_values[
  assessment_values >= 30
]

# Named indexing.
named_values <- c(
  First = 10,
  Second = 20,
  Third = 30
)

named_values[
  c(
    "First",
    "Third"
  )
]

# Missing and duplicates.
values_with_missing <- c(
  10,
  NA,
  20,
  NA
)

which(
  is.na(values_with_missing)
)

duplicate_codes <- c(
  "A",
  "B",
  "A",
  "C",
  "B"
)

duplicate_codes[
  duplicated(duplicate_codes)
]

# Matrix indexing.
assessment_matrix <- matrix(
  1:12,
  nrow = 4,
  byrow = TRUE,
  dimnames = list(
    paste0(
      "R",
      1:4
    ),
    paste0(
      "C",
      1:3
    )
  )
)

assessment_matrix[
  2,
  3
]

assessment_matrix[
  "R3",
  "C2"
]

assessment_matrix[
  1,
  ,
  drop = FALSE
]

# List extraction.
assessment_list <- list(
  title = "Indexing assessment",
  score = 85,
  status = TRUE
)

assessment_list[
  "title"
]

assessment_list[[
  "score"
]]

assessment_list$status

# Data-frame subsetting.
assessment_students <- read.csv(
  "data/student_scores.csv",
  stringsAsFactors = FALSE
)

selected_rows <- assessment_students[
  assessment_students$final_score >= 70 &
    assessment_students$programme %in%
    c(
      "Statistics",
      "Procurement"
    ),
  c(
    "student_id",
    "programme",
    "final_score"
  )
]

selected_rows

# Ordering.
ordered_students <- assessment_students[
  order(
    assessment_students$programme,
    -assessment_students$final_score
  ),
]

ordered_students

# Safe modification.
assessment_copy <- assessment_students

assessment_copy[
  1,
  "coursework_score"
] <- 36

assessment_copy[
  1,
  "final_score"
] <-
  assessment_copy[
    1,
    "coursework_score"
  ] +
  assessment_copy[
    1,
    "examination_score"
  ]

all(
  assessment_copy$final_score ==
    assessment_copy$coursework_score +
    assessment_copy$examination_score
)


# ==============================================================================
# End of Lesson 9 Solutions
# ==============================================================================
