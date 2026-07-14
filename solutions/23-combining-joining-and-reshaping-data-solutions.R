# ==============================================================================
# Learning R
# Part III: Importing, Cleaning and Transforming Data
# Lesson 23: Combining, Joining and Reshaping Data
# Suggested Solutions
# ==============================================================================

# Exercise 1: Row-bind compatible datasets.
students_2025 <- data.frame(
  student_id = c(
    "S001",
    "S002"
  ),
  programme = c(
    "Statistics",
    "Procurement"
  ),
  final_score = c(
    72,
    68
  ),
  year = 2025L,
  stringsAsFactors = FALSE
)

students_2026 <- data.frame(
  student_id = c(
    "S003",
    "S004"
  ),
  programme = c(
    "Hospitality",
    "Agriculture"
  ),
  final_score = c(
    81,
    75
  ),
  year = 2026L,
  stringsAsFactors = FALSE
)

all_students <- rbind(
  students_2025,
  students_2026
)

all_students

# Exercise 2: Demonstrate incompatible row bind safely.
incomplete_students <- data.frame(
  student_id = "S005",
  final_score = 77,
  stringsAsFactors = FALSE
)

incompatible_result <- tryCatch(
  rbind(
    students_2025,
    incomplete_students
  ),
  error = function(error_object) {
    conditionMessage(
      error_object
    )
  }
)

incompatible_result

# Exercise 3: Column-bind aligned tables.
student_identity <- data.frame(
  student_id = c(
    "S001",
    "S002",
    "S003"
  ),
  programme = c(
    "Statistics",
    "Procurement",
    "Hospitality"
  ),
  stringsAsFactors = FALSE
)

student_results <- data.frame(
  coursework = c(
    30,
    28,
    35
  ),
  examination = c(
    48,
    42,
    51
  )
)

combined_columns <- cbind(
  student_identity,
  student_results
)

combined_columns

# Exercise 4: Demonstrate positional alignment risk.
shuffled_results <- student_results[
  c(
    3,
    1,
    2
  ),
]

misaligned_result <- cbind(
  student_identity,
  shuffled_results
)

misaligned_result

# Exercise 5: Perform four join types.
student_master <- data.frame(
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
  stringsAsFactors = FALSE
)

score_table <- data.frame(
  student_id = c(
    "S001",
    "S002",
    "S003",
    "S005"
  ),
  final_score = c(
    72,
    68,
    81,
    77
  ),
  stringsAsFactors = FALSE
)

inner_joined <- merge(
  student_master,
  score_table,
  by = "student_id"
)

left_joined <- merge(
  student_master,
  score_table,
  by = "student_id",
  all.x = TRUE
)

right_joined <- merge(
  student_master,
  score_table,
  by = "student_id",
  all.y = TRUE
)

full_joined <- merge(
  student_master,
  score_table,
  by = "student_id",
  all = TRUE
)

inner_joined
left_joined
right_joined
full_joined

# Exercise 6: Identify unmatched keys.
unmatched_left <- setdiff(
  student_master$student_id,
  score_table$student_id
)

unmatched_right <- setdiff(
  score_table$student_id,
  student_master$student_id
)

unmatched_left
unmatched_right

# Exercise 7: Join differently named variables.
identity_data <- data.frame(
  student_code = c(
    "S001",
    "S002"
  ),
  programme = c(
    "Statistics",
    "Procurement"
  ),
  stringsAsFactors = FALSE
)

result_data <- data.frame(
  student_id = c(
    "S001",
    "S002"
  ),
  final_score = c(
    72,
    68
  ),
  stringsAsFactors = FALSE
)

different_key_join <- merge(
  identity_data,
  result_data,
  by.x = "student_code",
  by.y = "student_id"
)

different_key_join

# Exercise 8: Join on a composite key.
semester_records <- data.frame(
  student_id = c(
    "S001",
    "S001",
    "S002",
    "S002"
  ),
  semester = c(
    1,
    2,
    1,
    2
  ),
  final_score = c(
    70,
    75,
    68,
    72
  )
)

semester_status <- data.frame(
  student_id = c(
    "S001",
    "S001",
    "S002",
    "S002"
  ),
  semester = c(
    1,
    2,
    1,
    2
  ),
  status = c(
    "Completed",
    "Completed",
    "Completed",
    "Completed"
  ),
  stringsAsFactors = FALSE
)

composite_join <- merge(
  semester_records,
  semester_status,
  by = c(
    "student_id",
    "semester"
  )
)

composite_join

# Exercise 9: One-to-many and many-to-many relationships.
households <- data.frame(
  household_id = c(
    "H001",
    "H002"
  ),
  district = c(
    "Bolgatanga",
    "Bawku West"
  ),
  stringsAsFactors = FALSE
)

members <- data.frame(
  household_id = c(
    "H001",
    "H001",
    "H002"
  ),
  member_name = c(
    "Ama",
    "Kojo",
    "Esi"
  ),
  stringsAsFactors = FALSE
)

one_to_many <- merge(
  households,
  members,
  by = "household_id"
)

one_to_many

table_a <- data.frame(
  key = c(
    "A",
    "A"
  ),
  value_a = c(
    1,
    2
  )
)

table_b <- data.frame(
  key = c(
    "A",
    "A",
    "A"
  ),
  value_b = c(
    10,
    20,
    30
  )
)

many_to_many <- merge(
  table_a,
  table_b,
  by = "key"
)

many_to_many
nrow(many_to_many)

# Exercise 10: Audit key uniqueness.
audit_key <- function(
  data,
  key
) {
  key_values <- data[[
    key
  ]]

  data.frame(
    total_rows = nrow(data),
    missing_keys = sum(
      is.na(key_values) |
        key_values == ""
    ),
    duplicated_rows = sum(
      duplicated(key_values)
    ),
    unique_keys = length(
      unique(key_values)
    )
  )
}

audit_key(
  student_master,
  "student_id"
)

audit_key(
  score_table,
  "student_id"
)

# Exercise 11: Wide to long.
wide_scores <- data.frame(
  student_id = c(
    "S001",
    "S002",
    "S003"
  ),
  semester_1 = c(
    65,
    72,
    81
  ),
  semester_2 = c(
    68,
    75,
    84
  ),
  stringsAsFactors = FALSE
)

long_scores <- reshape(
  wide_scores,
  varying = c(
    "semester_1",
    "semester_2"
  ),
  v.names = "score",
  timevar = "semester",
  times = c(
    1,
    2
  ),
  direction = "long"
)

long_scores

# Exercise 12: Long to wide.
wide_again <- reshape(
  long_scores,
  idvar = "student_id",
  timevar = "semester",
  direction = "wide"
)

wide_again

# Exercise 13: Validate identifier-time uniqueness.
long_key <- paste(
  long_scores$student_id,
  long_scores$semester,
  sep = "::"
)

identifier_time_duplicates <- anyDuplicated(
  long_key
)

identifier_time_duplicates

# Exercise 14: Add source-file tracking.
year_2025 <- students_2025
year_2026 <- students_2026

year_2025$source_file <-
  "students_2025.csv"

year_2026$source_file <-
  "students_2026.csv"

combined_with_source <- rbind(
  year_2025,
  year_2026
)

combined_with_source

# Exercise 15: Post-combination audit.
audit_join <- function(
  left_data,
  right_data,
  key
) {
  left_keys <- left_data[[key]]
  right_keys <- right_data[[key]]

  list(
    left_rows = nrow(left_data),
    right_rows = nrow(right_data),
    left_duplicate_keys = sum(
      duplicated(left_keys)
    ),
    right_duplicate_keys = sum(
      duplicated(right_keys)
    ),
    unmatched_left = setdiff(
      unique(left_keys),
      unique(right_keys)
    ),
    unmatched_right = setdiff(
      unique(right_keys),
      unique(left_keys)
    )
  )
}

join_audit <- audit_join(
  student_master,
  score_table,
  "student_id"
)

join_audit

post_combination_audit <- list(
  expected_row_bound_rows =
    nrow(students_2025) +
    nrow(students_2026),
  observed_row_bound_rows =
    nrow(all_students),
  expected_left_join_rows =
    nrow(student_master),
  observed_left_join_rows =
    nrow(left_joined),
  long_identifier_time_duplicates =
    identifier_time_duplicates
)

post_combination_audit

# Optional dplyr and tidyr equivalents.
if (requireNamespace(
  "dplyr",
  quietly = TRUE
)) {
  print(
    dplyr::left_join(
      student_master,
      score_table,
      by = "student_id"
    )
  )
}

if (requireNamespace(
  "tidyr",
  quietly = TRUE
)) {
  long_tidyr <- tidyr::pivot_longer(
    wide_scores,
    cols = c(
      semester_1,
      semester_2
    ),
    names_to = "semester",
    values_to = "score"
  )

  print(long_tidyr)
}

# Practical assessment workflow.
combination_log <- data.frame(
  operation = c(
    "Row bind",
    "Left join",
    "Wide-to-long"
  ),
  input_objects = c(
    "students_2025; students_2026",
    "student_master; score_table",
    "wide_scores"
  ),
  key = c(
    "student_id + year",
    "student_id",
    "student_id + semester"
  ),
  expected_rows = c(
    nrow(students_2025) +
      nrow(students_2026),
    nrow(student_master),
    nrow(wide_scores) * 2
  ),
  observed_rows = c(
    nrow(all_students),
    nrow(left_joined),
    nrow(long_scores)
  ),
  stringsAsFactors = FALSE
)

combination_log

combination_log$row_count_matches <-
  combination_log$expected_rows ==
  combination_log$observed_rows

combination_log

# ==============================================================================
# End of Lesson 23 Solutions
# ==============================================================================
