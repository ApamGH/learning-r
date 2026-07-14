# ==============================================================================
# Learning R
# Part III: Importing, Cleaning and Transforming Data
# Lesson 17: Data Inspection, Validation and Quality Assurance
# Suggested Solutions
# ==============================================================================

raw_student_data <- read.csv(
  "data/student_scores.csv",
  stringsAsFactors = FALSE
)

analysis_student_data <- raw_student_data

# Exercise 1: Structural profile.
structural_profile <- data.frame(
  variable = names(raw_student_data),
  class = vapply(
    raw_student_data,
    function(x) {
      paste(
        class(x),
        collapse = ", "
      )
    },
    FUN.VALUE = character(1)
  ),
  type = vapply(
    raw_student_data,
    typeof,
    FUN.VALUE = character(1)
  ),
  length = vapply(
    raw_student_data,
    length,
    FUN.VALUE = integer(1)
  ),
  unique_values = vapply(
    raw_student_data,
    function(x) {
      length(unique(x))
    },
    FUN.VALUE = integer(1)
  ),
  stringsAsFactors = FALSE
)

structural_profile

# Exercise 2: Missingness report.
missingness_report <- data.frame(
  variable = names(raw_student_data),
  missing_count = vapply(
    raw_student_data,
    function(x) {
      sum(is.na(x))
    },
    FUN.VALUE = integer(1)
  ),
  missing_percentage = vapply(
    raw_student_data,
    function(x) {
      mean(is.na(x)) * 100
    },
    FUN.VALUE = numeric(1)
  ),
  stringsAsFactors = FALSE
)

missingness_report

# Exercise 3: Identifier validation.
identifier_missing <- is.na(
  raw_student_data$student_id
) |
  raw_student_data$student_id == ""

identifier_duplicate <- duplicated(
  raw_student_data$student_id
)

sum(identifier_missing)
sum(identifier_duplicate)

# Exercise 4: All duplicate members.
all_duplicate_members <- duplicated(
  raw_student_data$student_id
) |
  duplicated(
    raw_student_data$student_id,
    fromLast = TRUE
  )

raw_student_data[
  all_duplicate_members,
]

# Exercise 5: Range rules.
coursework_valid <-
  raw_student_data$coursework_score >= 0 &
  raw_student_data$coursework_score <= 40

examination_valid <-
  raw_student_data$examination_score >= 0 &
  raw_student_data$examination_score <= 60

final_valid <-
  raw_student_data$final_score >= 0 &
  raw_student_data$final_score <= 100

all(coursework_valid)
all(examination_valid)
all(final_valid)

# Exercise 6: Category validation.
allowed_programmes <- c(
  "Statistics",
  "Procurement",
  "Hospitality",
  "Agriculture"
)

programme_valid <-
  raw_student_data$programme %in%
  allowed_programmes

all(programme_valid)

invalid_programmes <- setdiff(
  unique(
    raw_student_data$programme
  ),
  allowed_programmes
)

invalid_programmes

# Exercise 7: Arithmetic consistency.
arithmetic_valid <-
  raw_student_data$final_score ==
  raw_student_data$coursework_score +
  raw_student_data$examination_score

all(arithmetic_valid)

raw_student_data[
  !arithmetic_valid,
]

# Exercise 8: Row-level issue flags.
validation_data <- raw_student_data

validation_data$missing_identifier <-
  identifier_missing

validation_data$duplicate_identifier <-
  all_duplicate_members

validation_data$invalid_coursework <-
  !coursework_valid

validation_data$invalid_examination <-
  !examination_valid

validation_data$invalid_final_score <-
  !final_valid

validation_data$invalid_programme <-
  !programme_valid

validation_data$invalid_arithmetic <-
  !arithmetic_valid

issue_columns <- c(
  "missing_identifier",
  "duplicate_identifier",
  "invalid_coursework",
  "invalid_examination",
  "invalid_final_score",
  "invalid_programme",
  "invalid_arithmetic"
)

validation_data$has_any_issue <- rowSums(
  validation_data[
    issue_columns
  ],
  na.rm = TRUE
) > 0

head(validation_data)

# Exercise 9: Issue count per record.
validation_data$number_of_issues <- rowSums(
  validation_data[
    issue_columns
  ],
  na.rm = TRUE
)

validation_data[
  validation_data$number_of_issues > 0,
]

# Exercise 10: Issue summary table.
issue_summary <- data.frame(
  issue = issue_columns,
  count = vapply(
    validation_data[
      issue_columns
    ],
    sum,
    na.rm = TRUE,
    FUN.VALUE = numeric(1)
  ),
  stringsAsFactors = FALSE
)

issue_summary$percentage <-
  issue_summary$count /
  nrow(validation_data) *
  100

issue_summary

# Exercise 11: Reusable validation function.
validate_score_data <- function(
  data,
  id_variable = "student_id",
  coursework_variable =
    "coursework_score",
  examination_variable =
    "examination_score",
  final_variable =
    "final_score"
) {
  required_variables <- c(
    id_variable,
    coursework_variable,
    examination_variable,
    final_variable
  )

  missing_variables <- setdiff(
    required_variables,
    names(data)
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

  id_values <- data[[id_variable]]
  coursework_values <-
    data[[coursework_variable]]
  examination_values <-
    data[[examination_variable]]
  final_values <- data[[final_variable]]

  data.frame(
    missing_identifier =
      is.na(id_values) |
      id_values == "",
    duplicate_identifier =
      duplicated(id_values) |
      duplicated(
        id_values,
        fromLast = TRUE
      ),
    invalid_coursework =
      coursework_values < 0 |
      coursework_values > 40,
    invalid_examination =
      examination_values < 0 |
      examination_values > 60,
    invalid_final =
      final_values < 0 |
      final_values > 100,
    invalid_arithmetic =
      final_values !=
      coursework_values +
      examination_values
  )
}

score_validation <- validate_score_data(
  raw_student_data
)

head(score_validation)
colSums(score_validation)

# Exercise 12: Quality gate.
critical_issue_count <- sum(
  validation_data$invalid_arithmetic |
    validation_data$invalid_final_score,
  na.rm = TRUE
)

if (critical_issue_count > 0L) {
  warning(
    paste(
      critical_issue_count,
      "critical issue(s) detected."
    ),
    call. = FALSE
  )
} else {
  message(
    "No critical score issues detected."
  )
}

# Exercise 13: Correction log.
correction_log <- data.frame(
  record_id = character(0),
  variable = character(0),
  old_value = character(0),
  new_value = character(0),
  reason = character(0),
  corrected_by = character(0),
  correction_date = as.Date(
    character(0)
  ),
  stringsAsFactors = FALSE
)

correction_log

# Exercise 14: Clinic data-quality report.
clinic_data <- read.csv(
  "data/clinic_visits.csv",
  stringsAsFactors = FALSE
)

create_quality_report <- function(
  data
) {
  data.frame(
    variable = names(data),
    class = vapply(
      data,
      function(x) {
        paste(
          class(x),
          collapse = ", "
        )
      },
      FUN.VALUE = character(1)
    ),
    missing = vapply(
      data,
      function(x) {
        sum(is.na(x))
      },
      FUN.VALUE = integer(1)
    ),
    missing_percentage = vapply(
      data,
      function(x) {
        mean(is.na(x)) * 100
      },
      FUN.VALUE = numeric(1)
    ),
    unique_values = vapply(
      data,
      function(x) {
        length(unique(x))
      },
      FUN.VALUE = integer(1)
    ),
    stringsAsFactors = FALSE
  )
}

clinic_quality_report <- create_quality_report(
  clinic_data
)

clinic_quality_report

clinic_id_missing <- sum(
  is.na(clinic_data$patient_id) |
    clinic_data$patient_id == ""
)

clinic_id_duplicates <- sum(
  duplicated(
    clinic_data$patient_id
  )
)

age_valid <- clinic_data$age >= 0 &
  clinic_data$age <= 120

waiting_valid <- clinic_data$waiting_time >= 0

consultation_valid <-
  clinic_data$consultation_duration >= 0

clinic_validation_summary <- data.frame(
  rule = c(
    "Missing patient IDs",
    "Duplicate patient IDs",
    "Invalid age",
    "Negative waiting time",
    "Negative consultation duration"
  ),
  count = c(
    clinic_id_missing,
    clinic_id_duplicates,
    sum(!age_valid, na.rm = TRUE),
    sum(!waiting_valid, na.rm = TRUE),
    sum(!consultation_valid, na.rm = TRUE)
  ),
  stringsAsFactors = FALSE
)

clinic_validation_summary

# Outlier screening.
final_scores <- raw_student_data$final_score

q1 <- quantile(
  final_scores,
  0.25,
  na.rm = TRUE
)

q3 <- quantile(
  final_scores,
  0.75,
  na.rm = TRUE
)

iqr_value <- q3 - q1

lower_fence <- q1 - 1.5 * iqr_value
upper_fence <- q3 + 1.5 * iqr_value

outlier_status <- final_scores < lower_fence |
  final_scores > upper_fence

raw_student_data[
  outlier_status,
]

# Decision on whether analysis should proceed.
analysis_can_proceed <-
  critical_issue_count == 0L &&
  all(
    required_columns <- c(
      "student_id",
      "programme",
      "final_score"
    ) %in%
      names(raw_student_data)
  )

analysis_can_proceed

# Practical assessment report.
quality_assessment <- list(
  structural_profile =
    structural_profile,
  missingness =
    missingness_report,
  issue_summary =
    issue_summary,
  correction_log =
    correction_log,
  analysis_can_proceed =
    analysis_can_proceed
)

quality_assessment

# ==============================================================================
# End of Lesson 17 Solutions
# ==============================================================================
