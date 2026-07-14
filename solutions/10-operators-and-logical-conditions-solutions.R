# ==============================================================================
# Learning R
# Part II: Accessing and Controlling Data
# Lesson 10: Operators, Comparisons and Logical Conditions
# Suggested Solutions
# ==============================================================================


# ==============================================================================
# Exercise 1: Arithmetic operators
# ==============================================================================

10 + 3
10 - 3
10 * 3
10 / 3
10 ^ 3
10 %/% 3
10 %% 3


# ==============================================================================
# Exercise 2: Relational comparisons
# ==============================================================================

scores <- c(
  45,
  58,
  72,
  84,
  91
)

scores > 50
scores < 70
scores >= 72
scores <= 84
scores == 72
scores != 72


# ==============================================================================
# Exercise 3: Logical AND
# ==============================================================================

attendance <- c(
  0.80,
  0.92,
  0.88,
  0.95,
  0.70
)

eligible <-
  (scores >= 70) &
  (attendance >= 0.85)

eligible


# ==============================================================================
# Exercise 4: Logical OR
# ==============================================================================

requires_support <-
  (scores < 50) |
  (attendance < 0.75)

requires_support


# ==============================================================================
# Exercise 5: Logical negation
# ==============================================================================

!eligible


# ==============================================================================
# Exercise 6: Vectorised and short-circuit operators
# ==============================================================================

vectorised_and <- c(
  TRUE,
  FALSE,
  TRUE
) & c(
  TRUE,
  TRUE,
  FALSE
)

vectorised_or <- c(
  TRUE,
  FALSE,
  TRUE
) | c(
  FALSE,
  FALSE,
  TRUE
)

short_circuit_and <- c(
  TRUE,
  FALSE
) && c(
  TRUE,
  TRUE
)

short_circuit_or <- c(
  FALSE,
  TRUE
) || c(
  TRUE,
  FALSE
)

vectorised_and
vectorised_or
short_circuit_and
short_circuit_or

# & and | operate element by element.
# && and || use only the first element and are intended for single decisions.


# ==============================================================================
# Exercise 7: Membership
# ==============================================================================

programmes <- c(
  "Statistics",
  "Hospitality",
  "Agriculture",
  "Procurement"
)

selected_programme <- programmes %in%
  c(
    "Statistics",
    "Procurement"
  )

not_selected_programme <- !(
  programmes %in%
    c(
      "Statistics",
      "Procurement"
    )
)

selected_programme
not_selected_programme


# ==============================================================================
# Exercise 8: Operator precedence
# ==============================================================================

without_parentheses <- 10 + 2 * 5
with_parentheses <- (10 + 2) * 5

without_parentheses
with_parentheses

logical_without_parentheses <-
  TRUE |
  FALSE &
  FALSE

logical_with_parentheses <-
  TRUE |
  (
    FALSE &
    FALSE
  )

logical_without_parentheses
logical_with_parentheses


# ==============================================================================
# Exercise 9: Approximate equality
# ==============================================================================

floating_result <- 0.1 + 0.2

floating_result
floating_result == 0.3

all.equal(
  floating_result,
  0.3
)

abs(
  floating_result - 0.3
) < 1e-8


# ==============================================================================
# Exercise 10: Three-valued logic
# ==============================================================================

TRUE & NA
FALSE & NA
TRUE | NA
FALSE | NA

# TRUE & NA remains unknown.
# FALSE & NA is definitely FALSE.
# TRUE | NA is definitely TRUE.
# FALSE | NA remains unknown.


# ==============================================================================
# Exercise 11: Multi-part validation condition
# ==============================================================================

validation_scores <- c(
  65,
  72,
  81,
  59
)

scores_are_numeric <- is.numeric(
  validation_scores
)

scores_have_no_missing_values <-
  !anyNA(
    validation_scores
  )

scores_are_in_range <- all(
  validation_scores >= 0 &
    validation_scores <= 100
)

scores_are_valid <-
  scores_are_numeric &&
  scores_have_no_missing_values &&
  scores_are_in_range

scores_are_numeric
scores_have_no_missing_values
scores_are_in_range
scores_are_valid


# ==============================================================================
# Exercise 12: Translate verbal rules
# ==============================================================================

# Rule 1:
# A student progresses when score >= 50 and attendance >= 75%.

final_score <- 64
attendance_rate <- 0.82

progresses <-
  final_score >= 50 &&
  attendance_rate >= 0.75

progresses

# Rule 2:
# Procurement requires review if value > 50000 or delay > 7 days.

contract_value <- 62000
delay_days <- 4

requires_review <-
  contract_value > 50000 ||
  delay_days > 7

requires_review

# Rule 3:
# A clinic record is urgent when severity is High or waiting time > 60.

severity <- "High"
waiting_time <- 40

urgent <-
  severity == "High" ||
  waiting_time > 60

urgent


# ==============================================================================
# Exercise 13: Vectorised rule on imported data
# ==============================================================================

student_data <- read.csv(
  "data/student_scores.csv",
  stringsAsFactors = FALSE
)

selected_programmes <-
  student_data$programme %in%
  c(
    "Statistics",
    "Procurement"
  )

distinction_status <-
  student_data$final_score >= 80

selected_distinctions <-
  selected_programmes &
  distinction_status

student_data[
  selected_distinctions,
  c(
    "student_id",
    "programme",
    "final_score"
  )
]


# ==============================================================================
# Exercise 14: Verify De Morgan's laws
# ==============================================================================

a <- c(
  TRUE,
  TRUE,
  FALSE,
  FALSE
)

b <- c(
  TRUE,
  FALSE,
  TRUE,
  FALSE
)

left_one <- !(a & b)
right_one <- (!a) | (!b)

left_two <- !(a | b)
right_two <- (!a) & (!b)

left_one
right_one

left_two
right_two

identical(
  left_one,
  right_one
)

identical(
  left_two,
  right_two
)


# ==============================================================================
# Practical assessment: Complete suggested workflow
# ==============================================================================

# Arithmetic and relational operators.
x <- 18
y <- 5

x + y
x - y
x * y
x / y
x ^ 2
x %/% y
x %% y

x > y
x < y
x >= 18
x <= 17
x == 18
x != y

# Assignment versus comparison.
score <- 75
score == 75

# Logical operators.
condition_a <- TRUE
condition_b <- FALSE

condition_a & condition_b
condition_a | condition_b
!condition_a
condition_a && condition_b
condition_a || condition_b

# Membership.
programme <- "Statistics"

programme %in%
  c(
    "Statistics",
    "Procurement"
  )

# Precedence.
10 + 4 * 3
(10 + 4) * 3

# Floating-point comparison.
direct_decimal_comparison <-
  0.1 + 0.2 == 0.3

approximate_decimal_comparison <-
  isTRUE(
    all.equal(
      0.1 + 0.2,
      0.3
    )
  )

direct_decimal_comparison
approximate_decimal_comparison

# Missing logical values.
logical_with_missing <- c(
  TRUE,
  FALSE,
  NA
)

sum(
  logical_with_missing,
  na.rm = TRUE
)

# Combined validation.
assessment_scores <- c(
  65,
  72,
  81
)

assessment_validation <-
  is.numeric(
    assessment_scores
  ) &&
  !anyNA(
    assessment_scores
  ) &&
  all(
    assessment_scores >= 0 &
      assessment_scores <= 100
  )

assessment_validation

# Multi-condition rule.
assessment_students <- read.csv(
  "data/student_scores.csv",
  stringsAsFactors = FALSE
)

rule_status <-
  assessment_students$final_score >= 70 &
  assessment_students$programme %in%
  c(
    "Statistics",
    "Procurement"
  )

assessment_students[
  rule_status,
  c(
    "student_id",
    "programme",
    "final_score"
  )
]


# ==============================================================================
# End of Lesson 10 Solutions
# ==============================================================================
