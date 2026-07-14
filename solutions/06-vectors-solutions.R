# ==============================================================================
# Learning R
# Part I: R Foundations
# Lesson 6: Vectors and Vectorised Operations
# Suggested Solutions
# ==============================================================================


# ==============================================================================
# Exercise 1: Numeric vector
# ==============================================================================

scores <- c(
  65,
  72,
  81,
  59,
  90
)

scores
length(scores)
typeof(scores)
class(scores)


# ==============================================================================
# Exercise 2: Sequences
# ==============================================================================

sequence_colon <- 1:10

sequence_interval <- seq(
  from = 0,
  to = 100,
  by = 10
)

sequence_length <- seq(
  from = 0,
  to = 1,
  length.out = 6
)

repeated_values <- rep(
  c("A", "B"),
  each = 3
)

sequence_colon
sequence_interval
sequence_length
repeated_values


# ==============================================================================
# Exercise 3: Named vector
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
# Exercise 4: Element-wise arithmetic
# ==============================================================================

coursework <- c(
  30,
  28,
  35,
  24
)

examination <- c(
  48,
  42,
  51,
  39
)

final_scores <- coursework + examination

final_scores

remaining_marks <- 100 - final_scores
remaining_marks


# ==============================================================================
# Exercise 5: Exact and inexact recycling
# ==============================================================================

exact_recycling <- c(
  10,
  20,
  30,
  40
) + c(
  1,
  2
)

exact_recycling

# The shorter vector is recycled exactly because 4 is a multiple of 2.

# Inexact recycling would produce a warning:
#
# c(10, 20, 30, 40, 50) + c(1, 2)
#
# The longer length 5 is not an exact multiple of the shorter length 2.


# ==============================================================================
# Exercise 6: Logical counts and percentages
# ==============================================================================

pass_mark <- 50

pass_status <- final_scores >= pass_mark

number_passed <- sum(
  pass_status
)

pass_percentage <- mean(
  pass_status
) * 100

pass_status
number_passed
pass_percentage


# ==============================================================================
# Exercise 7: Sort, order and rank
# ==============================================================================

sorted_scores <- sort(
  scores
)

descending_scores <- sort(
  scores,
  decreasing = TRUE
)

ordering_indices <- order(
  scores
)

score_ranks <- rank(
  scores
)

sorted_scores
descending_scores
ordering_indices
score_ranks


# ==============================================================================
# Exercise 8: Duplicates and unique values
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

number_of_duplicate_positions <- sum(
  duplicate_status
)

unique_student_ids <- unique(
  student_ids
)

duplicate_status
number_of_duplicate_positions
unique_student_ids
length(unique_student_ids)


# ==============================================================================
# Exercise 9: Set operations
# ==============================================================================

group_a <- c(
  "Ama",
  "Kojo",
  "Esi",
  "Yaw"
)

group_b <- c(
  "Esi",
  "Yaw",
  "Akua",
  "Kofi"
)

union(
  group_a,
  group_b
)

intersect(
  group_a,
  group_b
)

setdiff(
  group_a,
  group_b
)

setdiff(
  group_b,
  group_a
)

setequal(
  c("A", "B", "C"),
  c("C", "B", "A")
)


# ==============================================================================
# Exercise 10: Cumulative sums and differences
# ==============================================================================

monthly_sales <- c(
  100,
  120,
  90,
  150
)

cumulative_sales <- cumsum(
  monthly_sales
)

monthly_changes <- diff(
  monthly_sales
)

cumulative_sales
monthly_changes


# ==============================================================================
# Exercise 11: Sampling
# ==============================================================================

set.seed(20260713)

sample_without_replacement <- sample(
  x = scores,
  size = 3,
  replace = FALSE
)

sample_with_replacement <- sample(
  x = scores,
  size = 8,
  replace = TRUE
)

sample_without_replacement
sample_with_replacement


# ==============================================================================
# Exercise 12: Rainfall vector
# ==============================================================================

rainfall_data <- read.csv(
  "data/rainfall_records.csv",
  stringsAsFactors = FALSE
)

rainfall_mm <- rainfall_data$rainfall_mm

str(rainfall_mm)
length(rainfall_mm)
sum(is.na(rainfall_mm))

rainfall_summary <- c(
  Observed = sum(
    !is.na(rainfall_mm)
  ),
  Missing = sum(
    is.na(rainfall_mm)
  ),
  Mean = mean(
    rainfall_mm,
    na.rm = TRUE
  ),
  Median = median(
    rainfall_mm,
    na.rm = TRUE
  ),
  Minimum = min(
    rainfall_mm,
    na.rm = TRUE
  ),
  Maximum = max(
    rainfall_mm,
    na.rm = TRUE
  )
)

rainfall_summary


# ==============================================================================
# Practical assessment: Complete vector workflow
# ==============================================================================

# Vector creation.
assessment_scores <- c(
  45,
  58,
  72,
  84,
  91,
  72
)

# Names.
names(assessment_scores) <- paste0(
  "Student_",
  seq_along(assessment_scores)
)

assessment_scores

# Vectorised arithmetic.
moderated_scores <- assessment_scores + 2
moderated_scores

# Comparison.
pass_status <- moderated_scores >= 50
distinction_status <- moderated_scores >= 80

pass_status
distinction_status

# Logical summaries.
sum(pass_status)
mean(pass_status) * 100

sum(distinction_status)
mean(distinction_status) * 100

# Recycling.
adjustment_pattern <- c(
  1,
  2
)

recycled_adjustment <-
  assessment_scores +
  adjustment_pattern

recycled_adjustment

# Missing values.
scores_with_missing <- c(
  assessment_scores,
  NA
)

sum(is.na(scores_with_missing))

mean(
  scores_with_missing,
  na.rm = TRUE
)

# Sorting.
sort(assessment_scores)
order(assessment_scores)
rank(assessment_scores)

# Duplicates.
duplicated(assessment_scores)
unique(assessment_scores)

# Set operations.
high_scores <- assessment_scores[
  assessment_scores >= 70
]

distinction_scores <- assessment_scores[
  assessment_scores >= 80
]

union(
  high_scores,
  distinction_scores
)

intersect(
  high_scores,
  distinction_scores
)

setdiff(
  high_scores,
  distinction_scores
)

# Cumulative functions.
cumsum(assessment_scores)
cummin(assessment_scores)
cummax(assessment_scores)

# Sampling.
set.seed(20260713)

sample(
  assessment_scores,
  size = 3,
  replace = FALSE
)

# Validation.
scores_are_numeric <- is.numeric(
  assessment_scores
)

scores_have_valid_length <-
  length(assessment_scores) > 0

scores_have_no_missing_values <-
  !anyNA(assessment_scores)

scores_are_in_range <- all(
  assessment_scores >= 0 &
    assessment_scores <= 100
)

scores_are_numeric
scores_have_valid_length
scores_have_no_missing_values
scores_are_in_range


# ==============================================================================
# End of Lesson 6 Solutions
# ==============================================================================
